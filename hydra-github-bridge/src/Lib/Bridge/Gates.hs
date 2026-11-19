-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Hold GitHub Actions jobs until Hydra is done with their commit.
--
-- A workflow job that declares an environment protected by this GitHub
-- App's custom deployment protection rule, e.g.
--
-- > jobs:
-- >   after-hydra:
-- >     environment: hydra
--
-- waits (without a runner) until the App approves or rejects it.  GitHub
-- tells the App with a @deployment_protection_rule@ webhook; the bridge
-- records the waiting run in @github_gate@, and 'gateWorker' approves it
-- once the aggregate @hydra@ check-run of the commit (see
-- "Lib.Bridge.HydraCheck") has succeeded, or rejects it once it has failed.
--
-- Missed webhooks are recovered by the periodic sync, which lists the
-- repository's waiting runs ('discoverWaitingGates').
module Lib.Bridge.Gates
  ( -- * Webhook payload
    GateRequest (..),
    parseGateRequest,
    gateHeadSha,
    runIdFromCallbackUrl,
    verifySignature256,

    -- * Decisions
    GateFacts (..),
    GateDecision (..),
    decideGate,

    -- * Waiting runs (sync)
    WaitingRun (..),
    parseWaitingRuns,
    parsePendingEnvironments,
    hydraEnvironment,
    discoverWaitingGates,

    -- * Storage and workers
    gateSchema,
    recordGate,
    gateHookApp,
    GateWorkerConfig (..),
    gateWorker,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (forM, forM_, forever, void, when)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Aeson (Value, eitherDecode, parseJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bits (xor, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query, query_)
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue ((:=)),
    StdMethod (GET, POST),
  )
import GitHub.REST.PageLinks (PageLinks)
import Lib.Bridge.HydraCheck (HydraOutcome (..), HydraState (..), lookupHydraState)
import Lib.Bridge.PublicUrl (hydraDetailsUrl)
import Lib.Bridge.Source (escapeHydraName)
import Lib.Connection (guardedGetNotification)
import Lib.Exception (tryNonAsync)
import Lib.GitHub.Client (GitHubRestConfig, TokenLease, gitHubApiVersion, gitHubRestConfig, queryGitHubRestPage)
import Lib.GitHub.Client qualified as GHClient
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status400, status401, status404)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (Application, requestHeaders, responseLBS, strictRequestBody)

-- | The environment name the sync looks for when recovering missed
-- webhooks.  The webhook itself is honoured for any environment, since
-- GitHub only sends it for environments with this App's rule enabled.
hydraEnvironment :: Text
hydraEnvironment = "hydra"

------------------------------------------------------------------------
-- Webhook payload
------------------------------------------------------------------------

-- | A run waiting for this App to review one of its environments.
data GateRequest = GateRequest
  { owner :: Text,
    repo :: Text,
    environment :: Text,
    -- | The event that started the workflow run (@push@, @pull_request@, ...).
    event :: Text,
    -- | The commit Hydra builds for this run (see 'gateHeadSha').
    headSha :: Text,
    runId :: Int,
    -- | The pull request, for @pull_request@ runs.
    prNumber :: Maybe Int
  }
  deriving (Eq, Show)

-- | Parse a @deployment_protection_rule@ webhook payload.
parseGateRequest :: Value -> Either String GateRequest
parseGateRequest = parseEither $ withObject "deployment_protection_rule" $ \o -> do
  environment <- o .: "environment"
  event <- o .: "event"
  sha <- o .: "sha"
  callbackUrl <- o .: "deployment_callback_url"
  repository <- o .: "repository"
  fullName <- withObject "repository" (.: "full_name") repository
  prs <- o .:? "pull_requests" .!= []
  pullRequests <- mapM pullRequest prs
  runId <- maybe (fail "deployment_callback_url names no workflow run") pure (runIdFromCallbackUrl callbackUrl)
  (owner, repo) <- case Text.splitOn "/" fullName of
    [owner, repo] -> pure (owner, repo)
    _ -> fail ("repository.full_name is not owner/repo: " <> Text.unpack fullName)
  pure
    GateRequest
      { owner = owner,
        repo = repo,
        environment = environment,
        event = event,
        headSha = gateHeadSha event sha pullRequests,
        runId = runId,
        prNumber = if isPullRequestEvent event then fst <$> listToMaybe pullRequests else Nothing
      }
  where
    pullRequest :: Value -> Parser (Int, Maybe Text)
    pullRequest = withObject "pull_request" $ \pr -> do
      number <- pr .: "number"
      headSha <- pr .:? "head" >>= maybe (pure Nothing) (withObject "head" (.:? "sha"))
      pure (number, headSha)

isPullRequestEvent :: Text -> Bool
isPullRequestEvent event = event `elem` ["pull_request", "pull_request_target"]

-- | The commit Hydra builds for a run.  For a pull request run the
-- payload's @sha@ is GitHub's merge commit, which Hydra never sees: the PR
-- jobset builds the PR head.  Every other run (push, tag, merge queue, ...)
-- is for the commit Hydra builds.
gateHeadSha :: Text -> Text -> [(Int, Maybe Text)] -> Text
gateHeadSha event sha pullRequests
  | isPullRequestEvent event, ((_, Just prHead) : _) <- pullRequests = prHead
  | otherwise = sha

-- | The workflow run id in a deployment callback URL,
-- @.../repos/OWNER/REPO/actions/runs/RUN_ID/deployment_protection_rule@.
runIdFromCallbackUrl :: Text -> Maybe Int
runIdFromCallbackUrl url = go (Text.splitOn "/" url)
  where
    go ("runs" : n : _) | not (Text.null n), Text.all isDigit n = Just (read (Text.unpack n))
    go (_ : rest) = go rest
    go [] = Nothing

-- | Check GitHub's @X-Hub-Signature-256@ header (@sha256=<hex HMAC>@) of a
-- webhook body against the webhook secret.
verifySignature256 :: ByteString -> ByteString -> Maybe ByteString -> Bool
verifySignature256 secret body header =
  case BS.stripPrefix "sha256=" =<< header of
    Nothing -> False
    Just given -> constantTimeEq (BS8.map toLowerAscii given) expected
  where
    expected = BS8.pack (show (hmacGetDigest (hmac secret body :: HMAC SHA256)))
    toLowerAscii c = if c >= 'A' && c <= 'F' then toEnum (fromEnum c + 32) else c
    constantTimeEq a b =
      BS.length a == BS.length b && foldl' (.|.) 0 (BS.zipWith xor a b) == 0

------------------------------------------------------------------------
-- Decisions
------------------------------------------------------------------------

-- | What is known about a waiting run when deciding what to do with it.
data GateFacts = GateFacts
  { headSha :: Text,
    -- | Time since the run started waiting.
    age :: NominalDiffTime,
    -- | The last status report sent for it, if any.
    reported :: Maybe Text,
    -- | The commit's aggregate Hydra state, if Hydra has started on it.
    state :: Maybe HydraState,
    -- | Hydra will not build the commit: its pull request jobset has moved
    -- on to another commit.
    superseded :: Bool,
    -- | Base URL of the Hydra web UI, for links.
    publicBase :: Text
  }
  deriving (Eq, Show)

data GateDecision
  = GateWait
  | -- | Report progress without deciding (sent once per distinct message).
    GateReport Text
  | GateApprove Text
  | GateReject Text
  deriving (Eq, Show)

decideGate :: GateFacts -> GateDecision
decideGate f = case f.state of
  Just st -> case st.outcome of
    HydraSucceeded -> GateApprove ("Hydra succeeded: " <> st.title <> evalLink st)
    HydraFailed -> GateReject ("Hydra failed: " <> st.title <> evalLink st)
    -- A stable message (it does not follow the build counts), so the run
    -- gets at most a couple of reports: GitHub keeps only ten.
    HydraPending -> report $ case st.evalId of
      Just evalId -> "Waiting for Hydra to finish evaluation " <> tshow evalId <> evalLink st
      Nothing -> waitingForEval
  Nothing
    | f.superseded && f.age > 30 * 60 ->
        GateReject ("Hydra will not build " <> f.headSha <> ": the pull request's jobset has moved on to another commit.")
    | f.age > 24 * 60 * 60 ->
        GateReject ("Hydra has not started on " <> f.headSha <> " in 24 hours.")
    | otherwise -> report waitingForEval
  where
    report msg = if f.reported == Just msg then GateWait else GateReport msg
    waitingForEval = "Waiting for Hydra to evaluate " <> f.headSha <> "."
    evalLink :: HydraState -> Text
    evalLink st = maybe "" (\evalId -> " " <> hydraDetailsUrl f.publicBase ("/eval/" <> tshow evalId)) st.evalId

------------------------------------------------------------------------
-- Waiting runs (sync)
------------------------------------------------------------------------

-- | A workflow run in the @waiting@ state, from
-- @GET /repos/{owner}/{repo}/actions/runs?status=waiting@.
data WaitingRun = WaitingRun
  { runId :: Int,
    event :: Text,
    -- | For a pull request run this is already the PR head.
    headSha :: Text,
    prNumber :: Maybe Int
  }
  deriving (Eq, Show)

parseWaitingRuns :: Value -> Either String [WaitingRun]
parseWaitingRuns = parseEither $ withObject "workflow runs" $ \o -> do
  runs <- o .: "workflow_runs"
  forM runs $ withObject "workflow run" $ \r -> do
    runId <- r .: "id"
    event <- r .: "event"
    headSha <- r .: "head_sha"
    prs <- r .:? "pull_requests" .!= []
    numbers <- mapM (withObject "pull_request" (.: "number")) prs
    pure
      WaitingRun
        { runId = runId,
          event = event,
          headSha = headSha,
          prNumber = if isPullRequestEvent event then listToMaybe numbers else Nothing
        }

-- | The environment names of
-- @GET /repos/{owner}/{repo}/actions/runs/{run_id}/pending_deployments@.
parsePendingEnvironments :: Value -> Either String [Text]
parsePendingEnvironments = parseEither $ \v -> do
  entries <- parseJSON v :: Parser [Value]
  forM entries $ withObject "pending deployment" $ \o -> do
    env <- o .: "environment"
    withObject "environment" (.: "name") env

-- | Record a gate for every run of @owner/repo@ waiting on the @hydra@
-- environment that the bridge does not know about, e.g. because its webhook
-- was missed.  A gate resolved more than ten minutes ago is reopened: the
-- run was re-run (it keeps its id), or the review did not reach GitHub.
-- Returns the number of gates recorded.
discoverWaitingGates :: GitHubRestConfig -> Connection -> Text -> Text -> IO Int
discoverWaitingGates ghCfg conn owner repo = do
  (runsJson, _) <- queryGitHubRestPage ghCfg (get ("/repos/:owner/:repo/actions/runs?status=waiting&per_page=100") []) :: IO (Value, PageLinks)
  runs <- either (\e -> fail ("unexpected workflow runs listing: " <> e)) pure (parseWaitingRuns runsJson)
  recorded <- forM runs $ \run -> do
    (pendingJson, _) <-
      queryGitHubRestPage ghCfg (get "/repos/:owner/:repo/actions/runs/:runId/pending_deployments" ["runId" := run.runId]) :: IO (Value, PageLinks)
    envs <- either (\e -> fail ("unexpected pending deployments: " <> e)) pure (parsePendingEnvironments pendingJson)
    if hydraEnvironment `notElem` envs
      then pure 0
      else
        execute
          conn
          "INSERT INTO github_gate (owner, repo, head_sha, run_id, environment, event, pr_number) \
          \VALUES (?, ?, ?, ?, ?, ?, ?) \
          \ON CONFLICT (owner, repo, run_id, environment) DO UPDATE \
          \SET head_sha = excluded.head_sha, event = excluded.event, pr_number = excluded.pr_number, \
          \    created = NOW(), reported = NULL, resolved_at = NULL, outcome = NULL, tries = 0 \
          \WHERE github_gate.resolved_at < NOW() - interval '10 minutes'"
          (owner, repo, run.headSha, run.runId, hydraEnvironment, run.event, run.prNumber)
  let total = fromIntegral (sum recorded)
  when (total > 0) $ void $ execute_ conn "NOTIFY github_gate"
  pure total
  where
    get path extra =
      GHEndpoint
        { method = GET,
          endpoint = path,
          endpointVals = ["owner" := owner, "repo" := repo] ++ extra,
          ghData = []
        }

------------------------------------------------------------------------
-- Storage
------------------------------------------------------------------------

-- | One row per (run, environment) waiting for review.  @reported@ is the
-- last status report sent, so each is sent once; @tries@ counts failed
-- reviews.  Run ids do not fit in 32 bits.
gateSchema :: [String]
gateSchema =
  [ "CREATE TABLE IF NOT EXISTS github_gate ( \
    \  id SERIAL PRIMARY KEY, \
    \  owner TEXT NOT NULL, \
    \  repo TEXT NOT NULL, \
    \  head_sha TEXT NOT NULL, \
    \  run_id BIGINT NOT NULL, \
    \  environment TEXT NOT NULL, \
    \  event TEXT NOT NULL, \
    \  pr_number INTEGER, \
    \  created TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \  reported TEXT, \
    \  resolved_at TIMESTAMPTZ, \
    \  outcome TEXT, \
    \  tries INTEGER NOT NULL DEFAULT 0, \
    \  UNIQUE (owner, repo, run_id, environment) \
    \)",
    "CREATE INDEX IF NOT EXISTS idx_github_gate_unresolved \
    \ON github_gate (id) WHERE resolved_at IS NULL"
  ]

-- | Record a run waiting for review and wake the gate worker.  A run that
-- is re-run keeps its id, so a gate that was already resolved is reopened.
recordGate :: Connection -> GateRequest -> IO ()
recordGate conn g = do
  _ <-
    execute
      conn
      "INSERT INTO github_gate (owner, repo, head_sha, run_id, environment, event, pr_number) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (owner, repo, run_id, environment) DO UPDATE \
      \SET head_sha = excluded.head_sha, event = excluded.event, pr_number = excluded.pr_number, \
      \    created = NOW(), reported = NULL, resolved_at = NULL, outcome = NULL, tries = 0"
      (g.owner, g.repo, g.headSha, g.runId, g.environment, g.event, g.prNumber)
  void $ execute_ conn "NOTIFY github_gate"

------------------------------------------------------------------------
-- Webhook
------------------------------------------------------------------------

-- | Handles the @deployment_protection_rule@ webhook, which
-- servant-github-webhook has no event type for, so the request is checked
-- here: GitHub signs it with the same secret as every other webhook.  Any
-- other unknown event gets the 404 servant-github-webhook used to give.
gateHookApp :: Set Text -> ByteString -> Connection -> Application
gateHookApp orgs secret conn req respond =
  case lookup "X-GitHub-Event" (requestHeaders req) of
    Nothing -> respond $ responseLBS status401 [] "missing X-GitHub-Event"
    Just "deployment_protection_rule" -> do
      body <- strictRequestBody req
      if not (verifySignature256 secret (LBS.toStrict body) (lookup "X-Hub-Signature-256" (requestHeaders req)))
        then respond $ responseLBS status401 [] "bad signature"
        else case eitherDecode body >>= parseGateRequest of
          Left err -> do
            putStrLn $ "Malformed deployment_protection_rule payload: " <> err
            respond $ responseLBS status400 [] (LBS.fromStrict (BS8.pack err))
          Right g
            | not (Set.member g.owner orgs) -> do
                putStrLn $ "Ignoring deployment_protection_rule from non-allowed org: " <> Text.unpack g.owner
                respond $ responseLBS status200 [] "ignored"
            | otherwise -> do
                putStrLn $
                  "Gate requested: "
                    <> Text.unpack (g.owner <> "/" <> g.repo)
                    <> " run "
                    <> show g.runId
                    <> " environment "
                    <> Text.unpack g.environment
                    <> " for "
                    <> Text.unpack g.headSha
                recordGate conn g
                respond $ responseLBS status200 [] "recorded"
    Just event -> respond $ responseLBS status404 [] ("unsupported event: " <> LBS.fromStrict event)

------------------------------------------------------------------------
-- Worker
------------------------------------------------------------------------

data GateWorkerConfig = GateWorkerConfig
  { publicBase :: Text,
    endpoint :: Text,
    userAgent :: ByteString,
    getTokens :: IO [(String, TokenLease)],
    -- | Seconds between passes when nothing wakes the worker.
    interval :: Int
  }

-- | Review the waiting runs whenever something may have changed (a gate
-- was recorded, or a commit's Hydra state finished: @NOTIFY github_gate@),
-- and at least every 'interval' seconds, which also times gates out.
gateWorker :: GateWorkerConfig -> Connection -> IO ()
gateWorker cfg conn = do
  _ <- execute_ conn "LISTEN github_gate"
  forever $ do
    processGates cfg conn
      `catch` \e -> case fromException e of
        Just (async :: SomeAsyncException) -> throwIO async
        Nothing -> do
          putStrLn $ "ERROR: gate worker: " ++ displayException (e :: SomeException)
          threadDelay 5_000_000
    void $ guardedGetNotification conn (cfg.interval * 1_000_000)

processGates :: GateWorkerConfig -> Connection -> IO ()
processGates cfg conn = do
  gates <-
    query_
      conn
      "SELECT id, owner, repo, head_sha, run_id, environment, pr_number, \
      \       EXTRACT(EPOCH FROM NOW() - created)::float8, reported \
      \FROM github_gate WHERE resolved_at IS NULL ORDER BY id" ::
      IO [(Int, Text, Text, Text, Int, Text, Maybe Int, Double, Maybe Text)]
  forM_ gates $ \(gateId, owner, repo, sha, runId, env, prNumber, ageSecs, reported) -> do
    st <- lookupHydraState conn owner repo sha
    superseded <- case (st, prNumber) of
      (Nothing, Just n) -> prJobsetMovedOn conn owner repo n sha
      _ -> pure False
    let decision =
          decideGate
            GateFacts
              { headSha = sha,
                age = realToFrac ageSecs,
                reported = reported,
                state = st,
                superseded = superseded,
                publicBase = cfg.publicBase
              }
        review mState msg onDone = do
          res <- reviewRun cfg owner repo runId env mState msg
          case res of
            Right () -> onDone
            Left e -> reviewFailed gateId owner repo runId e
    case decision of
      GateWait -> pure ()
      -- A report is only informational: whether or not GitHub took it,
      -- it is not retried, and a failure never resolves the gate.
      GateReport msg -> do
        res <- reviewRun cfg owner repo runId env Nothing msg
        case res of
          Right () -> pure ()
          Left e -> putStrLn $ "Gate status report failed: " <> Text.unpack (owner <> "/" <> repo) <> " run " <> show runId <> ": " <> displayException e
        void $ execute conn "UPDATE github_gate SET reported = ? WHERE id = ?" (msg, gateId)
      GateApprove msg ->
        review (Just "approved") msg $ resolve gateId owner repo runId "approved" msg
      GateReject msg ->
        review (Just "rejected") msg $ resolve gateId owner repo runId "rejected" msg
  where
    resolve :: Int -> Text -> Text -> Int -> Text -> Text -> IO ()
    resolve gateId owner repo runId outcome msg = do
      putStrLn $ "Gate " <> Text.unpack outcome <> ": " <> Text.unpack (owner <> "/" <> repo) <> " run " <> show runId <> ": " <> Text.unpack msg
      void $ execute conn "UPDATE github_gate SET resolved_at = NOW(), outcome = ? WHERE id = ?" (outcome, gateId)

    -- The run is gone or no longer waiting (cancelled, re-run, reviewed
    -- elsewhere): nothing to retry.  Anything else (rate limits, missing
    -- permissions, GitHub errors) is retried on the next pass.
    reviewFailed :: Int -> Text -> Text -> Int -> SomeException -> IO ()
    reviewFailed gateId owner repo runId e
      | Just code <- httpStatus e,
        code `elem` [404, 410, 422] = do
          putStrLn $ "Gate gone (HTTP " <> show code <> "): " <> Text.unpack (owner <> "/" <> repo) <> " run " <> show runId
          void $ execute conn "UPDATE github_gate SET resolved_at = NOW(), outcome = ? WHERE id = ?" ("gone: HTTP " <> tshow code, gateId)
      | otherwise = do
          putStrLn $ "Gate review failed: " <> Text.unpack (owner <> "/" <> repo) <> " run " <> show runId <> ": " <> displayException e
          void $ execute conn "UPDATE github_gate SET tries = tries + 1 WHERE id = ?" (Only gateId)

    httpStatus :: SomeException -> Maybe Int
    httpStatus e = case fromException e of
      Just (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) -> Just (statusCode (HTTP.responseStatus resp))
      _ -> Nothing

-- | Hydra will not build @sha@ for pull request @n@ when the PR's jobset
-- now points at another commit.
prJobsetMovedOn :: Connection -> Text -> Text -> Int -> Text -> IO Bool
prJobsetMovedOn conn owner repo n sha = do
  rows <-
    query
      conn
      "SELECT flake FROM jobsets WHERE project = ? AND name = ?"
      (escapeHydraName (owner <> "/" <> repo), "pullrequest-" <> tshow n)
  pure $ case rows of
    [Only (Just flake)] -> not (sha `Text.isInfixOf` flake)
    _ -> False

-- | Approve, reject (@state@) or just report on (no @state@) a run waiting
-- for this App's protection rule.
reviewRun :: GateWorkerConfig -> Text -> Text -> Int -> Text -> Maybe Text -> Text -> IO (Either SomeException ())
reviewRun cfg owner repo runId env mState comment = tryNonAsync $ do
  tokens <- cfg.getTokens
  lease <- case find ((== Text.unpack owner) . fst) tokens of
    Just (_, l) -> pure l
    Nothing -> fail ("No GitHub token for " <> Text.unpack owner)
  ghCfg <-
    gitHubRestConfig
      GitHubSettings
        { token = Just (GHClient.token lease),
          userAgent = cfg.userAgent,
          apiVersion = gitHubApiVersion
        }
      cfg.endpoint
  (_ :: Value, _) <-
    queryGitHubRestPage
      ghCfg
      GHEndpoint
        { method = POST,
          endpoint = "/repos/:owner/:repo/actions/runs/:runId/deployment_protection_rule",
          endpointVals = ["owner" := owner, "repo" := repo, "runId" := runId],
          ghData =
            ["environment_name" := env, "comment" := Text.take 1024 comment]
              ++ maybe [] (\s -> ["state" := s]) mState
        }
  pure ()

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
