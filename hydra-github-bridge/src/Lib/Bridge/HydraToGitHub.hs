{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib.Bridge.HydraToGitHub
  ( statusHandler,
    toHydraNotification,
    handleHydraNotification,
    notificationWatcher,
    queueCheckRun,
    statusHandlers,
    parseGitHubFlakeURI,
    -- Exposed for testing.
    EmitDecision (..),
    decideEmit,
    hasRequiredKeyword,
    whenStatusOrJob,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async as Async
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    catchJust,
    displayException,
    fromException,
    throw,
    toException,
  )
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (Error, Success)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Duration (oneSecond)
import Data.Foldable (foldr')
import Data.Functor ((<&>))
import Data.List (intercalate, singleton)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock (addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
  ( getPOSIXTime,
    posixSecondsToUTCTime,
  )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Debug.Trace (traceShowId)
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue ((:=)),
    StdMethod (POST),
    queryGitHub,
  )
import Lib (binarySearch)
import Lib.Bridge.CheckRunGroups (CheckRunGroups, GroupKey (..), GroupTracker, jobGroup, markGroupDirty)
import Lib.Bridge.DB qualified as BridgeDB
import Lib.Bridge.PublicUrl (hydraDetailsUrl, normalizePublicBase)
import Lib.Connection (guardedGetNotification)
import Lib.Data.Duration (humanReadableDuration)
import Lib.Data.List (takeEnd)
import Lib.Data.Text (indentLine)
import Lib.Exception (tryNonAsync)
import Lib.GitHub (CheckRunConclusion)
import Lib.GitHub qualified as GitHub
import Lib.Hydra (BuildStatus)
import Lib.Hydra qualified as Hydra
import Lib.Hydra.DB (readBuildLog)
import Network.HTTP.Client qualified as HTTP
import Text.Regex.TDFA ((=~))

-- Text utils
tshow :: (Show a) => a -> Text
tshow = cs . show

notificationWatcher :: String -> String -> Text -> Bool -> CheckRunGroups -> GroupTracker -> Connection -> IO ()
notificationWatcher host stateDir checkRunPrefix filterJobs groups tracker conn = do
  publicBase <- either (ioError . userError . Text.unpack . ("Invalid Hydra public URL: " <>)) pure (normalizePublicBase $ cs host)
  _ <- execute_ conn "LISTEN eval_started" -- (opaque id, jobset id)
  _ <- execute_ conn "LISTEN eval_added" -- (opaque id, jobset id, eval record id, error_changed)
  _ <- execute_ conn "LISTEN eval_cached" -- (opaque id, jobset id, prev identical eval id)
  _ <- execute_ conn "LISTEN eval_failed" -- (opaque id, jobset id, error_changed)
  _ <- execute_ conn "LISTEN build_queued" -- (build id)
  _ <- execute_ conn "LISTEN cached_build_queued" -- (eval id, build id)
  _ <- execute_ conn "LISTEN build_started" -- (build id)
  _ <- execute_ conn "LISTEN build_finished" -- (build id, dependent build ids...)
  _ <- execute_ conn "LISTEN cached_build_finished" -- (eval id, build id)
  forever $ do
    putStrLn "Waiting for notification..."
    n <- traceShowId <$> getNotification conn
    case toHydraNotification n of
      Nothing -> putStrLn $ "Ignoring unrecognised notification: " ++ show n
      Just note -> do
        statuses <- handleHydraNotification conn publicBase stateDir checkRunPrefix filterJobs groups tracker note
        forM_ statuses (queueCheckRun conn)

-- | Queue a check-run for 'statusHandlers' to send.  The priority column
-- (0 = ci/eval and per-job ci/eval:<job>, 1 = *required, 2 = everything
-- else) is the order in which the queue is drained.
queueCheckRun :: Connection -> GitHub.CheckRun -> IO ()
queueCheckRun conn (GitHub.CheckRun owner repo payload) = do
  Text.putStrLn $ "QUEUEING [" <> owner <> "/" <> repo <> "/" <> payload.headSha <> "] " <> payload.name <> ":" <> Text.pack (show payload.status)
  [Only _id'] <-
    query
      conn
      "with status_upsert as (insert into github_status (owner, repo, headSha, name) values (?, ?, ?, ?) on conflict (owner, repo, headSha, name) do update set name = excluded.name returning id) insert into github_status_payload (status_id, payload, priority) select (select id from status_upsert), ?, case when ? like 'ci/eval%' then 0 when ? like '%required' then 1 else 2 end returning id"
      (owner, repo, payload.headSha, payload.name, toJSON payload, payload.name, payload.name) ::
      IO [Only Int]
  void $ execute_ conn "NOTIFY github_status"

statusHandlers ::
  -- | Seconds without a notification before the connection is checked
  Int ->
  Text ->
  ByteString ->
  IO [(String, GitHub.TokenLease)] ->
  Connection ->
  IO ()
statusHandlers notifTimeoutSec ghEndpointUrl ghUserAgent getValidGitHubToken conn = forever $ do
  let processStatuses = withTransaction conn $ do
        rows <-
          query_
            conn
            ( fromString $
                unwords
                  [ -- Walk the partial index idx_github_status_payload_queue
                    -- (priority, id) and stop at the first row that is the
                    -- most recent unsent payload for its status.  Ordering
                    -- by an expression over github_status.name instead
                    -- forced a hash of all of github_status (millions of
                    -- rows) and a sort of the whole backlog on every send.
                    -- Priority puts evaluation and aggregate check-runs
                    -- first so that downstream consumers (e.g.
                    -- wait-for-hydra) don't have to wait for hundreds of
                    -- per-build check-runs to be posted; see
                    -- 'queueCheckRun'.
                    "SELECT p.id, p.status_id, s.owner, s.repo, p.payload",
                    "FROM github_status_payload p",
                    "JOIN github_status s ON s.id = p.status_id",
                    "WHERE p.sent IS NULL AND p.tries < 5",
                    "  AND NOT EXISTS (",
                    "    SELECT 1 FROM github_status_payload p2",
                    "    WHERE p2.status_id = p.status_id AND p2.id > p.id AND p2.sent IS NULL",
                    "  )",
                    "ORDER BY p.priority, p.id",
                    "LIMIT 1",
                    "FOR UPDATE OF p SKIP LOCKED"
                  ]
            )
        case rows of
          (id', statusId, owner, repo, payload) : _ -> do
            let payload' = case fromJSON payload of
                  Aeson.Success p -> p
                  Aeson.Error e -> error e
            eres <- statusHandler ghEndpointUrl ghUserAgent getValidGitHubToken (GitHub.CheckRun owner repo payload')
            case eres of
              Left ex
                | Just (HTTP.HttpExceptionRequest _req (HTTP.StatusCodeException resp _)) <- fromException ex,
                  Just n <- read . BS.unpack <$> lookup "Retry-After" (HTTP.responseHeaders resp) -> do
                    putStrLn $ "Hit the rate-limit: Retrying in " <> show n <> " seconds..."
                    threadDelay (n * 1000000)
                    return ()
              Left ex
                | Just (HTTP.HttpExceptionRequest _req (HTTP.StatusCodeException resp _)) <- fromException ex,
                  Just remaining <- read . BS.unpack <$> lookup "X-RateLimit-Remaining" (HTTP.responseHeaders resp),
                  remaining == (0 :: Int),
                  Just utc_epoch_offset <- read . BS.unpack <$> lookup "X-RateLimit-Reset" (HTTP.responseHeaders resp) -> do
                    current_utc_epoch <- round <$> getPOSIXTime
                    putStrLn $ "Hit the rate-limit: Retrying in " <> show (utc_epoch_offset - current_utc_epoch) <> " seconds..."
                    threadDelay ((utc_epoch_offset - current_utc_epoch) * 1000000)
                    return ()
              Left ex
                | Just (HTTP.HttpExceptionRequest _req HTTP.ConnectionTimeout) <- fromException ex -> do
                    putStrLn "Connection timeout, retrying..."
                    return ()
              Left e -> do
                Text.putStrLn $ "FAIL [" <> owner <> "/" <> repo <> "/" <> payload'.headSha <> "] " <> payload'.name <> ":" <> Text.pack (show payload'.status) <> ": " <> Text.pack (show e)
                _ <- execute conn "UPDATE github_status_payload SET tries = tries + 1 WHERE id = ?" (Only id' :: Only Int)
                return ()
              Right _res -> do
                Text.putStrLn $ "SENT [" <> owner <> "/" <> repo <> "/" <> payload'.headSha <> "] " <> payload'.name <> ":" <> Text.pack (show payload'.status)
                -- Mark this payload and every older unsent payload for the
                -- same status as sent; GitHub only shows the latest one
                -- anyway.  The queue query skips superseded payloads, so
                -- anything not marked here would stay unsent forever.
                superseded <- execute conn "UPDATE github_status_payload SET sent = NOW() WHERE status_id = ? AND id <= ? AND sent IS NULL" (statusId :: Int, id' :: Int)
                when (superseded > 1) $
                  putStrLn $
                    "MARK " <> show (superseded - 1) <> " superseded payload(s) SENT"
                -- BSL.putStrLn $ "<- " <> encode res
                return ()
            return True
          _ -> return False
  _ <- execute_ conn "LISTEN github_status"
  let loop = do
        executed <- processStatuses
        unless executed $ void $ guardedGetNotification conn (notifTimeoutSec * 1000000)
        when executed loop
  loop

-- | Hydra appends fields to some payloads over time (e.g. @eval_added@ and
-- @eval_failed@ gained a trailing @error_changed@ flag), so the eval patterns
-- ignore anything after the fields we use.  Anything still unrecognised is
-- 'Nothing' for the caller to log and skip, rather than an 'error' that takes
-- the whole bridge down.
toHydraNotification :: Notification -> Maybe Hydra.Notification
toHydraNotification Notification {notificationChannel = chan, notificationData = payload}
  | chan == "eval_started", (_ : jid : _) <- words (cs payload) = Just $ Hydra.EvalStarted (read jid)
  | chan == "eval_added", (_ : jid : eid : _) <- words (cs payload) = Just $ Hydra.EvalAdded (read jid) (read eid)
  | chan == "eval_cached", (_ : jid : eid : _) <- words (cs payload) = Just $ Hydra.EvalCached (read jid) (read eid)
  | chan == "eval_failed", (_ : jid : _) <- words (cs payload) = Just $ Hydra.EvalFailed (read jid)
  | chan == "build_queued", [bid] <- words (cs payload) = Just $ Hydra.BuildQueued (read bid)
  | chan == "cached_build_queued", [_, bid] <- words (cs payload) = Just $ Hydra.BuildQueued (read bid)
  | chan == "build_started", [bid] <- words (cs payload) = Just $ Hydra.BuildStarted (read bid)
  | chan == "build_finished", (bid : depBids) <- words (cs payload) = Just $ Hydra.BuildFinished (read bid) (map read depBids)
  | chan == "cached_build_finished", [_, bid] <- words (cs payload) = Just $ Hydra.BuildFinished (read bid) []
  | otherwise = Nothing

-- | True when the job name contains the @required@ or @nonrequired@ marker
-- as a prefix, suffix, or dot-delimited component (e.g. @required@,
-- @build-required@, @x86_64-linux.required.foo@). Such jobs always have their
-- status reported to GitHub.
hasRequiredKeyword :: Text -> Bool
hasRequiredKeyword job =
  or
    [ n `Text.isPrefixOf` job
        || n `Text.isSuffixOf` job
        || ("." <> n <> ".") `Text.isInfixOf` job
    | n <- ["required", "nonrequired"]
    ]

-- | Outcome of 'decideEmit'. 'CheckPriorFailure' means the caller must
-- additionally consult whether a failure has already been reported for this
-- commit+job before deciding whether to emit.
data EmitDecision = Emit | CheckPriorFailure
  deriving (Eq, Show)

-- | Pure decision: should this Hydra event produce a GitHub status update,
-- based only on the current conclusion (if known) and the job name?
-- Quiet jobs (no keyword) carrying a failure-class conclusion emit; quiet
-- jobs with no conclusion yet, or with a non-failure conclusion, defer to
-- the prior-failure lookup so a retry can clear a previously reported
-- failure (queued/started/success keep flowing once a failure was sent).
decideEmit :: Maybe GitHub.CheckRunConclusion -> Text -> EmitDecision
decideEmit status job
  | hasRequiredKeyword job = Emit
  | Just s <- status, s `elem` GitHub.failureConclusions = Emit
  | otherwise = CheckPriorFailure

-- | Filter which build notifications become GitHub check-runs.
-- When @filterEnabled@ is 'False', every job is reported unconditionally.
-- When 'True', required-keyword jobs and failure-class conclusions emit
-- immediately; everything else consults the bridge's own status records
-- (via the supplied 'IO Bool', which is only executed when needed) so that
-- quiet jobs keep emitting queued/started/success after a Hydra retry once
-- a failure has been reported for the same commit+job.
whenStatusOrJob :: Bool -> Maybe GitHub.CheckRunConclusion -> IO Bool -> Text -> IO [GitHub.CheckRun] -> IO [GitHub.CheckRun]
whenStatusOrJob filterEnabled status priorFailureSentIO job action
  | not filterEnabled = action
  | otherwise = case decideEmit status job of
      Emit -> action
      CheckPriorFailure -> do
        priorFailureSent <- priorFailureSentIO
        if priorFailureSent
          then action
          else Text.putStrLn ("Ignoring job: " <> job) >> pure []

withGithubFlake :: Text -> (Text -> Text -> Text -> IO [GitHub.CheckRun]) -> IO [GitHub.CheckRun]
withGithubFlake flake action
  | Just (owner, repo, hash) <- parseGitHubFlakeURI flake = action owner repo hash
  | "github:" `Text.isPrefixOf` flake
      || "git+https://github.com/" `Text.isPrefixOf` flake =
      Text.putStrLn ("Failed to parse GitHub flake: " <> flake) >> pure []
  | otherwise =
      Text.putStrLn ("Ignoring non-GitHub flake: " <> flake) >> pure []

parseGitHubFlakeURI :: Text -> Maybe (Text, Text, Text)
parseGitHubFlakeURI uri
  | "github:" `Text.isPrefixOf` uri =
      case splitFlakeRef (Text.drop 7 uri) of
        -- TODO: hash == 40 is a _very_ poor approximation to ensure this is a sha
        Just (owner, repo, hash) | Text.length hash == 40 -> Just (owner, repo, hash)
        Just (owner, repo, hash)
          | (hash' : _) <- Text.splitOn "?" hash,
            Text.length hash' == 40 ->
              Just (owner, repo, hash')
        _ -> Nothing
  | otherwise = Nothing
  where
    splitFlakeRef t =
      case Text.splitOn "/" t of
        -- Query parameters can contain slashes that we don't want to split, so combine everything
        -- after repo
        (owner : repo : ts) -> Just (owner, repo, Text.concat ts)
        _ -> Nothing

handleHydraNotification :: Connection -> Text -> FilePath -> Text -> Bool -> CheckRunGroups -> GroupTracker -> Hydra.Notification -> IO [GitHub.CheckRun]
handleHydraNotification conn publicBase stateDir checkRunPrefix filterJobs groups tracker e = (\computation -> catchJust catchJustPredicate computation (handler e)) $ case e of
  -- Evaluations
  (Hydra.EvalStarted jid) -> do
    [(proj, name, flake, triggertime)] <- query conn "select project, name, flake, triggertime from jobsets where id = ?" (Only jid)
    Text.putStrLn $ "Eval Started (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow flake
    withGithubFlake flake $ \owner repo hash ->
      pure $
        singleton $
          GitHub.CheckRun owner repo $
            GitHub.CheckRunPayload
              { name = "ci/eval",
                headSha = hash,
                detailsUrl = Just $ hydraDetailsUrl publicBase ("/jobset/" <> proj <> "/" <> name),
                externalId = Just $ tshow jid,
                status = GitHub.InProgress,
                conclusion = Nothing,
                -- `triggertime` is `Nothing` if the evaluation is cached.
                startedAt = (triggertime :: Maybe Int) >>= Just . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral,
                completedAt = Nothing,
                output = Nothing
              }
  (Hydra.EvalAdded jid eid) -> handleEvalDone jid eid "Added"
  (Hydra.EvalCached jid eid) -> handleEvalDone jid eid "Cached"
  (Hydra.EvalFailed jid) -> do
    [(proj, name, flake, fetcherrormsg, errormsg, errortime)] <- query conn "select project, name, flake, fetcherrormsg, errormsg, errortime from jobsets where id = ?" (Only jid)
    Text.putStrLn $ "Eval Failed (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow (parseGitHubFlakeURI flake)
    withGithubFlake flake $ \owner repo hash ->
      pure $
        singleton $
          GitHub.CheckRun owner repo $
            GitHub.CheckRunPayload
              { name = "ci/eval",
                headSha = hash,
                detailsUrl = Just $ hydraDetailsUrl publicBase ("/jobset/" <> proj <> "/" <> name),
                externalId = Just $ tshow jid,
                status = GitHub.Completed,
                conclusion = Just GitHub.Failure,
                startedAt = Nothing, -- Hydra does not record this information but GitHub still has it
                completedAt = Just . posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral (errortime :: Int),
                output =
                  Just $
                    GitHub.CheckRunOutput
                      { title = "Evaluation failed",
                        summary = "",
                        text =
                          maybe
                            (errormsg >>= mkEvalErrorSummary)
                            mkFetchErrorSummary
                            fetcherrormsg
                      }
              }

  -- Builds
  (Hydra.BuildQueued bid) -> do
    [(proj, name, eid, flake, job, desc)] <- query conn ("select j.project, j.name, e.id, e.flake, b.job, b.description" <> sqlFromBuild) (Only bid)
    Text.putStrLn $ "Build Queued (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
    withGithubFlake flake $ \owner repo hash -> do
      let checkRunName = checkRunPrefix <> job
      reportJob eid job False owner repo hash $
        whenStatusOrJob filterJobs Nothing (BridgeDB.hasPriorReportedFailure conn owner repo hash checkRunName) job $
          pure $
            singleton $
              GitHub.CheckRun owner repo $
                GitHub.CheckRunPayload
                  { name = checkRunName,
                    headSha = hash,
                    detailsUrl = Just $ hydraDetailsUrl publicBase ("/build/" <> tshow bid),
                    externalId = Just $ tshow bid,
                    status = GitHub.Queued,
                    conclusion = Nothing,
                    startedAt = Nothing,
                    completedAt = Nothing,
                    output = Nothing
                  }
  (Hydra.BuildStarted bid) -> do
    [(proj, name, eid, flake, job, desc, starttime)] <- query conn ("select j.project, j.name, e.id, e.flake, b.job, b.description, b.starttime" <> sqlFromBuild) (Only bid)
    Text.putStrLn $ "Build Started (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
    withGithubFlake flake $ \owner repo hash -> do
      let checkRunName = checkRunPrefix <> job
      reportJob eid job False owner repo hash $
        whenStatusOrJob filterJobs Nothing (BridgeDB.hasPriorReportedFailure conn owner repo hash checkRunName) job $
          pure $
            singleton $
              GitHub.CheckRun owner repo $
                GitHub.CheckRunPayload
                  { name = checkRunName,
                    headSha = hash,
                    detailsUrl = Just $ hydraDetailsUrl publicBase ("/build/" <> tshow bid),
                    externalId = Just $ tshow bid,
                    status = GitHub.InProgress,
                    conclusion = Nothing,
                    -- apparently hydra may send the notification before actually starting the build... got 9 seconds difference when testing!
                    startedAt = (starttime :: Maybe Int) >>= Just . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral,
                    completedAt = Nothing,
                    output = Nothing
                  }

  -- note; buildstatus is only != NULL for Finished, Queued and Started leave it as NULL.
  (Hydra.BuildFinished bid depBids) -> do
    [(proj, name, eid, flake, job, desc, finished, status)] <- query conn ("select j.project, j.name, e.id, e.flake, b.job, b.description, b.finished, b.buildstatus" <> sqlFromBuild) (Only bid)
    Text.putStrLn $ "Build Finished (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
    withGithubFlake flake $ \owner repo hash -> do
      checkRun <- handleBuildDone eid bid job status (finished == (1 :: Int)) owner repo hash
      depCheckRuns <-
        sequence $
          (if toEnum status /= Hydra.Succeeded then depBids else []) <&> \depBid -> do
            [(depJob, depStatus, depFinished)] <- query conn "SELECT job, buildstatus, finished FROM builds WHERE id = ?" (Only depBid)
            handleBuildDone eid depBid depJob depStatus (depFinished == (1 :: Int)) owner repo hash
      return $ checkRun ++ concat depCheckRuns
  where
    catchJustPredicate ee
      | Just (_ :: SomeAsyncException) <- fromException ee = Nothing
      | Just (_ :: Async.AsyncCancelled) <- fromException ee = Nothing
      | otherwise = Just ee

    handler :: Hydra.Notification -> SomeException -> IO [GitHub.CheckRun]
    handler n ex = print ("ERROR: " ++ show n ++ " triggert exception " ++ displayException ex) >> pure ([] :: [GitHub.CheckRun])

    sqlFromBuild = " from builds b JOIN jobsets j on b.jobset_id = j.id JOIN jobsetevalmembers m on m.build = b.id JOIN jobsetevals e on e.id = m.eval where b.id = ? order by e.id desc fetch first row only"

    -- For a job in a check-run group (see "Lib.Bridge.CheckRunGroups"),
    -- mark its group for the next flush and report the job itself only if
    -- it failed, or if it has been reported on its own before: once a
    -- failure is on GitHub, a restarted job's later statuses replace it.
    reportJob :: Int -> Text -> Bool -> Text -> Text -> Text -> IO [GitHub.CheckRun] -> IO [GitHub.CheckRun]
    reportJob eid job failed owner repo hash individually =
      case jobGroup groups owner repo job of
        Nothing -> individually
        Just grp -> do
          markGroupDirty tracker (GroupKey eid owner repo hash) grp
          [Only reported] <-
            if failed
              then pure [Only True]
              else
                query
                  conn
                  "SELECT EXISTS (SELECT 1 FROM github_status WHERE owner = ? AND repo = ? AND headSha = ? AND name = ?)"
                  (owner, repo, hash, checkRunPrefix <> job)
          if reported then individually else pure []

    handleEvalDone :: Hydra.JobSetId -> Hydra.EvalId -> Text -> IO [GitHub.CheckRun]
    handleEvalDone jid eid eventName = do
      [(proj, name, flake, fetcherrmsg)] <- query conn "select project, name, flake, fetcherrormsg from jobsets where id = ?" (Only jid)
      -- The evaluation error belongs to this evaluation, not the jobset: by the
      -- time a backlogged notification is handled the jobset may already hold
      -- a later evaluation's errors (or none), and those must not be
      -- reported, or cleared, for this commit.  Hydra links every evaluation
      -- to an evaluationerrors row (an empty message when it succeeded); fall
      -- back to the jobset only for evaluations without one.
      [Only errmsg] <-
        query
          conn
          "select coalesce(e.errormsg, j.errormsg) from jobsetevals je join jobsets j on j.id = je.jobset_id left join evaluationerrors e on e.id = je.evaluationerror_id where je.id = ?"
          (Only eid)
      [(flake', timestamp, checkouttime, evaltime)] <- query conn "select flake, timestamp, checkouttime, evaltime from jobsetevals where id = ?" (Only eid) :: IO [(Text, Int, Int, Int)]
      Text.putStrLn $ "Eval " <> eventName <> " (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
      withGithubFlake flake' $ \owner repo hash -> do
        let startedAt = posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral timestamp
            fetchCompletedAt = addUTCTime (fromIntegral checkouttime) startedAt
            evalCompletedAt = addUTCTime (fromIntegral evaltime) fetchCompletedAt
            summary = mkEvalDurationSummary checkouttime (if isNothing fetcherrmsg then Just evaltime else Nothing)
        -- On a successful evaluation, clear any per-job @ci/eval:<job>@
        -- failures previously reported for this commit. The lookup uses the
        -- latest payload per status and is skipped for evals that failed in
        -- this run, since there is nothing to clear yet.
        let evalSucceeded =
              maybe True Text.null errmsg
                && maybe True Text.null fetcherrmsg
        priorFailedEvalJobs <-
          if evalSucceeded
            then BridgeDB.fetchPriorFailedEvalJobs conn owner repo hash
            else pure []
        -- \| A success payload reusing an existing @ci/eval:<job>@ name. Sent
        -- when a previously-failed per-job eval succeeds on a later retry, so
        -- GitHub no longer shows the stale failure.
        let mkEvalClearedStatus jobCheckRunName =
              GitHub.CheckRun owner repo $
                GitHub.CheckRunPayload
                  { name = jobCheckRunName,
                    headSha = hash,
                    detailsUrl = Just $ hydraDetailsUrl publicBase ("/eval/" <> tshow eid),
                    externalId = Just $ tshow eid,
                    status = GitHub.Completed,
                    conclusion = Just GitHub.Success,
                    startedAt = Just startedAt,
                    completedAt = Just evalCompletedAt,
                    output =
                      Just $
                        GitHub.CheckRunOutput
                          { title = "Evaluation succeeded on retry",
                            summary = summary,
                            text = Nothing
                          }
                  }
        evalStatuses <- pure $ case (errmsg, fetcherrmsg) :: (Maybe Text, Maybe Text) of
          (Just err, _)
            | not (Text.null err) ->
                ( singleton $
                    GitHub.CheckRun owner repo $
                      GitHub.CheckRunPayload
                        { name = "ci/eval",
                          headSha = hash,
                          detailsUrl = Just $ hydraDetailsUrl publicBase ("/eval/" <> tshow eid <> "#tabs-errors"),
                          externalId = Just $ tshow eid,
                          status = GitHub.Completed,
                          conclusion = Just GitHub.Failure,
                          startedAt = Just startedAt,
                          completedAt = Just evalCompletedAt,
                          output =
                            Just $
                              GitHub.CheckRunOutput
                                { title = "Evaluation has errors",
                                  summary = summary,
                                  text = mkEvalErrorSummary err
                                }
                        }
                )
                  -- Creates a failed check run for each job that failed to evaluate,
                  -- up to a limit: a jobset with hundreds of broken jobs would
                  -- otherwise put hundreds of red check-runs on every commit,
                  -- ahead of all build check-runs.  The ci/eval check-run above
                  -- carries the whole error message either way.
                  ++ ( take maxFailedJobEvalChecks (parseFailedJobEvals err) <&> \(job, msg) ->
                         GitHub.CheckRun owner repo $
                           GitHub.CheckRunPayload
                             { name = "ci/eval:" <> job,
                               headSha = hash,
                               detailsUrl = Just $ hydraDetailsUrl publicBase ("/eval/" <> tshow eid <> "#tabs-errors"),
                               externalId = Just $ tshow eid,
                               status = GitHub.Completed,
                               conclusion = Just GitHub.Failure,
                               startedAt = Just startedAt,
                               completedAt = Just evalCompletedAt,
                               output =
                                 Just $
                                   GitHub.CheckRunOutput
                                     { title = "Evaluation failed",
                                       summary = summary,
                                       text = mkEvalErrorSummary msg
                                     }
                             }
                     )
          (_, Just err)
            | not (Text.null err) ->
                singleton $
                  GitHub.CheckRun owner repo $
                    GitHub.CheckRunPayload
                      { name = "ci/eval",
                        headSha = hash,
                        detailsUrl = Just $ hydraDetailsUrl publicBase ("/eval/" <> tshow eid <> "#tabs-errors"),
                        externalId = Just $ tshow eid,
                        status = GitHub.Completed,
                        conclusion = Just GitHub.Failure,
                        startedAt = Just startedAt,
                        completedAt = Just fetchCompletedAt,
                        output =
                          Just $
                            GitHub.CheckRunOutput
                              { title = "Failed to fetch",
                                summary = summary,
                                text = mkFetchErrorSummary err
                              }
                      }
          _ ->
            singleton
              ( GitHub.CheckRun owner repo $
                  GitHub.CheckRunPayload
                    { name = "ci/eval",
                      headSha = hash,
                      detailsUrl = Just $ hydraDetailsUrl publicBase ("/eval/" <> tshow eid),
                      externalId = Just $ tshow eid,
                      status = GitHub.Completed,
                      conclusion = Just GitHub.Success,
                      startedAt = Just startedAt,
                      completedAt = Just evalCompletedAt,
                      output =
                        Just $
                          GitHub.CheckRunOutput
                            { title = "Evaluation succeeded",
                              summary = summary,
                              text = Nothing
                            }
                    }
              )
              -- Overwrite each stale per-job failure with a fresh success so
              -- GitHub no longer shows the failure after the retry.
              ++ (priorFailedEvalJobs <&> mkEvalClearedStatus)
        -- If this evaluation has builds (is not identical to a previous one), this selects no rows.
        -- Otherwise this selects all builds of the latest previous evaluation that differed from its predecessor.
        -- We then submit a status for each of these builds to the jobset's flake URL (the current one).
        -- This is necessary because `(cached_)?build_finished` notifications are not sent by Hydra
        -- when an evaluation is identical to its predecessor / has no builds.
        rows <-
          query
            conn
            "\
            \WITH prev_jobseteval AS (              \
            \    SELECT *                           \
            \    FROM jobsetevals                   \
            \    WHERE                              \
            \        id < ? AND                     \
            \        jobset_id = ? AND              \
            \        hasnewbuilds = 1               \
            \    ORDER BY id DESC                   \
            \    FETCH FIRST ROW ONLY               \
            \)                                      \
            \SELECT e.id, b.id, b.job, b.buildstatus \
            \FROM builds b                          \
            \JOIN prev_jobseteval e ON NOT EXISTS ( \
            \    SELECT NULL                        \
            \    FROM jobsetevals                   \
            \    WHERE                              \
            \        id = ? AND                     \
            \        hasnewbuilds = 1               \
            \)                                      \
            \JOIN jobsetevalmembers m ON            \
            \    m.build = b.id AND                 \
            \    m.eval = e.id                      \
            \WHERE b.finished = 1                   \
            \ "
            [eid, jid, eid] ::
            IO [(Int, Int, Text, Int)]
        buildStatuses <- sequence $ rows <&> \(prevEid, bid, job, status) -> handleBuildDone prevEid bid job status True owner repo hash
        pure $ evalStatuses ++ concat buildStatuses

    handleBuildDone :: Int -> Hydra.BuildId -> Text -> Int -> Bool -> Text -> Text -> Text -> IO [GitHub.CheckRun]
    handleBuildDone eid bid job status finished owner repo hash = do
      let buildStatus = toEnum status
      let ghCheckRunConclusion
            | finished = toCheckRunConclusion buildStatus
            | otherwise = GitHub.Failure
      -- The fully-prefixed name is the lookup key, exactly as emitted below,
      -- so the prior-failure check and the queued payload agree even with a
      -- custom CHECK_RUN_PREFIX (or an empty one).
      let checkRunName = checkRunPrefix <> job
      reportJob eid job (ghCheckRunConclusion `elem` GitHub.failureConclusions) owner repo hash $
        whenStatusOrJob filterJobs (Just ghCheckRunConclusion) (BridgeDB.hasPriorReportedFailure conn owner repo hash checkRunName) job $ do
          steps <- query conn ("SELECT stepnr, drvpath, status FROM buildsteps WHERE build = ? ORDER BY stepnr DESC") (Only bid) :: IO [(Int, String, Maybe Int)]
          buildTimes <- getBuildTimes bid
          let failedSteps = filter (\(_, _, statusInt) -> maybe False (/= Hydra.Succeeded) $ statusInt <&> toEnum) steps
          failedStepLogs <-
            sequence $
              failedSteps <&> \(stepnr, drvpath, _) -> do
                -- Complete log decoding inside this boundary. Lazy decompression
                -- used to escape later while the status payload was encoded.
                logs <-
                  catchJust
                    catchJustPredicate
                    (fmap (fmap Text.unpack) (readBuildLog stateDir drvpath))
                    ( \err -> do
                        Text.putStrLn $ "Warning: could not fetch build log: " <> Text.pack (displayException err)
                        pure Nothing
                    )
                return (stepnr, drvpath, logs)
          output <- query conn ("SELECT path FROM buildoutputs WHERE name = 'out' and build = ? LIMIT 1") (Only bid) :: IO [(Only Text)]
          pure $
            singleton $
              GitHub.CheckRun owner repo $
                GitHub.CheckRunPayload
                  { name = checkRunName,
                    headSha = hash,
                    detailsUrl = Just $ hydraDetailsUrl publicBase ("/build/" <> tshow bid),
                    externalId = Just $ tshow bid,
                    status = GitHub.Completed,
                    conclusion = Just ghCheckRunConclusion,
                    startedAt = buildTimes >>= Just . fst,
                    completedAt = buildTimes >>= Just . snd,
                    output =
                      Just $
                        GitHub.CheckRunOutput
                          { title = tshow buildStatus,
                            summary =
                              if buildStatus == Hydra.Succeeded
                                then -- TODO: This is only the "out" path, maybe we do want to put _all_ paths in here JSON encoded?
                                -- The idea is that on successful builds, we can grab the nix paths (if needed) directly out of the
                                -- github status. And use it for nix-store -r, or similar.
                                  Text.intercalate ", " (fmap fromOnly output)
                                else tshow (length failedSteps) <> " failed steps",
                            text =
                              if buildStatus == Hydra.Succeeded
                                then Nothing
                                else -- TODO: We should include some meta information about the build. Similar to what hydra provides on the
                                -- build page.
                                  let limit = 65535
                                      maxLines = foldr' max 0 $ failedStepLogs <&> \(_, _, logs) -> maybe 0 length logs
                                      indentPrefix = cs $ indentLine "" :: String
                                      stepLogsLines = failedStepLogs <&> \(stepnr, drvpath, logs) -> (stepnr, drvpath, logs >>= Just . lines)
                                   in binarySearch 0 limit $ \numLines ->
                                        let parts =
                                              singleton "# Failed Steps\n\n"
                                                ++ intercalate
                                                  (singleton "\n")
                                                  ( stepLogsLines <&> \(stepnr, drvpath, logLines) ->
                                                      [ "## Step ",
                                                        show stepnr,
                                                        "\n\n",
                                                        -- making code blocks by indenting instead of triple backticks so they cannot be escaped
                                                        "### Derivation\n\n",
                                                        indentPrefix,
                                                        drvpath,
                                                        "\n\n",
                                                        "### Log\n\n"
                                                      ]
                                                        ++ (if numLines < maxLines then ["Last ", show numLines, " lines:\n\n"] else [])
                                                        ++ maybe
                                                          (singleton "*Not available.*\n")
                                                          ((concatMap (\l -> [indentPrefix, l, "\n"])) . (takeEnd numLines))
                                                          logLines
                                                  )
                                            totalLength = foldr' ((+) . length) 0 parts
                                         in ( totalLength < limit && numLines < maxLines,
                                              if totalLength > limit then Nothing else Just . cs $ mconcat parts
                                            )
                          }
                  }

    -- Given an evaluation's error message, returns the jobs that could not be evaluated and their excerpt from the error message.
    maxFailedJobEvalChecks :: Int
    maxFailedJobEvalChecks = 20

    parseFailedJobEvals :: Text -> [(Text, Text)]
    parseFailedJobEvals errormsg =
      (internal errormsg) <&> \(_, job, msg) -> (job, msg)
      where
        internal :: Text -> [(Text, Text, Text)]
        internal rest =
          case rest =~ ("^in job ‘([^’]*)’:$" :: Text) :: (Text, Text, Text, [Text]) of
            (before, _, after, (job : _)) ->
              let next = internal after
                  msg = case next of
                    ((m, _, _) : _) -> m
                    [] -> after
               in singleton (before, job, msg) ++ next
            _ -> []

    getBuildTimes :: Hydra.BuildId -> IO (Maybe (UTCTime, UTCTime))
    getBuildTimes bid = do
      rows <-
        query
          conn
          "\
          \WITH                                                          \
          \    given_build AS (                                          \
          \        SELECT *                                              \
          \        FROM builds                                           \
          \        WHERE id = ?                                          \
          \    ),                                                        \
          \    given_build_output AS (                                   \
          \        SELECT o.*                                            \
          \        FROM buildoutputs o                                   \
          \        JOIN given_build g_b ON o.build = g_b.id              \
          \        FETCH FIRST ROW ONLY                                  \
          \    ),                                                        \
          \    actual_build_step AS (                                    \
          \        SELECT s.*                                            \
          \        FROM buildsteps s                                     \
          \        JOIN buildstepoutputs o ON                            \
          \            o.build = s.build AND                             \
          \            o.stepnr = s.stepnr                               \
          \        JOIN given_build_output g_b_o ON o.path = g_b_o.path  \
          \        WHERE s.busy = 0                                      \
          \        ORDER BY s.status, s.stoptime DESC                    \
          \        FETCH FIRST ROW ONLY                                  \
          \    ),                                                        \
          \    actual_build AS (                                         \
          \        SELECT b.*                                            \
          \        FROM builds b                                         \
          \        JOIN actual_build_step a_b_s ON a_b_s.build = b.id    \
          \    ),                                                        \
          \    given_build_maybe AS (                                    \
          \        SELECT *                                              \
          \        FROM given_build                                      \
          \        WHERE                                                 \
          \            finished = 0 OR                                   \
          \            iscachedbuild = 0                                 \
          \    ),                                                        \
          \    selected_build AS (                                       \
          \        SELECT *                                              \
          \        FROM given_build_maybe                                \
          \                                                              \
          \        UNION ALL                                             \
          \                                                              \
          \        SELECT *                                              \
          \        FROM actual_build                                     \
          \        WHERE NOT EXISTS (SELECT NULL FROM given_build_maybe) \
          \    )                                                         \
          \SELECT selected_build.starttime, selected_build.stoptime      \
          \FROM selected_build                                           \
          \ "
          (Only bid) ::
          IO [(Int, Int)]
      pure $ case rows of
        [(starttime, stoptime)] ->
          Just
            ( posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral starttime,
              posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral stoptime
            )
        _ -> Nothing

    mkEvalDurationSummary :: Int -> Maybe Int -> Text
    mkEvalDurationSummary checkouttime evaltime =
      "Checkout took "
        <> humanReadableDuration (fromIntegral checkouttime * oneSecond)
        <> "."
        <> maybe mempty (\j -> "\nEvaluation took " <> humanReadableDuration (fromIntegral j * oneSecond) <> ".") evaltime

    mkFetchErrorSummary :: Text -> Maybe Text
    mkFetchErrorSummary fetcherrmsg =
      let limit = 65535
          fetcherrmsgLines = Text.lines fetcherrmsg
          maxLines = length fetcherrmsgLines
          indentPrefix = indentLine ""
       in binarySearch 0 limit $ \numLines ->
            let parts =
                  singleton "Fetch error:\n\n"
                    ++ (if numLines < maxLines then ["Last ", tshow numLines, " lines:\n\n"] else [])
                    -- making code blocks by indenting instead of triple backticks so they cannot be escaped
                    ++ (concatMap (\l -> [indentPrefix, l, "\n"]) $ takeEnd numLines fetcherrmsgLines)
                totalLength = foldr' ((+) . Text.length) 0 parts
             in ( totalLength < limit && numLines < maxLines,
                  if totalLength > limit then Nothing else Just . cs $ Text.concat parts
                )

    mkEvalErrorSummary :: Text -> Maybe Text
    mkEvalErrorSummary errmsg =
      let limit = 65535
          errmsgLines = Text.lines errmsg
          maxLines = length errmsgLines
          indentPrefix = indentLine ""
       in binarySearch 0 limit $ \numLines ->
            let parts =
                  singleton "Evaluation error:\n\n"
                    ++ (if numLines < maxLines then ["Last ", tshow numLines, " lines:\n\n"] else [])
                    -- making code blocks by indenting instead of triple backticks so they cannot be escaped
                    ++ (concatMap (\l -> [indentPrefix, l, "\n"]) $ takeEnd numLines errmsgLines)
                totalLength = foldr' ((+) . Text.length) 0 parts
             in ( totalLength < limit && numLines < maxLines,
                  if totalLength > limit then Nothing else Just . cs $ Text.concat parts
                )

statusHandler :: Text -> BS.ByteString -> IO [(String, GitHub.TokenLease)] -> GitHub.CheckRun -> IO (Either SomeException Value)
statusHandler ghEndpointUrl ghUserAgent getGitHubToken checkRun = do
  Text.putStrLn $ "SENDING [" <> checkRun.owner <> "/" <> checkRun.repo <> "/" <> checkRun.payload.headSha <> "] " <> checkRun.payload.name <> ":" <> Text.pack (show checkRun.payload.status)

  -- putStrLn $ "Obtain GitHub token..."
  ghToken <- getGitHubToken
  -- putStrLn $ "GitHub Token: " <> show ghToken

  let token' = case [tok.token | (owner, tok) <- ghToken, Text.pack owner == checkRun.owner] of
        [t] -> Just t
        _ -> throw (toException $ userError ("No GitHub token found for " <> Text.unpack checkRun.owner))
  let githubSettings =
        GitHubSettings
          { token = token',
            userAgent = ghUserAgent,
            apiVersion = GitHub.gitHubApiVersion
          }
  tryNonAsync . liftIO . GitHub.runGitHubRestT githubSettings ghEndpointUrl $
    queryGitHub
      GHEndpoint
        { method = POST,
          endpoint = "/repos/:owner/:repo/check-runs",
          endpointVals =
            [ "owner" := checkRun.owner,
              "repo" := checkRun.repo
            ],
          ghData = GitHub.toKeyValue checkRun.payload
        } ::
    IO (Either SomeException Value)

toCheckRunConclusion :: BuildStatus -> CheckRunConclusion
toCheckRunConclusion = \case
  Hydra.Succeeded -> GitHub.Success
  Hydra.Failed -> GitHub.Failure
  Hydra.DependencyFailed -> GitHub.Failure
  Hydra.Aborted -> GitHub.Cancelled
  Hydra.Cancelled -> GitHub.Cancelled
  Hydra.FailedWithOutput -> GitHub.Failure
  Hydra.TimedOut -> GitHub.TimedOut
  Hydra.LogLimitExceeded -> GitHub.Failure
  Hydra.OutputSizeLimitExceeded -> GitHub.Failure
  Hydra.NonDeterministicBuild -> GitHub.Failure
  Hydra.Other -> GitHub.Failure
