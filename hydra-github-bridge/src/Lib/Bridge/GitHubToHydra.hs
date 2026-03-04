{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib.Bridge.GitHubToHydra
  ( GitHubToHydraEnv (..),
    GitHubToHydraT (..),
    hydraClientEnv,
    app,
    singleEndpoint,
    pushHook,
    escapeHydraName,
    parseMergeQueueRef,
    issueCommentHook,
    pullRequestHook,
    checkSuiteHook,
    checkRunHook,
    hydraClient,
    handleCmd,
    on404,
    splitRepo,
    reAuthenticate,
  )
where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (forM_, forever, void)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson qualified as Aeson
import Data.Char (isNumber)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database.PostgreSQL.Simple (Connection)
import GitHub.Data.Webhooks.Events
  ( CheckRunEvent (..),
    CheckRunEventAction (..),
    CheckSuiteEvent (..),
    CheckSuiteEventAction (..),
    IssueCommentEvent (..),
    PullRequestEvent (..),
    PullRequestEventAction (..),
    PushEvent (..),
  )
import GitHub.Data.Webhooks.Payload
  ( HookCheckRun (..),
    HookCheckSuite (..),
    HookChecksInstallation (..),
    HookChecksPullRequest (..),
    HookChecksPullRequestTarget (..),
    HookIssueComment (..),
    HookPullRequest (..),
    HookRepository (..),
    HookSimpleUser (..),
    HookUser (..),
    PullRequestTarget (..),
  )
import Lib.GitHub (GitHubKey, SingleHookEndpointAPI)
import Lib.Hydra (Command, HydraClientEnv)
import Lib.Hydra qualified as Hydra
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (..))
import Network.URI (parseURI)
import Servant
  ( Application,
    Context (..),
    Handler,
    HasServer (..),
    (:<|>) (..),
  )
import Servant qualified
import Servant.Client (ClientEnv, ClientM)
import Servant.Client qualified as Servant
import Servant.GitHub.Webhook (RepoWebhookEvent)
import System.Exit (die)
import Text.Read (readMaybe)

data GitHubToHydraEnv = GitHubToHydraEnv
  { gthEnvHydraClient :: ClientEnv,
    gthEnvGitHubKey :: GitHubKey,
    gthEnvGhAppInstallIds :: [(Text, Int)]
  }

newtype GitHubToHydraT m a = GitHubToHydraT
  {unGitHubToHydraT :: ReaderT GitHubToHydraEnv m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader GitHubToHydraEnv,
      MonadError e
    )

runGitHubToHydraT :: GitHubToHydraEnv -> GitHubToHydraT m a -> m a
runGitHubToHydraT env (GitHubToHydraT action) = runReaderT action env

app :: GitHubToHydraEnv -> Connection -> Application
app env@GitHubToHydraEnv {gthEnvGitHubKey} conn =
  Servant.serveWithContextT
    (Proxy :: Proxy SingleHookEndpointAPI)
    (gthEnvGitHubKey :. EmptyContext)
    (runGitHubToHydraT env)
    (singleEndpoint conn)

singleEndpoint ::
  Connection ->
  ServerT SingleHookEndpointAPI (GitHubToHydraT Handler)
singleEndpoint conn =
  pushHook conn
    :<|> issueCommentHook
    :<|> pullRequestHook conn
    :<|> checkSuiteHook conn
    :<|> checkRunHook conn

-- | Handle push event webhooks under the following cases:
--
--  1. Merge queue deleted: Disable existing Hydra job
--  2. Merge queue pushed: Create or update Hydra job
--  3. Push important branches or tags: Create or update Hydra job
--  4. All others: Logged and ignored
pushHook :: Connection -> RepoWebhookEvent -> ((), PushEvent) -> GitHubToHydraT Handler ()
pushHook conn _ (_, pushEvent@PushEvent {evPushHeadSha = Just headSha})
  | isMergeQueueDeleted =
      maybe
        (liftIO warnParseMergeQueueRefFailed)
        (handleMergeQueueDeleted conn pushEvent headSha)
        (parseMergeQueueRef pushEvent.evPushRef)
  | isMergeQueue =
      maybe
        (liftIO warnParseMergeQueueRefFailed)
        (handleMergeQueuePushed conn pushEvent headSha)
        (parseMergeQueueRef pushEvent.evPushRef)
  | isInterestingBranch = handlePushBranch conn pushEvent headSha
  | otherwise = liftIO $ do
      Text.putStrLn $
        "Ignoring unhandled push event"
          <> maybe mempty ((" from " <>) . (.whUserLogin)) pushEvent.evPushSender
          <> maybe mempty (\sha -> " (" <> sha <> ")") pushEvent.evPushHeadSha
  where
    isMergeQueueDeleted =
      "refs/heads/gh-readonly-queue/" `Text.isPrefixOf` pushEvent.evPushRef
        && "0000000000000000000000000000000000000000" == headSha
    isMergeQueue = "refs/heads/gh-readonly-queue/" `Text.isPrefixOf` pushEvent.evPushRef
    isInterestingBranch =
      pushEvent.evPushRef `elem` ["refs/heads/" <> x | x <- ["main", "master", "develop"]]
        || any
          (`Text.isPrefixOf` pushEvent.evPushRef)
          ["refs/" <> x <> "/" | x <- ["tags", "heads/release", "heads/ci"]]

    warnParseMergeQueueRefFailed =
      Text.putStrLn $
        "Warning: Failed to parse merge queue ref " <> pushEvent.evPushRef
pushHook _conn _ (_, pushEvent@PushEvent {evPushHeadSha = Nothing}) =
  liftIO . Text.putStrLn $
    "Ignoring commit without HEAD SHA"
      <> maybe mempty ((" from " <>) . (.whUserLogin)) pushEvent.evPushSender

-- | Handle merge queue deleted webhook event. GitHub signals this by sending head SHA
-- (`after` JSON key) as "00000...". Instead of deleting the jobset, we disable it so we
-- don't lose past build results. We also set the SHA to "00000..." so they are easy to
-- find and delete.
handleMergeQueueDeleted ::
  (MonadIO io) =>
  Connection ->
  PushEvent ->
  Text ->
  (Text, Int) ->
  GitHubToHydraT io ()
handleMergeQueueDeleted conn pushEvent headSha (targetBranch, pullReqNumber) = do
  let owner = hookRepoOwner pushEvent.evPushRepository
      jobsetName = "merge-queue-" <> Text.show pullReqNumber
      projName = escapeHydraName pushEvent.evPushRepository.whRepoFullName

      jobset =
        Hydra.defHydraFlakeJobset
          { Hydra.hjName = "merge-queue-" <> Text.show pullReqNumber,
            Hydra.hjDescription = "Merge Queue: PR" <> Text.show pullReqNumber <> " -> " <> targetBranch,
            Hydra.hjFlake = "github:" <> pushEvent.evPushRepository.whRepoFullName <> "/" <> headSha,
            -- setting visiblity seems to have no effect...
            Hydra.hjVisible = False,
            -- ... so we just disable it.
            Hydra.hjEnabled = 0
          }

  whenKnownInstallId owner Nothing . liftIO $ do
    Text.putStrLn $ "Adding Update " <> projName <> "/" <> jobsetName <> " to the queue."
    Hydra.writeCommand
      conn
      (Hydra.UpdateJobset pushEvent.evPushRepository.whRepoFullName projName jobsetName jobset)

-- | Handle merge queue pushed event. Creates or updates the Hydra jobset with the
-- specified commit SHA.
handleMergeQueuePushed ::
  (MonadIO io) =>
  Connection ->
  PushEvent ->
  Text ->
  (Text, Int) ->
  GitHubToHydraT io ()
handleMergeQueuePushed conn pushEvent headSha (targetBranch, pullReqNumber) = do
  let owner = hookRepoOwner pushEvent.evPushRepository
      projName = escapeHydraName pushEvent.evPushRepository.whRepoFullName
      jobset =
        Hydra.defHydraFlakeJobset
          { Hydra.hjName = "merge-queue-" <> Text.show pullReqNumber,
            Hydra.hjDescription = "Merge Queue: PR" <> Text.show pullReqNumber <> " -> " <> targetBranch,
            Hydra.hjFlake = "github:" <> pushEvent.evPushRepository.whRepoFullName <> "/" <> headSha
          }

      jobsetName = "merge-queue-" <> Text.pack (show pullReqNumber)

  -- TODO: 'PushEvent' doesn't contain app installation, even though GitHub sends it. This
  -- will be fixed by https://github.com/cuedo/github-webhooks/pull/92. After it is
  -- merged and released, be sure to pass the installation ID.
  whenKnownInstallId owner Nothing . liftIO $ do
    Text.putStrLn $ "Adding Create/Update " <> projName <> "/" <> jobsetName <> " to the queue."
    Hydra.writeCommand conn $
      Hydra.CreateOrUpdateJobset
        pushEvent.evPushRepository.whRepoFullName
        projName
        jobsetName
        jobset

-- | Handle pushed event for the following branches:
--
--  * master
--  * main
--  * develop
--  * tags
--  * branch names beginning with ci or release
--
-- Creates or updates the Hydra jobset with the specified commit SHA.
handlePushBranch ::
  (MonadIO io) =>
  Connection ->
  PushEvent ->
  Text ->
  GitHubToHydraT io ()
handlePushBranch conn pushEvent headSha = do
  let owner = hookRepoOwner pushEvent.evPushRepository
      refName =
        fromMaybe
          (Text.drop (Text.length "refs/tags/") pushEvent.evPushRef)
          (Text.stripPrefix "refs/heads/" pushEvent.evPushRef)
      jobsetName = escapeHydraName refName
      jobset =
        Hydra.defHydraFlakeJobset
          { Hydra.hjName = jobsetName,
            Hydra.hjDescription =
              refName
                <> " "
                <> if "refs/heads/" `Text.isPrefixOf` pushEvent.evPushRef then "branch" else "tag",
            Hydra.hjFlake =
              "github:"
                <> pushEvent.evPushRepository.whRepoFullName
                <> "/"
                <> headSha
          }
      projName = escapeHydraName pushEvent.evPushRepository.whRepoFullName

  -- TODO: 'PushEvent' doesn't contain app installation, even though GitHub sends it. This
  -- will be fixed by https://github.com/cuedo/github-webhooks/pull/92. After it is
  -- merged and released, be sure to pass the installation ID.
  whenKnownInstallId owner Nothing . liftIO $ do
    Text.putStrLn $ "Adding Create/Update " <> projName <> "/" <> jobsetName <> " to the queue."
    Hydra.writeCommand conn $
      Hydra.CreateOrUpdateJobset
        pushEvent.evPushRepository.whRepoFullName
        projName
        jobsetName
        jobset

-- | Attempt to interpret the `ref` from a merge queue. The expected format is:
--
-- refs/heads/gh-readonly-queue/<branch-name>/pr-<number>
parseMergeQueueRef :: Text -> Maybe (Text, Int)
parseMergeQueueRef ref = do
  suffix <- Text.stripPrefix "refs/heads/gh-readonly-queue/" ref
  let (branchName, prPart) = Text.breakOn "/pr-" suffix
  rest <- Text.stripPrefix "/pr-" prPart
  let prNumberText = Text.takeWhile isNumber rest
  prNumber <- readMaybe (Text.unpack prNumberText)

  return (branchName, prNumber)

-- | Remove '/' and '.' by replacing them with '-'. Hydra can't handle these in project
-- and jobset names.
escapeHydraName :: Text -> Text
escapeHydraName = Text.replace "/" "-" . Text.replace "." "-"

-- | Handle issue webhook events. These are always logged and ignored.
issueCommentHook ::
  RepoWebhookEvent ->
  ((), IssueCommentEvent) ->
  GitHubToHydraT Handler ()
issueCommentHook _ (_, commentEvent) = liftIO $ do
  Text.putStrLn $
    "An issue comment was posted: "
      <> commentEvent.evIssueCommentPayload.whIssueCommentBody

-- | Handle PR event webhooks under the following cases:
--
--  * PR Opened/Reopened/Synchronize: Create or update Hydra project and jobset
--  * PR Closed: Disable Hydra jobset
--  * Others: Log and ignore
pullRequestHook ::
  Connection ->
  RepoWebhookEvent ->
  ((), PullRequestEvent) ->
  GitHubToHydraT Handler ()
pullRequestHook conn _ (_, prEvent) =
  case prEvent.evPullReqAction of
    -- Build-triggering actions
    PullRequestOpenedAction
      | shouldIgnore -> logAndIgnore
      | otherwise -> handlePullRequestUpdated conn prEvent
    PullRequestReopenedAction
      | shouldIgnore -> logAndIgnore
      | otherwise -> handlePullRequestUpdated conn prEvent
    PullRequestActionOther "synchronize"
      | shouldIgnore -> logAndIgnore
      | otherwise -> handlePullRequestUpdated conn prEvent
    -- Closed action triggers the jobset to be disabled
    PullRequestClosedAction -> handlePullRequestClosed conn prEvent
    -- All others are ignored
    PullRequestActionOther _ -> logAndIgnore
    PullRequestAssignedAction -> logAndIgnore
    PullRequestUnassignedAction -> logAndIgnore
    PullRequestReviewRequestedAction -> logAndIgnore
    PullRequestReviewRequestRemovedAction -> logAndIgnore
    PullRequestLabeledAction -> logAndIgnore
    PullRequestUnlabeledAction -> logAndIgnore
    PullRequestEditedAction -> logAndIgnore
  where
    isDraft = fromMaybe False prEvent.evPullReqPayload.whPullReqIsDraft
    repoFullName = prEvent.evPullReqRepo.whRepoFullName

    shouldIgnore =
      isDraft
        && repoFullName
          `elem` [ "IntersectMBO/ouroboros-network",
                   "IntersectMBO/cardano-cli",
                   "IntersectMBO/cardano-api"
                 ]

    logAndIgnore =
      liftIO . Text.putStrLn $
        "Ignoring pull request action "
          <> Text.show prEvent.evPullReqAction
          <> " on "
          <> repoFullName
          <> if isDraft then " (draft)" else ""

-- | Handle pull request created/updated/synchronized events. This creates or updates the
-- Hydra jobset with:
--
--  * name: pullrequest-{n}
--  * description: PR {n}: {pr title}
--  * flake = "github:${info.head.repo.owner.login}/${info.head.repo.name}/${info.head.ref}";
handlePullRequestUpdated ::
  (MonadIO io) =>
  Connection ->
  PullRequestEvent ->
  GitHubToHydraT io ()
handlePullRequestUpdated conn prEvent = do
  let owner = hookRepoOwner prEvent.evPullReqRepo
      projName = escapeHydraName prEvent.evPullReqRepo.whRepoFullName
      jobsetName = "pullrequest-" <> Text.show prEvent.evPullReqNumber

      jobset =
        Hydra.defHydraFlakeJobset
          { Hydra.hjName = "pullrequest-" <> Text.show prEvent.evPullReqNumber,
            Hydra.hjDescription =
              "PR "
                <> Text.show prEvent.evPullReqNumber
                <> ": "
                <> prEvent.evPullReqPayload.whPullReqTitle,
            Hydra.hjFlake =
              "github:"
                <> prEvent.evPullReqRepo.whRepoFullName
                <> "/"
                <> prEvent.evPullReqPayload.whPullReqHead.whPullReqTargetSha
          }

  whenKnownInstallId owner prEvent.evPullReqInstallationId . liftIO $ do
    Text.putStrLn $
      "Adding Create/Update " <> projName <> "/" <> jobsetName <> " to the queue."
    Hydra.writeCommand conn $
      Hydra.CreateOrUpdateJobset
        prEvent.evPullReqRepo.whRepoFullName
        projName
        jobsetName
        jobset

-- | Handle pull request closed event. This disables the jobset so we don't lose past
-- build results
handlePullRequestClosed ::
  (MonadIO io) =>
  Connection ->
  PullRequestEvent ->
  GitHubToHydraT io ()
handlePullRequestClosed conn prEvent = do
  let repoName = prEvent.evPullReqRepo.whRepoFullName
      projName = escapeHydraName repoName
      jobsetName = "pullrequest-" <> Text.show prEvent.evPullReqNumber
      repo = prEvent.evPullReqRepo
      installationId = prEvent.evPullReqInstallationId
      owner = hookRepoOwner repo
      jobset =
        Hydra.defHydraFlakeJobset
          { Hydra.hjName = "pullrequest-" <> Text.show prEvent.evPullReqNumber,
            Hydra.hjDescription =
              "PR "
                <> Text.show prEvent.evPullReqNumber
                <> ": "
                <> prEvent.evPullReqPayload.whPullReqTitle,
            Hydra.hjFlake =
              "github:"
                <> repoName
                <> "/"
                <> prEvent.evPullReqPayload.whPullReqHead.whPullReqTargetSha,
            -- setting visiblity seems to have no effect...
            Hydra.hjVisible = False,
            -- so we just disable it.
            Hydra.hjEnabled = 0
          }

  whenKnownInstallId owner installationId . liftIO $ do
    Text.putStrLn $ "Adding Update " <> projName <> "/" <> jobsetName <> " to the queue."
    Hydra.writeCommand conn (Hydra.UpdateJobset repoName projName jobsetName jobset)

-- | Look up the owner of a repository as Text
hookRepoOwner :: HookRepository -> Text
hookRepoOwner HookRepository {whRepoOwner} =
  either (.whSimplUserName) (.whUserLogin) whRepoOwner

-- | Compare the owner and GitHub application installation ID with the configured app
-- install IDs. If there's a match, execute the specified action. Otherwise, log an error.
whenKnownInstallId ::
  (MonadIO io) =>
  Text ->
  Maybe Int ->
  GitHubToHydraT io () ->
  GitHubToHydraT io ()
whenKnownInstallId owner installId action = do
  knownInstallIds <- asks (.gthEnvGhAppInstallIds)
  if matchesKnownInstallId owner installId knownInstallIds
    then action
    else
      liftIO . Text.putStrLn $
        "Ignoring unknown GitHub App Installation: "
          <> owner
          <> maybe mempty (\i -> " (" <> Text.show i <> ")") installId

-- | Determine if GitHub Application Installation ID is whitelisted. If GitHub doesn't send
-- an Installation ID (for example, on repository-specific webhooks), fallback to looking
-- up the owner.
matchesKnownInstallId :: Text -> Maybe Int -> [(Text, Int)] -> Bool
matchesKnownInstallId owner installId knownInstallIds =
  maybe
    (matchesOwnerInstallId knownInstallIds)
    (matchesKnownInstallId' knownInstallIds)
    installId
  where
    matchesKnownInstallId' knownInsts instId = (owner, instId) `elem` knownInsts
    matchesOwnerInstallId = any $ (== owner) . fst

-- | Handle check suite event webhook. When the action is rerequested, the matching Hydra
-- PR jobsets are updated, otherwise it's logged and ignored.
checkSuiteHook ::
  Connection ->
  RepoWebhookEvent ->
  ((), CheckSuiteEvent) ->
  GitHubToHydraT Handler ()
checkSuiteHook conn _ (_, checkEvent) =
  case checkEvent.evCheckSuiteAction of
    CheckSuiteEventActionRerequested -> handleCheckSuiteRerequested conn checkEvent
    _ -> logAndIgnore
  where
    logAndIgnore =
      liftIO . Text.putStrLn $
        "Ignoring unhandled check suite "
          <> Text.show checkEvent.evCheckSuiteAction

-- | Handle check suite reregistered event. The matching Hydra PR jobsets are updated with
-- the specified target commit SHAs (`check_suite.pull_requests.[].head.sha` JSON keys).
handleCheckSuiteRerequested ::
  (MonadIO io) =>
  Connection ->
  CheckSuiteEvent ->
  GitHubToHydraT io ()
handleCheckSuiteRerequested conn checkEvent = do
  clientEnv <- asks (.gthEnvHydraClient)

  let prs = checkEvent.evCheckSuiteCheckSuite.whCheckSuitePullRequests
      repo = checkEvent.evCheckSuiteRepository
      repoName = repo.whRepoFullName
      owner = hookRepoOwner repo
      projName = escapeHydraName repoName
      installId = (.whChecksInstallationId) <$> checkEvent.evCheckSuiteInstallation

  forM_ prs $ \pr -> do
    let jobsetName = "pullrequest-" <> Text.show pr.whChecksPullRequestNumber

    whenKnownInstallId owner installId . liftIO $ do
      -- TODO: Handle failures gracefully
      jobset <-
        Servant.runClientM (Hydra.getJobset projName jobsetName) clientEnv
          >>= either (die . show) return
          >>= \response -> case Aeson.fromJSON response of
            Aeson.Error e -> die $ show e
            Aeson.Success v -> return v

      Text.putStrLn $ "Adding Update " <> projName <> "/" <> jobsetName <> " to the queue."
      Hydra.writeCommand conn $
        Hydra.UpdateJobset repoName projName jobsetName $
          jobset
            { Hydra.hjFlake =
                "github:"
                  <> repoName
                  <> "/"
                  <> pr.whChecksPullRequestHead.whChecksPullRequestTargetSha
            }

-- | Handle check run webhook event. If the action is rerequested (`action = "rerequested"`
-- in JSON), eval or rebuild the jobset. Otherwise, log and ignore it.
checkRunHook ::
  Connection ->
  RepoWebhookEvent ->
  ((), CheckRunEvent) ->
  GitHubToHydraT Handler ()
checkRunHook conn _ (_, checkEvent) =
  case checkEvent.evCheckRunAction of
    CheckRunEventActionRerequested -> handleCheckRunRerequested conn checkEvent
    CheckRunEventActionCreated -> logAndIgnore
    CheckRunEventActionCompleted -> logAndIgnore
    CheckRunEventActionRequestedAction -> logAndIgnore
    CheckRunEventActionOther _ -> logAndIgnore
  where
    logAndIgnore =
      liftIO . Text.putStrLn $
        "Ignoring unhandled check run: "
          <> Text.show checkEvent.evCheckRunAction

-- | Handle check run rerequested action. If the check run (`.check_run.name` JSON key) is
-- ci/eval, evaluate the jobset, otherwise rebuild it.
handleCheckRunRerequested ::
  Connection ->
  CheckRunEvent ->
  GitHubToHydraT Handler ()
handleCheckRunRerequested conn checkEvent = do
  let checkRun = checkEvent.evCheckRunCheckRun
      checkRunName = checkRun.whCheckRunName
      repo = checkEvent.evCheckRunRepository
      repoName = repo.whRepoFullName
      owner = hookRepoOwner repo
      projName = escapeHydraName repoName
      prs = checkRun.whCheckRunPullRequests
      installId = (.whChecksInstallationId) <$> checkEvent.evCheckRunInstallation

  whenKnownInstallId owner installId . liftIO $ do
    if "ci/eval" `Text.isPrefixOf` checkRunName
      then forM_ prs $ \pr -> do
        let jobsetName = "pullrequest-" <> Text.show pr.whChecksPullRequestNumber

        Text.putStrLn $ "Adding Eval " <> projName <> "/" <> jobsetName <> " to the queue."
        Hydra.writeCommand conn $ Hydra.EvaluateJobset projName jobsetName True
      else do
        let externalId = read . Text.unpack $ checkRun.whCheckRunExternalId
        Text.putStrLn $
          "Adding Restart " <> checkRunName <> " #" <> Text.show externalId <> " to the queue."
        Hydra.writeCommand conn $ Hydra.RestartBuild externalId

hydraClientEnv :: Text -> Text -> Text -> IO HydraClientEnv
hydraClientEnv host user pass = do
  mgr <- newManager tlsManagerSettings
  jar <- newTVarIO mempty

  -- Parse host string
  let hostStr = Text.unpack host
  hydraUrl <-
    case parseURI hostStr of
      -- It's a valid URI, use servant's parser
      Just _ -> Servant.parseBaseUrl hostStr
      -- Otherwise, we'll just assume it's a bare host
      _ -> pure (Servant.BaseUrl Servant.Https hostStr 443 "")

  let env =
        (Servant.mkClientEnv mgr hydraUrl)
          { Servant.cookieJar = Just jar
          }
      -- The base url will be passed around in the origin header
      host' = Text.pack (Servant.showBaseUrl hydraUrl)

  -- login first
  Servant.runClientM (Hydra.login (Just host') (Hydra.HydraLogin user pass)) env >>= \case
    Left e -> die (show e)
    Right _ -> pure ()

  return $ Hydra.HydraClientEnv host' user pass env

hydraClient :: HydraClientEnv -> Connection -> IO ()
hydraClient henv@Hydra.HydraClientEnv {hceClientEnv} conn =
  -- loop forever, working down the hydra commands
  forever $
    Hydra.readCommand conn >>= \cmd -> do
      result <- Servant.runClientM (handleCmd henv cmd) hceClientEnv
      case result of
        Left e | Hydra.isAuthError e -> do
          putStrLn "Authentication error detected, re-authenticating..."
          reAuthenticate henv >>= \case
            Left authErr -> putStrLn authErr
            Right () -> do
              -- Retry the command after successful re-authentication
              Servant.runClientM (handleCmd henv cmd) hceClientEnv >>= \case
                Left e' -> print e'
                Right _ -> pure ()
        Left e -> print e
        Right _ -> pure ()

handleCmd :: HydraClientEnv -> Command -> ClientM ()
handleCmd Hydra.HydraClientEnv {..} (Hydra.CreateOrUpdateJobset repoName projName jobsetName jobset) = do
  liftIO (putStrLn $ "Processing Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue.")
  void $
    Hydra.mkJobset projName jobsetName jobset `on404` do
      let proj = snd $ splitRepo repoName
      void $
        Hydra.mkProject
          projName
          ( Hydra.defHydraProject
              { Hydra.hpName = projName,
                Hydra.hpDisplayname = proj,
                Hydra.hpHomepage = "https://github.com/" <> repoName,
                Hydra.hpOwner = hceUser
              }
          )
      Hydra.mkJobset projName jobsetName jobset

  liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " triggering push...")
  void $ Hydra.push (Just hceHost) (Just (projName <> ":" <> jobsetName)) Nothing
  return ()
handleCmd Hydra.HydraClientEnv {..} (Hydra.UpdateJobset repoName projName jobsetName jobset) = do
  liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue.")
  -- ensure we try to get this first, ...
  void $ Hydra.getJobset projName jobsetName
  -- if get fails, no point in making one.
  void $
    Hydra.mkJobset projName jobsetName jobset `on404` do
      let proj = snd $ splitRepo repoName
      void $
        Hydra.mkProject
          projName
          ( Hydra.defHydraProject
              { Hydra.hpName = projName,
                Hydra.hpDisplayname = proj,
                Hydra.hpHomepage = "https://github.com/" <> repoName
              }
          )
      Hydra.mkJobset projName jobsetName jobset

  -- or triggering an eval
  liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " triggering push...")
  void $ Hydra.push (Just hceHost) (Just (projName <> ":" <> jobsetName)) Nothing
  return ()
handleCmd _ (Hydra.DeleteJobset projName jobsetName) = do
  void $ Hydra.rmJobset projName jobsetName
  return ()
handleCmd Hydra.HydraClientEnv {..} (Hydra.EvaluateJobset projName jobsetName force) = do
  liftIO (putStrLn $ "Processing Eval " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue. Triggering push...")
  void $ Hydra.push (Just hceHost) (Just (projName <> ":" <> jobsetName)) (Just force)
  return ()
handleCmd _ (Hydra.RestartBuild bid) = do
  liftIO (putStrLn $ "Processing Restart " ++ show bid ++ " from the queue. Triggering restart...")
  void $ Hydra.restartBuild $ bid
  return ()

-- combinator for handing 404 (not found)
on404 :: ClientM a -> ClientM a -> ClientM a
on404 a b = a `catchError` handle
  where
    handle (Servant.FailureResponse _ (Servant.Response {responseStatusCode = Status {statusCode = 404}})) = b
    handle e = throwError e

splitRepo :: Text -> (Text, Text)
splitRepo repo =
  case Text.splitOn "/" repo of
    (org : proj : _) -> (org, proj)
    _ -> error $ "Lib.splitrepo on " ++ Text.unpack repo

-- Re-authenticate with Hydra when session expires
reAuthenticate :: HydraClientEnv -> IO (Either String ())
reAuthenticate (Hydra.HydraClientEnv host user pass env) = do
  result <- Servant.runClientM (Hydra.login (Just host) (Hydra.HydraLogin user pass)) env
  case result of
    Left e -> return $ Left ("Re-authentication failed: " ++ show e)
    Right _ -> return $ Right ()
