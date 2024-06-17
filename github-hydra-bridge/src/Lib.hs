{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Lib where

import           Control.Concurrent.STM       (newTVarIO)
import           Control.Monad                (forM_, forever, void)
import           Control.Monad.Error.Class    (catchError)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Aeson                   (fromJSON)
import qualified Data.Aeson                   as Aeson
import           Data.ByteString.Char8        (ByteString)
import           Data.Char                    (isNumber)
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           DsQueue                      (DsQueue)
import qualified DsQueue
import           GitHub.Data.Webhooks.Events  (CheckRunEvent (..),
                                               CheckRunEventAction (..),
                                               CheckSuiteEvent (..),
                                               CheckSuiteEventAction (..),
                                               IssueCommentEvent (..),
                                               PullRequestEvent (..),
                                               PullRequestEventAction (..),
                                               PushEvent (..))
import           GitHub.Data.Webhooks.Payload (HookCheckRun (..),
                                               HookCheckSuite (..),
                                               HookChecksPullRequest (..),
                                               HookChecksPullRequestTarget (..),
                                               HookIssueComment (..),
                                               HookPullRequest (..),
                                               HookRepository (..),
                                               HookUser (..),
                                               PullRequestTarget (..))
import           Hydra
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Types.Status    (Status (..))
import           Servant
import           Servant.Client
import qualified Servant.GitHub.Webhook       as SGH
import           Servant.GitHub.Webhook       (GitHubEvent, GitHubSignedReqBody,
                                               RepoWebhookEvent (..))
import           System.Exit                  (die)
import           Text.Read                    (readMaybe)

newtype GitHubKey = GitHubKey (forall result. SGH.GitHubKey result)

data Command
    = UpdateJobset Text Text Text HydraJobset         -- only update it, never create
    | CreateOrUpdateJobset Text Text Text HydraJobset -- create or update.
    | DeleteJobset Text Text
    | EvaluateJobset Text Text
    | RestartBuild Int
    deriving (Read, Show)

gitHubKey :: ByteString -> GitHubKey
gitHubKey k = GitHubKey (SGH.gitHubKey $ pure k)

instance HasContextEntry '[GitHubKey] (SGH.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x

-- Push Hook
type PushHookAPI =
  GitHubEvent '[ 'WebhookPushEvent ]
    :> GitHubSignedReqBody '[JSON] PushEvent
    :> Post '[JSON] ()

parseMergeQueueRef :: Text -> Maybe (Text, Int)
parseMergeQueueRef ref = do

    suffix <- Text.stripPrefix "refs/heads/gh-readonly-queue/" ref
    let (branchName, prPart) = Text.breakOn "/pr-" suffix
    rest <- Text.stripPrefix "/pr-" prPart
    let prNumberText = Text.takeWhile isNumber rest
    prNumber <- readMaybe (Text.unpack prNumberText) :: Maybe Int

    return (branchName, prNumber)

pushHook :: DsQueue Command -> RepoWebhookEvent -> ((), PushEvent) -> Handler ()
pushHook queue _ (_, PushEvent { evPushRef = ref, evPushHeadSha = Just headSha, evPushRepository = HookRepository { whRepoFullName = repoName } })
    | "refs/heads/gh-readonly-queue/" `Text.isPrefixOf` ref
    , Just (targetBranch, pullReqNumber) <- parseMergeQueueRef ref
    , "0000000000000000000000000000000000000000" == headSha
    = liftIO $ do
        let projName = escapeHydraName repoName
            jobsetName = "merge-queue-" <> Text.pack (show pullReqNumber)

        let jobset = defHydraFlakeJobset
                { hjName = "merge-queue-" <> Text.pack (show pullReqNumber)
                , hjDescription = "Merge Queue: PR" <> Text.pack (show pullReqNumber) <> " -> " <> targetBranch
                , hjFlake = "github:" <> repoName <> "/" <> headSha
                -- setting visiblity seems to have no effect...
                , hjVisible = False
                -- ... so we just disable it.
                , hjEnabled = 0
                }
        -- We Update the Jobset instead of Delete, so that past build results will
        -- still be available.  This should update the sha to 000000, and as such
        -- allow us to find them and delete them later.
        liftIO $ do
            putStrLn $ "Adding Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
            DsQueue.write queue (UpdateJobset repoName projName jobsetName jobset)

    | "refs/heads/gh-readonly-queue/" `Text.isPrefixOf` ref
    , Just (targetBranch, pullReqNumber) <- parseMergeQueueRef ref
    = liftIO $ do
        let jobset = defHydraFlakeJobset
                { hjName = "merge-queue-" <> Text.pack (show pullReqNumber)
                , hjDescription = "Merge Queue: PR" <> Text.pack (show pullReqNumber) <> " -> " <> targetBranch
                , hjFlake = "github:" <> repoName <> "/" <> headSha
                }

            projName = escapeHydraName repoName
            jobsetName = "merge-queue-" <> Text.pack (show pullReqNumber)

        liftIO $ do
            putStrLn $ "Adding Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
            DsQueue.write queue (CreateOrUpdateJobset repoName projName jobsetName jobset)
    | (ref `elem` [ "refs/heads/" <> x | x <- [ "main", "master", "develop" ]]) || ("refs/heads/release/" `Text.isPrefixOf` ref) || ("refs/heads/ci/" `Text.isPrefixOf` ref)
    = liftIO $ do
        let projName = escapeHydraName repoName
            refName = Text.drop (Text.length "refs/heads/") ref
            jobsetName = escapeHydraName refName
            jobset = defHydraFlakeJobset
                { hjName = jobsetName
                , hjDescription = refName <> " branch"
                , hjFlake = "github:" <> repoName <> "/" <> headSha
                }

        liftIO $ do
            putStrLn $ "Adding Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
            DsQueue.write queue (CreateOrUpdateJobset repoName projName jobsetName jobset)

pushHook _queue _ (_, ev) = liftIO $ do
    putStrLn $ (show . whUserLogin . fromJust . evPushSender) ev ++ " pushed a commit causing HEAD SHA to become:"
    print $ (fromJust . evPushHeadSha) ev

-- PullRequest Hook
type PullRequestHookAPI =
    GitHubEvent '[ 'WebhookPullRequestEvent ]
    :> GitHubSignedReqBody '[JSON] PullRequestEvent
    :> Post '[JSON] ()

pullRequestHook :: DsQueue Command -> RepoWebhookEvent -> ((), PullRequestEvent) -> Handler ()
pullRequestHook queue _ (_, ev@PullRequestEvent
        { evPullReqAction = action
        , evPullReqPayload = HookPullRequest{ whPullReqIsDraft = isDraft }
        , evPullReqRepo = HookRepository{ whRepoFullName = repoName }
        })
    | action `elem` [ PullRequestOpenedAction
                    , PullRequestReopenedAction
                    , (PullRequestActionOther "synchronize") ] &&
      (not (repoName `elem` [ "IntersectMBO/ouroboros-network" ]) || not (maybe False id isDraft))
    = liftIO $ do
    -- we now want to send a request to
    -- $hydraApiUrl
    -- with
    -- POST /jobset/:project-id/:jobset-id
    -- { name: string
    -- , description: string || null
    -- , nixexprinput: string || null
    -- , nixexprpath: string || null
    -- , errormsg: string || null
    -- , errortime: string || null
    -- , lastcheckedtime: integer || null
    -- , triggertime: integer || null
    -- , enabled: integer (0: disabled, 1: enabled, 2: one-shot, 3: one-at-a-time)
    -- , enableemail: boolean
    -- , enable_dynamic_run_command: boolean
    -- , visible: boolean
    -- , emailoverride: string
    -- , keepnr: integer
    -- , checkinterval: integer
    -- , schedulingshares: integer
    -- , fetcherrormsg: string || null
    -- , startime: integer || null
    -- , type: integer
    -- , flake: flake-uri || null
    -- , inputs: ...
    -- } : Jobset
    --
    -- enabled = 1;
    -- hidden = false;
    -- keepnr = 5;
    -- schedulingshares = 42;
    -- checkinterval = 60;
    -- enableemail = false;
    -- emailoverride = "";
    --
    --  type: 1
    --
    --  name: pullrequest-{n}
    --  description: PR {n}: {pr title}
    --  flake = "github:${info.head.repo.owner.login}/${info.head.repo.name}/${info.head.ref}";
    --
    --
    let jobset = defHydraFlakeJobset
            { hjName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev))
            , hjDescription = "PR " <> Text.pack (show (evPullReqNumber ev)) <> ": " <> whPullReqTitle (evPullReqPayload ev)
            , hjFlake = "github:" <> repoName
                                <> "/"
                                <> whPullReqTargetSha (whPullReqHead (evPullReqPayload ev))
            }

        projName = escapeHydraName repoName
        jobsetName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev))

    liftIO $ do
        putStrLn $ "Adding Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
        DsQueue.write queue (CreateOrUpdateJobset repoName projName jobsetName jobset)

pullRequestHook queue _ (_, ev@PullRequestEvent{ evPullReqAction = PullRequestClosedAction }) = liftIO $ do
    let repoName = whRepoFullName (evPullReqRepo ev)
        projName = escapeHydraName repoName
        jobsetName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev))

    let jobset = defHydraFlakeJobset
            { hjName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev))
            , hjDescription = "PR " <> Text.pack (show (evPullReqNumber ev)) <> ": " <> whPullReqTitle (evPullReqPayload ev)
            , hjFlake = "github:" <> repoName
                                  <> "/"
                                  <> whPullReqTargetSha (whPullReqHead (evPullReqPayload ev))
            -- setting visiblity seems to have no effect...
            , hjVisible = False
            -- ... so we just disable it.
            , hjEnabled = 0
            }

    -- We Update the Jobset instead of Delete, so that past build results will
    -- still be available.  This should update the sha to 000000, and as such
    -- allow us to find them and delete them later.
    liftIO $ do
        putStrLn $ "Adding Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
        DsQueue.write queue (UpdateJobset repoName projName jobsetName jobset)

pullRequestHook _ _ (_, ev)
    = liftIO (putStrLn $ "Unhandled pullRequestEvent with action: " ++ show (evPullReqAction ev))

escapeHydraName :: Text -> Text
escapeHydraName = Text.replace "/" "-" . Text.replace "." "-"

splitRepo :: Text -> (Text, Text)
splitRepo repo =
  case Text.splitOn "/" repo of
    (org:proj:_) -> (org, proj)
    _            -> error $ "Lib.splitrepo on " ++ Text.unpack repo

-- Issue Comment Hook
type IssueCommentHookAPI =
  GitHubEvent '[ 'WebhookIssueCommentEvent]
    :> GitHubSignedReqBody '[JSON] IssueCommentEvent
    :> Post '[JSON] ()

issueCommentHook :: RepoWebhookEvent -> ((), IssueCommentEvent) -> Handler ()
issueCommentHook _ (_, ev) = liftIO $ do
  putStrLn "An issue comment was posted:"
  print $ (whIssueCommentBody . evIssueCommentPayload) ev

-- Check Suite Hook
type CheckSuiteHookAPI =
  GitHubEvent '[ 'WebhookCheckSuiteEvent ]
    :> GitHubSignedReqBody '[JSON] CheckSuiteEvent
    :> Post '[JSON] ()

checkSuiteHook :: ClientEnv -> DsQueue Command -> RepoWebhookEvent -> ((), CheckSuiteEvent) -> Handler ()
checkSuiteHook env queue _ (_, ev@CheckSuiteEvent{ evCheckSuiteAction = CheckSuiteEventActionRerequested }) = liftIO $ do
    let prs = whCheckSuitePullRequests $ evCheckSuiteCheckSuite ev
        repoName = whRepoFullName $ evCheckSuiteRepository ev
        projName = escapeHydraName repoName

    forM_ prs $ \pr -> do
        let jobsetName = "pullrequest-" <> Text.pack (show $ whChecksPullRequestNumber pr)

        jobset <- (flip runClientM env $ getJobset projName jobsetName)
            >>= either (die . show) return
            >>= \response -> case fromJSON response of
                Aeson.Error   e -> die $ show e
                Aeson.Success v -> return v

        putStrLn $ "Adding Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
        DsQueue.write queue $ UpdateJobset repoName projName jobsetName $ jobset
            { hjFlake = "github:" <> repoName <> "/" <> whChecksPullRequestTargetSha (whChecksPullRequestHead pr) }

checkSuiteHook _ _ _ (_, ev) = liftIO . putStrLn $ "Unhandled checkSuiteEvent with action: " ++ show (evCheckSuiteAction ev)

-- Check Run Hook
type CheckRunHookAPI =
  GitHubEvent '[ 'WebhookCheckRunEvent ]
    :> GitHubSignedReqBody '[JSON] CheckRunEvent
    :> Post '[JSON] ()

checkRunHook :: DsQueue Command -> RepoWebhookEvent -> ((), CheckRunEvent) -> Handler ()
checkRunHook queue _ (_, ev@CheckRunEvent{ evCheckRunAction = CheckRunEventActionRerequested }) = liftIO $ do
    let checkRun = evCheckRunCheckRun ev
        checkRunName = whCheckRunName checkRun
        repoName = whRepoFullName $ evCheckRunRepository ev
        projName = escapeHydraName repoName
        prs = whCheckRunPullRequests checkRun

    if "ci/eval" `Text.isPrefixOf` checkRunName
    then forM_ prs $ \pr -> do
        let jobsetName = "pullrequest-" <> Text.pack (show $ whChecksPullRequestNumber pr)

        putStrLn $ "Adding Eval " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
        DsQueue.write queue $ EvaluateJobset projName jobsetName
    else do
        let externalId = read . Text.unpack $ whCheckRunExternalId checkRun
        putStrLn $ "Adding Restart " ++ Text.unpack checkRunName ++ " #" ++ show externalId ++ " to the queue."
        DsQueue.write queue $ RestartBuild externalId

checkRunHook _ _ (_, ev) = liftIO . putStrLn $ "Unhandled checkRunEvent with action: " ++ show (evCheckRunAction ev)

type SingleHookEndpointAPI = "hook" :> (PushHookAPI :<|> IssueCommentHookAPI :<|> PullRequestHookAPI :<|> CheckSuiteHookAPI :<|> CheckRunHookAPI)

singleEndpoint :: ClientEnv -> DsQueue Command -> Server SingleHookEndpointAPI
singleEndpoint env queue = (pushHook queue) :<|> issueCommentHook :<|> (pullRequestHook queue) :<|> (checkSuiteHook env queue) :<|> (checkRunHook queue)

-- combinator for handing 404 (not found)
on404 :: ClientM a -> ClientM a -> ClientM a
on404 a b = a `catchError` handle
    where handle (FailureResponse _ (Response { responseStatusCode = Status { statusCode = 404 } })) = b
          handle e = throwError e

handleCmd :: Command -> ClientM ()
handleCmd (CreateOrUpdateJobset repoName projName jobsetName jobset) = do
    liftIO (putStrLn $ "Processing Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue.")
    void $ mkJobset projName jobsetName jobset `on404` do
            let proj = snd $ splitRepo repoName
            void $ mkProject projName (defHydraProject { hpName = projName
                                                , hpDisplayname = proj
                                                , hpHomepage = "https://github.com/" <> repoName
                                                })
            mkJobset projName jobsetName jobset

    liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " triggering push...")
    void $ push $ Just (projName <> ":" <> jobsetName)
    return ()

handleCmd (UpdateJobset repoName projName jobsetName jobset) = do
    liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue.")
    -- ensure we try to get this first, ...
    void $ getJobset projName jobsetName
    -- if get fails, no point in making one.
    void $ mkJobset projName jobsetName jobset `on404` do
            let proj = snd $ splitRepo repoName
            void $ mkProject projName (defHydraProject { hpName = projName
                                                , hpDisplayname = proj
                                                , hpHomepage = "https://github.com/" <> repoName
                                                })
            mkJobset projName jobsetName jobset

    -- or triggering an eval
    liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " triggering push...")
    void $ push $ Just (projName <> ":" <> jobsetName)
    return ()

handleCmd (DeleteJobset projName jobsetName) = do
    void $ rmJobset projName jobsetName
    return ()

handleCmd (EvaluateJobset projName jobsetName) = do
    liftIO (putStrLn $ "Processing Eval " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue. Triggering push...")
    void $ push $ Just (projName <> ":" <> jobsetName)
    return ()

handleCmd (RestartBuild bid) = do
    liftIO (putStrLn $ "Processing Restart " ++ show bid ++ " from the queue. Triggering restart...")
    void $ restartBuild $ bid
    return ()

hydraClientEnv :: Text -> Text -> Text -> IO ClientEnv
hydraClientEnv host user pass = do
    mgr <- newManager tlsManagerSettings
    jar <- newTVarIO mempty
    let env = (mkClientEnv mgr (BaseUrl Https (Text.unpack host) 443 ""))
                { cookieJar = Just jar}

    -- login first
    flip runClientM env (login (Just $ Text.append "https://" host) (HydraLogin user pass)) >>= \case
        Left e  -> die (show e)
        Right _ -> pure ()

    return env

hydraClient :: ClientEnv -> DsQueue Command -> IO ()
hydraClient env queue =
    -- loop forever, working down the hydra commands
    forever $ DsQueue.read queue >>= flip runClientM env . handleCmd >>= \case
        Left e  -> print e
        Right _ -> pure ()

app :: ClientEnv -> DsQueue Command -> GitHubKey -> Application
app env queue key
  = serveWithContext
    (Proxy :: Proxy SingleHookEndpointAPI)
    (key :. EmptyContext)
    (singleEndpoint env queue)
