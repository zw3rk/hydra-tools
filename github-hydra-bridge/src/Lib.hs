{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE LambdaCase            #-}
module Lib where

import           Data.Aeson
-- import Data.Aeson.Schemas

import           Control.Concurrent.STM       (TChan, atomically, newTVarIO,
                                               readTChan, writeTChan)
import           Control.Monad                (forever)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as C8
import           Data.Char                    (isNumber)
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           GHC.Generics
import           GitHub.Data.Webhooks.Events  (IssueCommentEvent (..),
                                               PullRequestEvent (..),
                                               PullRequestEventAction (..),
                                               PushEvent (..))
import           GitHub.Data.Webhooks.Payload (HookIssueComment (..),
                                               HookPullRequest (..),
                                               HookRepository (..),
                                               HookUser (..),
                                               PullRequestTarget (..))
import           Hydra
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Servant
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.Client
import qualified Servant.GitHub.Webhook       as SGH
import           Servant.GitHub.Webhook       (GitHubEvent, GitHubSignedReqBody,
                                               RepoWebhookEvent (..))
import           System.Exit                  (die)
import           Text.Read                    (readMaybe)

newtype GitHubKey = GitHubKey (forall result. SGH.GitHubKey result)

data Command
    = UpdateJobset Text Text HydraJobset         -- only update it, never create
    | CreateOrUpdateJobset Text Text HydraJobset -- create or update.
    | DeleteJobset Text Text
    deriving (Show)

gitHubKey :: IO BS.ByteString -> GitHubKey
gitHubKey k = GitHubKey (SGH.gitHubKey k)

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

pushHook :: TChan Command -> RepoWebhookEvent -> ((), PushEvent) -> Handler ()
pushHook queue _ (_, ev@PushEvent { evPushRef = ref, evPushHeadSha = Just headSha, evPushRepository = HookRepository { whRepoFullName = repoName } })
    | "refs/heads/gh-readonly-queue/" `Text.isPrefixOf` ref
    , Just (targetBranch, pullReqNumber) <- parseMergeQueueRef ref
    , "0000000000000000000000000000000000000000" == headSha
    = liftIO $ do
        let projName = repoToProject repoName
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
            writeQ queue (UpdateJobset projName jobsetName jobset)

    | "refs/heads/gh-readonly-queue/" `Text.isPrefixOf` ref
    , Just (targetBranch, pullReqNumber) <- parseMergeQueueRef ref
    = liftIO $ do
        let jobset = defHydraFlakeJobset
                { hjName = "merge-queue-" <> Text.pack (show pullReqNumber)
                , hjDescription = "Merge Queue: PR" <> Text.pack (show pullReqNumber) <> " -> " <> targetBranch
                , hjFlake = "github:" <> repoName <> "/" <> headSha
                }

            projName = repoToProject repoName
            jobsetName = "merge-queue-" <> Text.pack (show pullReqNumber)

        liftIO $ do
            putStrLn $ "Adding Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
            writeQ queue (CreateOrUpdateJobset projName jobsetName jobset)

pushHook _queue _ (_, ev) = liftIO $ do
    putStrLn $ (show . whUserLogin . fromJust . evPushSender) ev ++ " pushed a commit causing HEAD SHA to become:"
    print $ (fromJust . evPushHeadSha) ev

-- PullRequest Hook
type PullRequestHookAPI =
    GitHubEvent '[ 'WebhookPullRequestEvent ]
    :> GitHubSignedReqBody '[JSON] PullRequestEvent
    :> Post '[JSON] ()

pullRequestHook :: TChan Command -> RepoWebhookEvent -> ((), PullRequestEvent) -> Handler ()
pullRequestHook queue _ (_, ev@PullRequestEvent{ evPullReqAction = action })
    | action `elem` [ PullRequestOpenedAction
                    , PullRequestReopenedAction
                    , (PullRequestActionOther "synchronize") ]
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
            , hjFlake = "github:" <> whRepoFullName (evPullReqRepo ev)
                                <> "/"
                                <> whPullReqTargetSha (whPullReqHead (evPullReqPayload ev))
            }

        projName = repoToProject (whRepoFullName (evPullReqRepo ev))
        jobsetName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev))

    liftIO $ do
        putStrLn $ "Adding Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " to the queue."
        writeQ queue (CreateOrUpdateJobset projName jobsetName jobset)

pullRequestHook queue _ (_, ev@PullRequestEvent{ evPullReqAction = PullRequestClosedAction }) = liftIO $ do
    let projName = repoToProject (whRepoFullName (evPullReqRepo ev))
        jobsetName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev))

    let jobset = defHydraFlakeJobset
            { hjName = "pullrequest-" <> Text.pack (show (evPullReqNumber ev))
            , hjDescription = "PR " <> Text.pack (show (evPullReqNumber ev)) <> ": " <> whPullReqTitle (evPullReqPayload ev)
            , hjFlake = "github:" <> whRepoFullName (evPullReqRepo ev)
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
        writeQ queue (UpdateJobset projName jobsetName jobset)

pullRequestHook _ _ (_, ev)
    = liftIO (putStrLn $ "Unhandled pullRequestEvent with action: " ++ show (evPullReqAction ev))

repoToProject :: Text -> Text
repoToProject = Text.replace "/" "-" . Text.replace "." "-"

-- Issue Comment Hook
type IssueCommentHookAPI =
  GitHubEvent '[ 'WebhookIssueCommentEvent]
    :> GitHubSignedReqBody '[JSON] IssueCommentEvent
    :> Post '[JSON] ()

issueCommentHook :: RepoWebhookEvent -> ((), IssueCommentEvent) -> Handler ()
issueCommentHook _ (_, ev) = liftIO $ do
  putStrLn "An issue comment was posted:"
  print $ (whIssueCommentBody . evIssueCommentPayload) ev

type SingleHookEndpointAPI = "hook" :> (PushHookAPI :<|> IssueCommentHookAPI :<|> PullRequestHookAPI)

singleEndpoint :: TChan Command -> Server SingleHookEndpointAPI
singleEndpoint queue = (pushHook queue) :<|> issueCommentHook :<|> (pullRequestHook queue)

handleCmd :: Command -> ClientM ()
handleCmd (CreateOrUpdateJobset projName jobsetName jobset) = do
    liftIO (putStrLn $ "Processing Create/Update " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue.")
    mkJobset projName jobsetName jobset
    liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " triggering push...")
    push $ Just (projName <> ":" <> jobsetName)
    return ()

handleCmd (UpdateJobset projName jobsetName jobset) = do
    liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " from the queue.")
    -- ensure we try to get this first, ...
    getJobset projName jobsetName
    -- if get fails, no point in making one.
    mkJobset projName jobsetName jobset
    -- or triggering an eval
    liftIO (putStrLn $ "Processing Update " ++ show projName ++ "/" ++ show jobsetName ++ " triggering push...")
    push $ Just (projName <> ":" <> jobsetName)
    return ()

handleCmd (DeleteJobset projName jobsetName) = do
    rmJobset projName jobsetName
    return ()

readQ = atomically . readTChan
writeQ ch v = atomically $ writeTChan ch v

hydraClient :: Text -> Text -> Text -> TChan Command -> IO ()
hydraClient host user pass queue = do
    manager <- newManager tlsManagerSettings
    jar <- newTVarIO mempty
    let env = (mkClientEnv manager (BaseUrl Https (Text.unpack host) 443 ""))
                { cookieJar = Just jar}

    -- login first
    flip runClientM env (login (Just $ Text.append "https://" host) (HydraLogin user pass)) >>= \case
        Left e  -> die (show e)
        Right _ -> pure ()

    -- loop forever, working down the hydra commands
    forever $ readQ queue >>= flip runClientM env . handleCmd >>= \case
        Left e  -> print e
        Right _ -> pure ()

app :: TChan Command -> GitHubKey -> Application
app queue key
  = serveWithContext
    (Proxy :: Proxy SingleHookEndpointAPI)
    (key :. EmptyContext)
    (singleEndpoint queue)
