{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
module Lib where

import Data.Aeson
-- import Data.Aeson.Schemas

import           Control.Monad.IO.Class       ( liftIO )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromJust)
import GHC.Generics
import GitHub.Data.Webhooks.Events (IssueCommentEvent (..), PushEvent (..), PullRequestEvent (..), PullRequestEventAction (..))
import GitHub.Data.Webhooks.Payload (HookIssueComment (..), HookUser (..), HookPullRequest (..), HookRepository (..), PullRequestTarget (..))
import Servant
import Servant.Client
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import Servant.API.ContentTypes
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))
import qualified Servant.GitHub.Webhook as SGH
import Hydra
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Concurrent.STM (newTVarIO, TChan, readTChan, writeTChan, atomically)
import Control.Monad (forever)

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

pushHook :: TChan Command -> RepoWebhookEvent -> ((), PushEvent) -> Handler ()
pushHook _queue _ (_, ev) = liftIO $ do
  putStrLn $ (show . whUserLogin . evPushSender) ev ++ " pushed a commit causing HEAD SHA to become:"
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

hydraClient :: Text -> Text -> TChan Command -> IO ()
hydraClient user pass queue = do
    manager <- newManager tlsManagerSettings
    jar <- newTVarIO mempty
    let env = (mkClientEnv manager (BaseUrl Https "ci.zw3rk.com" 443 ""))
                { cookieJar = Just jar}

    -- login first
    _ <- flip runClientM env $ login (Just "https://ci.zw3rk.com") (HydraLogin user pass)

    -- loop forever, working down the hydra commands
    forever $ readQ queue >>= flip runClientM env . handleCmd

app :: TChan Command -> GitHubKey -> Application
app queue key
  = serveWithContext
    (Proxy :: Proxy SingleHookEndpointAPI)
    (key :. EmptyContext)
    (singleEndpoint queue)