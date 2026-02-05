{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib.GitHub.WebHookServer
  ( PushHookAPI,
    IssueCommentHookAPI,
    PullRequestHookAPI,
    CheckSuiteHookAPI,
    SingleHookEndpointAPI,
    GitHubKey (..),
    gitHubKey,
  )
where

import Data.ByteString.Char8 (ByteString)
import GitHub.Data.Webhooks.Events
  ( CheckRunEvent,
    CheckSuiteEvent,
    IssueCommentEvent,
    PullRequestEvent,
    PushEvent,
  )
import Servant (Context (..), HasContextEntry (..), JSON, Post, (:<|>), (:>))
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))
import qualified Servant.GitHub.Webhook as GitHub

-- Push Hook
type PushHookAPI =
  GitHubEvent '[ 'WebhookPushEvent]
    :> GitHubSignedReqBody '[JSON] PushEvent
    :> Post '[JSON] ()

-- Issue Comment Hook
type IssueCommentHookAPI =
  GitHubEvent '[ 'WebhookIssueCommentEvent]
    :> GitHubSignedReqBody '[JSON] IssueCommentEvent
    :> Post '[JSON] ()

-- PullRequest Hook
type PullRequestHookAPI =
  GitHubEvent '[ 'WebhookPullRequestEvent]
    :> GitHubSignedReqBody '[JSON] PullRequestEvent
    :> Post '[JSON] ()

-- Check Suite Hook
type CheckSuiteHookAPI =
  GitHubEvent '[ 'WebhookCheckSuiteEvent]
    :> GitHubSignedReqBody '[JSON] CheckSuiteEvent
    :> Post '[JSON] ()

-- Check Run Hook
type CheckRunHookAPI =
  GitHubEvent '[ 'WebhookCheckRunEvent]
    :> GitHubSignedReqBody '[JSON] CheckRunEvent
    :> Post '[JSON] ()

type SingleHookEndpointAPI =
  "hook"
    :> ( PushHookAPI
           :<|> IssueCommentHookAPI
           :<|> PullRequestHookAPI
           :<|> CheckSuiteHookAPI
           :<|> CheckRunHookAPI
       )

newtype GitHubKey = GitHubKey (forall result. GitHub.GitHubKey result)

gitHubKey :: ByteString -> GitHubKey
gitHubKey k = GitHubKey $ GitHub.gitHubKey (pure k)

instance HasContextEntry '[GitHubKey] (GitHub.GitHubKey result) where
  getContextEntry (GitHubKey x :. _) = x
