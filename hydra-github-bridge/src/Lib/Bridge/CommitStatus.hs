-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Commit statuses (the Statuses API, not check-runs) and a small durable
-- queue to send them.
--
-- The bridge mirrors the aggregate @hydra@ state as a commit status as well
-- as a check-run (see "Lib.Bridge.HydraCheck"), because a commit status
-- created by an App triggers @on: status@ workflows, which needs no
-- permission beyond the @statuses:write@ the bridge already has.
--
-- The queue keeps one row per (owner, repo, commit, context): queueing a
-- newer state replaces an unsent older one, so only the newest state is
-- sent.  A version number guards against marking a row sent when it was
-- replaced while the old state was in flight.  Statuses are rare (one per
-- state change of a commit), so they get their own table rather than
-- sharing the check-run queue, whose rows are check-run payloads.
module Lib.Bridge.CommitStatus
  ( CommitState (..),
    commitStateText,
    describeStatus,
    commitStatusSchema,
    enqueueCommitStatus,
    CommitStatusWorkerConfig (..),
    commitStatusWorker,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (forM_, forever, void)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple (Connection, execute, execute_, query_)
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue ((:=)),
    StdMethod (POST),
  )
import GitHub.REST.PageLinks (PageLinks)
import Lib.Connection (guardedGetNotification)
import Lib.Exception (tryNonAsync)
import Lib.GitHub.Client (TokenLease, gitHubApiVersion, gitHubRestConfig, queryGitHubRestPage)
import Lib.GitHub.Client qualified as GHClient

data CommitState = StatusPending | StatusSuccess | StatusFailure
  deriving (Eq, Show)

commitStateText :: CommitState -> Text
commitStateText StatusPending = "pending"
commitStateText StatusSuccess = "success"
commitStateText StatusFailure = "failure"

-- | Commit status descriptions are short one-liners: keep to 140 characters.
describeStatus :: Text -> Text
describeStatus = Text.take 140

commitStatusSchema :: [String]
commitStatusSchema =
  [ "CREATE TABLE IF NOT EXISTS github_commit_status ( \
    \  owner TEXT NOT NULL, \
    \  repo TEXT NOT NULL, \
    \  head_sha TEXT NOT NULL, \
    \  context TEXT NOT NULL, \
    \  state TEXT NOT NULL, \
    \  target_url TEXT, \
    \  description TEXT NOT NULL, \
    \  version BIGINT NOT NULL DEFAULT 1, \
    \  updated TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \  sent TIMESTAMPTZ, \
    \  tries INTEGER NOT NULL DEFAULT 0, \
    \  PRIMARY KEY (owner, repo, head_sha, context) \
    \)",
    "CREATE INDEX IF NOT EXISTS idx_github_commit_status_unsent \
    \ON github_commit_status (updated) WHERE sent IS NULL"
  ]

-- | Queue a status, replacing any older state of the same context on the
-- same commit, and wake the worker.
enqueueCommitStatus :: Connection -> Text -> Text -> Text -> Text -> CommitState -> Maybe Text -> Text -> IO ()
enqueueCommitStatus conn owner repo sha context state targetUrl description = do
  _ <-
    execute
      conn
      "INSERT INTO github_commit_status (owner, repo, head_sha, context, state, target_url, description) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (owner, repo, head_sha, context) DO UPDATE \
      \SET state = excluded.state, target_url = excluded.target_url, description = excluded.description, \
      \    version = github_commit_status.version + 1, updated = NOW(), sent = NULL, tries = 0"
      (owner, repo, sha, context, commitStateText state, targetUrl, describeStatus description)
  void $ execute_ conn "NOTIFY github_commit_status"

data CommitStatusWorkerConfig = CommitStatusWorkerConfig
  { endpoint :: Text,
    userAgent :: ByteString,
    getTokens :: IO [(String, TokenLease)],
    -- | Seconds between passes when nothing wakes the worker (retries).
    interval :: Int
  }

-- | Send queued statuses, oldest first.  A failed send is retried on later
-- passes, up to ten times.
commitStatusWorker :: CommitStatusWorkerConfig -> Connection -> IO ()
commitStatusWorker cfg conn = do
  _ <- execute_ conn "LISTEN github_commit_status"
  forever $ do
    sendPending cfg conn
      `catch` \e -> case fromException e of
        Just (async :: SomeAsyncException) -> throwIO async
        Nothing -> do
          putStrLn $ "ERROR: commit status worker: " ++ displayException (e :: SomeException)
          threadDelay 5_000_000
    void $ guardedGetNotification conn (cfg.interval * 1_000_000)

sendPending :: CommitStatusWorkerConfig -> Connection -> IO ()
sendPending cfg conn = do
  rows <-
    query_
      conn
      "SELECT owner, repo, head_sha, context, state, target_url, description, version \
      \FROM github_commit_status WHERE sent IS NULL AND tries < 10 ORDER BY updated LIMIT 100" ::
      IO [(Text, Text, Text, Text, Text, Maybe Text, Text, Int)]
  forM_ rows $ \(owner, repo, sha, context, state, targetUrl, description, version) -> do
    res <- postStatus cfg owner repo sha context state targetUrl description
    case res of
      Right () -> do
        putStrLn $ "STATUS SENT [" <> Text.unpack (owner <> "/" <> repo <> "/" <> sha) <> "] " <> Text.unpack context <> ": " <> Text.unpack state
        -- Only if it was not replaced meanwhile: a newer state stays unsent.
        void $
          execute
            conn
            "UPDATE github_commit_status SET sent = NOW() \
            \WHERE owner = ? AND repo = ? AND head_sha = ? AND context = ? AND version = ?"
            (owner, repo, sha, context, version)
      Left e -> do
        putStrLn $ "STATUS FAIL [" <> Text.unpack (owner <> "/" <> repo <> "/" <> sha) <> "] " <> Text.unpack context <> ": " <> displayException e
        void $
          execute
            conn
            "UPDATE github_commit_status SET tries = tries + 1 \
            \WHERE owner = ? AND repo = ? AND head_sha = ? AND context = ? AND version = ?"
            (owner, repo, sha, context, version)

postStatus :: CommitStatusWorkerConfig -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> Text -> IO (Either SomeException ())
postStatus cfg owner repo sha context state targetUrl description = tryNonAsync $ do
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
  (_ :: Value, _ :: PageLinks) <-
    queryGitHubRestPage
      ghCfg
      GHEndpoint
        { method = POST,
          endpoint = "/repos/:owner/:repo/statuses/:sha",
          endpointVals = ["owner" := owner, "repo" := repo, "sha" := sha],
          ghData =
            ["state" := state, "context" := context, "description" := description]
              ++ maybe [] (\u -> ["target_url" := u]) targetUrl
        }
  pure ()
