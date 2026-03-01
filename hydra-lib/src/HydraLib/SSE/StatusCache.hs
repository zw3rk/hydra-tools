-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | In-memory status cache for SSE subscribers.
--
-- Maintains the latest status per (owner, repo, sha, check-run name) and
-- broadcasts updates to connected SSE clients via 'TChan'. A periodic GC
-- thread evicts entries older than the configured TTL.
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HydraLib.SSE.StatusCache
  ( StatusCache (..)
  , StatusEntry (..)
  , CommitKey
  , newStatusCache
  , updateStatus
  , broadcastCheckRuns
  , lookupStatuses
  , garbageCollect
  ) where

import Control.Concurrent.STM
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import HydraLib.GitHub.Types

-- | Key identifying a specific commit in a specific repository.
type CommitKey = (Text, Text, Text) -- (owner, repo, sha)

-- | A single check-run status entry in the cache.
data StatusEntry = StatusEntry
  { seName       :: !Text
  , seStatus     :: !CheckRunStatus
  , seConclusion :: !(Maybe CheckRunConclusion)
  , seUpdatedAt  :: !UTCTime
  } deriving (Show, Eq)

instance ToJSON StatusEntry where
  toJSON StatusEntry {..} =
    object $
      [ "name"  .= seName
      , "status" .= seStatus
      , "state"  .= derivedState
      ] ++ maybe [] (\c -> ["conclusion" .= c]) seConclusion
    where
      derivedState :: Text
      derivedState = case seStatus of
        Completed -> case seConclusion of
          Just Success -> "success"
          Just _       -> "failure"
          Nothing      -> "pending"
        _ -> "pending"

-- | Thread-safe in-memory cache and subscriber registry.
data StatusCache = StatusCache
  { scStatuses    :: !(TVar (Map CommitKey (Map Text StatusEntry)))
  , scSubscribers :: !(TVar (Map CommitKey [TChan StatusEntry]))
  }

-- | Create a new empty status cache.
newStatusCache :: IO StatusCache
newStatusCache = StatusCache <$> newTVarIO Map.empty <*> newTVarIO Map.empty

-- | Update the cache with a single status entry and broadcast to subscribers.
updateStatus :: StatusCache -> CommitKey -> StatusEntry -> IO ()
updateStatus cache key entry = do
  chans <- atomically $ do
    modifyTVar' (scStatuses cache) $
      Map.insertWith Map.union key (Map.singleton (seName entry) entry)
    subs <- readTVar (scSubscribers cache)
    return $ Map.findWithDefault [] key subs
  mapM_ (\chan -> atomically $ writeTChan chan entry) chans

-- | Process a list of CheckRuns from the notification handler and broadcast
-- each as an SSE status update.
broadcastCheckRuns :: StatusCache -> [CheckRun] -> IO ()
broadcastCheckRuns cache checkRuns = do
  now <- getCurrentTime
  mapM_ (broadcastOne now) checkRuns
  where
    broadcastOne now (CheckRun owner' repo' payload') = do
      let key = (owner', repo', payload'.headSha)
          entry = StatusEntry
            { seName       = payload'.name
            , seStatus     = payload'.status
            , seConclusion = payload'.conclusion
            , seUpdatedAt  = now
            }
      updateStatus cache key entry

-- | Look up all current statuses for a given commit.
lookupStatuses :: StatusCache -> CommitKey -> IO (Map Text StatusEntry)
lookupStatuses cache key =
  Map.findWithDefault Map.empty key <$> readTVarIO (scStatuses cache)

-- | Evict cache entries older than the given TTL.
-- Returns the number of evicted commit keys.
garbageCollect :: StatusCache -> NominalDiffTime -> IO Int
garbageCollect cache ttl = do
  now <- getCurrentTime
  let cutoff = addUTCTime (negate ttl) now
  atomically $ do
    statuses <- readTVar (scStatuses cache)
    let (expired, kept) =
          Map.partition (all (\e -> seUpdatedAt e < cutoff)) statuses
    writeTVar (scStatuses cache) kept
    modifyTVar' (scSubscribers cache) $
      \subs -> Map.filterWithKey (\k _ -> Map.member k kept) subs
    return (Map.size expired)
