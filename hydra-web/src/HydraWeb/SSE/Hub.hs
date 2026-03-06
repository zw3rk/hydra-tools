-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Topic-based broadcast hub for Server-Sent Events.
-- Uses STM for thread-safe subscriber management. Each subscriber declares
-- interest in specific topics; broadcasts are routed only to matching
-- subscribers. Every subscriber auto-receives TopicGlobal for nav updates.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.SSE.Hub
  ( Hub
  , Topic (..)
  , newHub
  , subscribeTopics
  , unsubscribe
  , broadcastTo
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Topics for SSE routing. Subscribers declare which topics they want.
data Topic
  = TopicGlobal       -- ^ Nav count updates (auto-subscribed for everyone)
  | TopicBridges      -- ^ Bridge status updates
  | TopicQueue        -- ^ Queue list updates
  | TopicMachines     -- ^ Active steps / machine updates
  | TopicRunningEvals -- ^ Running evaluations updates
  | TopicProject !Text   -- ^ Per-project updates
  | TopicJobset !Text !Text -- ^ Per-jobset updates (project, jobset)
  deriving (Eq, Ord, Show)

-- | A subscriber with their message queue and topic subscriptions.
data Subscriber = Subscriber
  { subQueue  :: !(TBQueue BS.ByteString)
  , subTopics :: !(Set Topic)
  }

-- | Broadcast hub mapping subscriber IDs to their subscriptions.
data Hub = Hub
  { hubNextId :: !(TVar Int)
  , hubSubs   :: !(TVar (Map.Map Int Subscriber))
  }

-- | Create an empty broadcast hub.
newHub :: IO Hub
newHub = do
  nextId <- newTVarIO 0
  subs   <- newTVarIO Map.empty
  pure Hub { hubNextId = nextId, hubSubs = subs }

-- | Register a subscriber interested in specific topics.
-- TopicGlobal is always included automatically.
subscribeTopics :: Hub -> Set Topic -> IO (Int, TBQueue BS.ByteString)
subscribeTopics hub topics = atomically $ do
  sid <- readTVar (hubNextId hub)
  writeTVar (hubNextId hub) (sid + 1)
  q <- newTBQueue 16
  let sub = Subscriber q (Set.insert TopicGlobal topics)
  modifyTVar' (hubSubs hub) (Map.insert sid sub)
  pure (sid, q)

-- | Remove a subscriber by ID. Safe to call multiple times.
unsubscribe :: Hub -> Int -> IO ()
unsubscribe hub sid = atomically $
  modifyTVar' (hubSubs hub) (Map.delete sid)

-- | Broadcast a message only to subscribers of a specific topic.
broadcastTo :: Hub -> Topic -> BS.ByteString -> IO ()
broadcastTo hub topic msg = atomically $ do
  subs <- readTVar (hubSubs hub)
  let matching = Map.filter (\s -> Set.member topic (subTopics s)) subs
  mapM_ (tryWrite . subQueue) (Map.elems matching)
  where
    tryWrite q = do
      full <- isFullTBQueue q
      if full then pure () else writeTBQueue q msg
