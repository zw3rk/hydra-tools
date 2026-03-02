-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Simple broadcast hub for Server-Sent Events. Uses STM for
-- thread-safe subscriber management. Each subscriber gets a bounded
-- TChan; slow consumers that don't read in time have messages dropped.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.SSE.Hub
  ( Hub
  , newHub
  , subscribe
  , unsubscribe
  , broadcast
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

-- | Broadcast hub mapping subscriber IDs to their message queues.
-- The Int key is a monotonically increasing subscriber counter.
data Hub = Hub
  { hubNextId :: !(TVar Int)
  , hubSubs   :: !(TVar (Map.Map Int (TBQueue BS.ByteString)))
  }

-- | Create an empty broadcast hub.
newHub :: IO Hub
newHub = do
  nextId <- newTVarIO 0
  subs   <- newTVarIO Map.empty
  pure Hub { hubNextId = nextId, hubSubs = subs }

-- | Register a new subscriber. Returns a (subscriberId, queue) pair.
-- The queue is bounded (16 items); overflows are dropped by 'broadcast'.
subscribe :: Hub -> IO (Int, TBQueue BS.ByteString)
subscribe hub = atomically $ do
  sid <- readTVar (hubNextId hub)
  writeTVar (hubNextId hub) (sid + 1)
  q <- newTBQueue 16
  modifyTVar' (hubSubs hub) (Map.insert sid q)
  pure (sid, q)

-- | Remove a subscriber by ID. Safe to call multiple times.
unsubscribe :: Hub -> Int -> IO ()
unsubscribe hub sid = atomically $
  modifyTVar' (hubSubs hub) (Map.delete sid)

-- | Broadcast a message to all subscribers. Slow consumers whose
-- queues are full have the message dropped (non-blocking write).
broadcast :: Hub -> BS.ByteString -> IO ()
broadcast hub msg = atomically $ do
  subs <- readTVar (hubSubs hub)
  mapM_ tryWrite (Map.elems subs)
  where
    tryWrite q = do
      full <- isFullTBQueue q
      if full then pure () else writeTBQueue q msg
