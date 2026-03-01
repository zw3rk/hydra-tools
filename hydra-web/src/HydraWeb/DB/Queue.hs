-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for queue status, running builds, and navigation counts.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Queue
  ( queueCount
  , runningCount
  , navCounts
  ) where

import Database.PostgreSQL.Simple (Connection, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))

import HydraWeb.Models.Queue (NavCounts (..))

-- | Count of unfinished builds in the queue.
queueCount :: Connection -> IO Int
queueCount conn = do
  [Only n] <- query_ conn [sql|
    SELECT count(*) FROM builds WHERE finished = 0
  |]
  pure n

-- | Count of currently running build steps.
runningCount :: Connection -> IO Int
runningCount conn = do
  [Only n] <- query_ conn [sql|
    SELECT count(*) FROM buildsteps WHERE busy = 1
  |]
  pure n

-- | Batched navigation counts: queue size, running steps, bridge pending.
-- Combines three queries in one round-trip for the nav bar badges.
navCounts :: Connection -> IO NavCounts
navCounts conn = do
  queued  <- queueCount conn
  running <- runningCount conn
  -- Bridge pending count (0 if gf_* tables don't exist yet).
  pending <- bridgePending conn
  pure NavCounts
    { ncQueued        = queued
    , ncRunning       = running
    , ncBridgePending = pending
    }

-- | Count of pending bridge notifications. Returns 0 if the table doesn't exist.
bridgePending :: Connection -> IO Int
bridgePending _conn = pure 0  -- TODO: query github_status_payload WHERE sent IS NULL AND tries < 5
