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
  , queueSummary
  , systemQueueSummary
  , activeSteps
  , recentSteps
  , newsItems
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))

import HydraWeb.Models.Queue
import HydraWeb.Models.Build (BuildStep (..))

-- | Count of unfinished builds in the queue.
queueCount :: Connection -> IO Int
queueCount conn = do
  [Only n] <- query_ conn [sql|
    SELECT count(*) FROM builds WHERE finished = 0
  |]
  pure n

-- | Count of currently running build steps (distinct builds).
runningCount :: Connection -> IO Int
runningCount conn = do
  [Only n] <- query_ conn [sql|
    SELECT count(DISTINCT build) FROM buildsteps WHERE busy != 0
  |]
  pure n

-- | Batched navigation counts: queue size, running steps, bridge pending.
navCounts :: Connection -> IO NavCounts
navCounts conn = do
  queued  <- queueCount conn
  running <- runningCount conn
  pending <- bridgePending conn
  pure NavCounts
    { ncQueued        = queued
    , ncRunning       = running
    , ncBridgePending = pending
    }

-- | Count of pending bridge notifications. Returns 0 if the table doesn't exist.
bridgePending :: Connection -> IO Int
bridgePending conn = do
  rows <- query_ conn [sql|
    SELECT count(*)
    FROM pg_catalog.pg_tables
    WHERE schemaname = 'public' AND tablename = 'github_status_pending'
  |]
  case rows of
    [Only (0 :: Int)] -> pure 0
    _ -> do
      r <- query_ conn [sql|
        SELECT count(*) FROM github_status_pending WHERE sent IS NULL AND tries < 5
      |]
      case r of
        [Only n] -> pure n
        _        -> pure 0

-- | Queue grouped by jobset with oldest/newest timestamps.
queueSummary :: Connection -> IO [QueueSummary]
queueSummary conn = do
  rows <- query_ conn [sql|
    SELECT j.project, j.name, count(*) AS queued,
           min(b.timestamp) AS oldest, max(b.timestamp) AS newest
    FROM builds b
    JOIN jobsets j ON j.id = b.jobset_id
    WHERE b.finished = 0
    GROUP BY j.project, j.name
    ORDER BY queued DESC
  |]
  pure $ map (\(p, j, q, o, n) -> QueueSummary p j q (fromMaybe 0 o) (fromMaybe 0 n)) rows

-- | Queue grouped by system type.
systemQueueSummary :: Connection -> IO [SystemQueueRow]
systemQueueSummary conn = do
  rows <- query_ conn [sql|
    SELECT system, count(*) AS c
    FROM builds
    WHERE finished = 0
    GROUP BY system
    ORDER BY c DESC
  |]
  pure $ map (\(s, c) -> SystemQueueRow s c) rows

-- | Currently-running build steps with project/jobset info.
activeSteps :: Connection -> IO [ActiveStep]
activeSteps conn = do
  rows <- query_ conn [sql|
    SELECT s.build, s.stepnr, s.system, s.drvpath, s.machine,
           s.starttime, j.project, j.name, b.job, s.busy
    FROM buildsteps s
    JOIN builds b ON s.build = b.id
    JOIN jobsets j ON j.id = b.jobset_id
    WHERE s.busy != 0
    ORDER BY s.machine, s.stepnr
  |]
  pure $ map (\(b, nr, sys, drv, m, st, p, j, job, busy) ->
    ActiveStep b nr sys drv m st p j job busy) rows

-- | Recent finished build steps, paginated.
recentSteps :: Connection -> Int -> Int -> IO [BuildStep]
recentSteps conn offset limit = do
  rows <- query conn [sql|
    SELECT build, stepnr, type, drvpath, busy, status, errormsg,
           starttime, stoptime, machine, system,
           propagatedfrom, overhead, timesbuilt, isnondeterministic
    FROM buildsteps
    WHERE starttime IS NOT NULL AND stoptime IS NOT NULL
    ORDER BY stoptime DESC
    LIMIT ? OFFSET ?
  |] (limit, offset)
  pure $ map scanStepRow rows
  where
    scanStepRow (b, nr, t, drv, busy, st, err, start, stop, machine, sys,
                 prop, overhead, times, nondet) =
      BuildStep b nr t drv busy st err start stop machine sys
                prop overhead times nondet

-- | Recent news items for the overview page.
newsItems :: Connection -> Int -> IO [NewsItem]
newsItems conn limit = do
  rows <- query conn [sql|
    SELECT id, contents, createtime, author
    FROM newsitems
    ORDER BY createtime DESC
    LIMIT ?
  |] (Only limit)
  pure $ map (\(i, c, ct, a) -> NewsItem i c ct a) rows
