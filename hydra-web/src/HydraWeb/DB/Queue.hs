-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for queue status, running builds, and navigation counts.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.DB.Queue
  ( queueCount
  , runningCount
  , activeStepCount
  , navCounts
  , queueSummary
  , systemQueueSummary
  , activeSteps
  , recentSteps
  , newsItems
  ) where

import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))

import HydraWeb.Models.Queue
import HydraWeb.DB.Builds (scanStepRow)
import HydraWeb.Models.Build (BuildStep)

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

-- | Batched navigation counts: queue size, running steps, bridge pending, running evals.
navCounts :: Connection -> IO NavCounts
navCounts conn = do
  queued  <- queueCount conn
  running <- runningCount conn
  pending <- bridgePending conn
  evals   <- runningEvalsCount' conn
  pure NavCounts
    { ncQueued        = queued
    , ncRunning       = running
    , ncBridgePending = pending
    , ncRunningEvals  = evals
    }

-- | Count of currently-running evaluations (jobsets with starttime IS NOT NULL).
runningEvalsCount' :: Connection -> IO Int
runningEvalsCount' conn = do
  [Only n] <- query_ conn [sql|
    SELECT count(*) FROM jobsets WHERE starttime IS NOT NULL
  |]
  pure n

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
-- Excludes builds from hidden projects/jobsets.
queueSummary :: Connection -> IO [QueueSummary]
queueSummary conn = do
  rows <- query_ conn [sql|
    SELECT j.project, j.name, count(*) AS queued,
           min(b.timestamp) AS oldest, max(b.timestamp) AS newest
    FROM builds b
    JOIN jobsets j ON j.id = b.jobset_id
    JOIN projects p ON p.name = j.project
    WHERE b.finished = 0
      AND j.hidden = 0 AND p.hidden = 0
    GROUP BY j.project, j.name
    ORDER BY queued DESC
  |]
  pure $ map (\(p, j, q, o, n) -> QueueSummary p j q (fromMaybe 0 o) (fromMaybe 0 n)) rows

-- | Queue grouped by system type.
-- Excludes builds from hidden projects/jobsets.
systemQueueSummary :: Connection -> IO [SystemQueueRow]
systemQueueSummary conn = do
  rows <- query_ conn [sql|
    SELECT b.system, count(*) AS c
    FROM builds b
    JOIN jobsets j ON j.id = b.jobset_id
    JOIN projects p ON p.name = j.project
    WHERE b.finished = 0
      AND j.hidden = 0 AND p.hidden = 0
    GROUP BY b.system
    ORDER BY c DESC
  |]
  pure $ map (\(s, c) -> SystemQueueRow s c) rows

-- | Currently-running build steps with project/jobset info.
-- Excludes steps from hidden projects/jobsets.
activeSteps :: Connection -> IO [ActiveStep]
activeSteps conn = do
  rows <- query_ conn [sql|
    SELECT s.build, s.stepnr, s.system, s.drvpath, s.machine,
           s.starttime, j.project, j.name, b.job, s.busy
    FROM buildsteps s
    JOIN builds b ON s.build = b.id
    JOIN jobsets j ON j.id = b.jobset_id
    JOIN projects p ON p.name = j.project
    WHERE s.busy != 0
      AND j.hidden = 0 AND p.hidden = 0
    ORDER BY s.machine, s.stepnr
  |]
  pure $ map (\(b, nr, sys, drv, m, st, p, j, job, busy) ->
    ActiveStep b nr sys drv m st p j job busy) rows

-- | Count of currently-running build steps (for SSE fragments).
activeStepCount :: Connection -> IO Int
activeStepCount conn = do
  [Only n] <- query_ conn [sql|
    SELECT count(*)
    FROM buildsteps
    WHERE busy != 0
  |]
  pure n

-- | Recent finished build steps, paginated.
-- Excludes steps from hidden projects/jobsets.
recentSteps :: Connection -> Int -> Int -> IO [BuildStep]
recentSteps conn offset limit = do
  rows <- query conn [sql|
    SELECT s.build, s.stepnr, s.type, s.drvpath, s.busy, s.status, s.errormsg,
           s.starttime, s.stoptime, s.machine, s.system,
           s.propagatedfrom, s.overhead, s.timesbuilt, s.isnondeterministic
    FROM buildsteps s
    JOIN builds b ON s.build = b.id
    JOIN jobsets j ON j.id = b.jobset_id
    JOIN projects p ON p.name = j.project
    WHERE s.starttime IS NOT NULL AND s.stoptime IS NOT NULL
      AND j.hidden = 0 AND p.hidden = 0
    ORDER BY s.stoptime DESC
    LIMIT ? OFFSET ?
  |] (limit, offset)
  pure $ map scanStepRow rows

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
