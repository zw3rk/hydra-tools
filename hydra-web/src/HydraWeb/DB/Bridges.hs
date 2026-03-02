-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for bridge status (GitHub notifications + Attic uploads).
-- All queries gracefully handle missing bridge tables (SQLSTATE 42P01),
-- returning Nothing when the bridge is not deployed.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.DB.Bridges
  ( bridgeFullStatus
  , gitHubBridgeStatus
  , atticBridgeStatus
  ) where

import Control.Exception (try)
import Database.PostgreSQL.Simple (Connection, query_, SqlError (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import HydraWeb.Models.Bridge

-- | Check if a SQL error is "undefined_table" (42P01).
-- Allows graceful degradation when bridge tables don't exist.
isTableMissing :: SqlError -> Bool
isTableMissing e = sqlState e == "42P01"

-- | Try a query, returning Nothing on table-missing errors.
tryQuery :: IO a -> IO (Maybe a)
tryQuery act = do
  result <- try act
  case result of
    Right a -> pure (Just a)
    Left e | isTableMissing e -> pure Nothing
    Left e                    -> do
      -- Table exists but query failed — re-throw.
      ioError (userError (show e))

-- ── GitHub bridge ────────────────────────────────────────────────────

-- | Summary counts for the GitHub notification queue: (total, sent, pending, failed).
gitHubCounts :: Connection -> IO (Int, Int, Int, Int)
gitHubCounts conn = do
  [(total, sent, pending, failed)] <- query_ conn [sql|
    SELECT count(*)                                             AS total,
           count(*) FILTER (WHERE sent IS NOT NULL)             AS sent,
           count(*) FILTER (WHERE sent IS NULL AND tries < 5)   AS pending,
           count(*) FILTER (WHERE sent IS NULL AND tries >= 5)  AS failed
    FROM github_status_payload
  |]
  pure (total, sent, pending, failed)

-- | Per-owner/repo breakdown of unsent notifications.
gitHubByRepo :: Connection -> IO [GitHubRepoRow]
gitHubByRepo conn = do
  rows <- query_ conn [sql|
    SELECT g.owner, g.repo,
           count(*) FILTER (WHERE p.tries < 5)  AS pending,
           count(*) FILTER (WHERE p.tries >= 5) AS failed
    FROM github_status_payload p
    JOIN github_status g ON g.id = p.status_id
    WHERE p.sent IS NULL
    GROUP BY g.owner, g.repo
    ORDER BY pending DESC, failed DESC
  |]
  pure [GitHubRepoRow o r p f | (o, r, p, f) <- rows]

-- | Last 20 successfully sent notifications.
gitHubRecentSent :: Connection -> IO [GitHubRecentSend]
gitHubRecentSent conn = do
  rows <- query_ conn [sql|
    SELECT g.owner, g.repo, g.name, EXTRACT(EPOCH FROM p.sent)::int
    FROM github_status_payload p
    JOIN github_status g ON g.id = p.status_id
    WHERE p.sent IS NOT NULL
    ORDER BY p.sent DESC
    LIMIT 20
  |]
  pure [GitHubRecentSend o r n s | (o, r, n, s) <- rows]

-- | Full GitHub bridge status, or Nothing if tables don't exist.
gitHubBridgeStatus :: Connection -> IO (Maybe GitHubBridgeStatus)
gitHubBridgeStatus conn = tryQuery $ do
  (total, sent, pending, failed) <- gitHubCounts conn
  byRepo  <- gitHubByRepo conn
  recent  <- gitHubRecentSent conn
  pure GitHubBridgeStatus
    { ghsTotal      = total
    , ghsSent       = sent
    , ghsPending    = pending
    , ghsFailed     = failed
    , ghsByRepo     = byRepo
    , ghsRecentSent = recent
    }

-- ── Attic bridge ─────────────────────────────────────────────────────

-- | 20 most recently active items in the upload queue (tries < 20).
atticRecentActive :: Connection -> IO [AtticQueueItem]
atticRecentActive conn = do
  rows <- query_ conn [sql|
    SELECT id, drvpath, EXTRACT(EPOCH FROM last)::int, tries
    FROM drvpathstoupload
    WHERE tries < 20
    ORDER BY last DESC
    LIMIT 20
  |]
  pure [AtticQueueItem i d l t | (i, d, l, t) <- rows]

-- | Full Attic bridge status, or Nothing if table doesn't exist.
atticBridgeStatus :: Connection -> IO (Maybe AtticBridgeStatus)
atticBridgeStatus conn = tryQuery $ do
  [(total, pending, waiting, failed)] <- query_ conn [sql|
    SELECT count(*)                                              AS total,
           count(*) FILTER (WHERE last < NOW() AND tries < 20)   AS pending,
           count(*) FILTER (WHERE last >= NOW() AND tries < 20)  AS waiting,
           count(*) FILTER (WHERE tries >= 20)                   AS failed
    FROM drvpathstoupload
  |]
  recent <- atticRecentActive conn
  pure AtticBridgeStatus
    { absTotal        = total
    , absPending      = pending
    , absWaiting      = waiting
    , absFailed       = failed
    , absRecentActive = recent
    }

-- ── Combined ─────────────────────────────────────────────────────────

-- | Assemble the complete bridge status for the bridges page.
bridgeFullStatus :: Connection -> IO BridgeStatus
bridgeFullStatus conn = do
  gh    <- gitHubBridgeStatus conn
  attic <- atticBridgeStatus conn
  pure BridgeStatus { bsGitHub = gh, bsAttic = attic }
