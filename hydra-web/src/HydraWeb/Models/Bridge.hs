-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Bridge status data types. Both GitHub notification delivery and
-- Attic binary-cache upload status. Fields are 'Nothing' when the
-- corresponding bridge tables don't exist in the database (bridge
-- not deployed).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Models.Bridge
  ( BridgeStatus (..)
  , GitHubBridgeStatus (..)
  , GitHubRepoRow (..)
  , GitHubRecentSend (..)
  , AtticBridgeStatus (..)
  , AtticQueueItem (..)
  ) where

import Data.Text (Text)

-- | Combined operational status for all bridges.
data BridgeStatus = BridgeStatus
  { bsGitHub :: !(Maybe GitHubBridgeStatus)  -- ^ Nothing if tables missing
  , bsAttic  :: !(Maybe AtticBridgeStatus)   -- ^ Nothing if tables missing
  }

-- | GitHub check-run notification delivery status.
data GitHubBridgeStatus = GitHubBridgeStatus
  { ghsTotal      :: !Int                -- ^ All rows in github_status_payload
  , ghsSent       :: !Int                -- ^ sent IS NOT NULL
  , ghsPending    :: !Int                -- ^ sent IS NULL AND tries < 5
  , ghsFailed     :: !Int                -- ^ sent IS NULL AND tries >= 5
  , ghsByRepo     :: ![GitHubRepoRow]    -- ^ Breakdown by owner/repo
  , ghsRecentSent :: ![GitHubRecentSend] -- ^ Last 20 successfully sent
  }

-- | Per-repository breakdown of unsent notifications.
data GitHubRepoRow = GitHubRepoRow
  { grrOwner   :: !Text
  , grrRepo    :: !Text
  , grrPending :: !Int
  , grrFailed  :: !Int
  }

-- | A recently delivered GitHub notification.
data GitHubRecentSend = GitHubRecentSend
  { grsOwner :: !Text
  , grsRepo  :: !Text
  , grsName  :: !Text
  , grsSent  :: !Int  -- ^ Unix timestamp
  }

-- | Attic binary-cache upload queue status.
-- Note: successfully uploaded paths are deleted from the table, so all
-- rows represent incomplete work.
data AtticBridgeStatus = AtticBridgeStatus
  { absTotal        :: !Int              -- ^ All rows in drvpathstoupload
  , absPending      :: !Int              -- ^ last < NOW() AND tries < 20 (eligible for retry)
  , absWaiting      :: !Int              -- ^ last >= NOW() AND tries < 20 (in backoff)
  , absFailed       :: !Int              -- ^ tries >= 20 (exhausted retries)
  , absRecentActive :: ![AtticQueueItem] -- ^ 20 most recently active items
  }

-- | An item in the Attic upload queue.
data AtticQueueItem = AtticQueueItem
  { aqiId      :: !Int
  , aqiDrvPath :: !Text
  , aqiLast    :: !Int   -- ^ Unix timestamp of last attempt
  , aqiTries   :: !Int
  }
