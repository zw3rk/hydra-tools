-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
{-# LANGUAGE OverloadedStrings #-}

-- | Bridge status data types. Both GitHub notification delivery and
-- Attic binary-cache upload status. Fields are 'Nothing' when the
-- corresponding bridge tables don't exist in the database (bridge
-- not deployed).
module HydraWeb.Models.Bridge
  ( BridgeStatus (..),
    GitHubBridgeStatus (..),
    GitHubRepoRow (..),
    GitHubRecentSend (..),
    AtticBridgeStatus (..),
    AtticQueueItem (..),
  )
where

import Data.Text (Text)

-- | Combined operational status for all bridges.
data BridgeStatus = BridgeStatus
  { -- | Nothing if tables missing
    bsGitHub :: !(Maybe GitHubBridgeStatus),
    -- | Nothing if tables missing
    bsAttic :: !(Maybe AtticBridgeStatus)
  }

-- | GitHub check-run notification delivery status.
data GitHubBridgeStatus = GitHubBridgeStatus
  { -- | All rows in github_status_payload
    ghsTotal :: !Int,
    -- | sent IS NOT NULL
    ghsSent :: !Int,
    -- | sent IS NULL AND tries < 5
    ghsPending :: !Int,
    -- | sent IS NULL AND tries >= 5
    ghsFailed :: !Int,
    -- | Breakdown by owner/repo
    ghsByRepo :: ![GitHubRepoRow],
    -- | Last 20 successfully sent
    ghsRecentSent :: ![GitHubRecentSend]
  }

-- | Per-repository breakdown of unsent notifications.
data GitHubRepoRow = GitHubRepoRow
  { grrOwner :: !Text,
    grrRepo :: !Text,
    grrPending :: !Int,
    grrFailed :: !Int
  }

-- | A recently delivered GitHub notification.
data GitHubRecentSend = GitHubRecentSend
  { grsOwner :: !Text,
    grsRepo :: !Text,
    grsName :: !Text,
    -- | Unix timestamp
    grsSent :: !Int
  }

-- | Attic binary-cache upload queue status.
-- Note: successfully uploaded paths are deleted from the table, so all
-- rows represent incomplete work.
data AtticBridgeStatus = AtticBridgeStatus
  { -- | All rows in drvpathstoupload
    absTotal :: !Int,
    -- | last < NOW() AND tries < 20 (eligible for retry)
    absPending :: !Int,
    -- | last >= NOW() AND tries < 20 (in backoff)
    absWaiting :: !Int,
    -- | tries >= 20 (exhausted retries)
    absFailed :: !Int,
    -- | 20 most recently active items
    absRecentActive :: ![AtticQueueItem]
  }

-- | An item in the Attic upload queue.
data AtticQueueItem = AtticQueueItem
  { aqiId :: !Int,
    aqiDrvPath :: !Text,
    -- | Unix timestamp of last attempt
    aqiLast :: !Int,
    aqiTries :: !Int
  }
