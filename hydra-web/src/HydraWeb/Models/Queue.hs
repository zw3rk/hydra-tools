-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Queue and machine status types for the build queue dashboard.
{-# LANGUAGE DeriveGeneric #-}

module HydraWeb.Models.Queue
  ( QueueSummary (..)
  , SystemQueueRow (..)
  , ActiveStep (..)
  , NavCounts (..)
  , NewsItem (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Aggregate queue counts per jobset (for queue-summary view).
data QueueSummary = QueueSummary
  { qsProject :: !Text
  , qsJobset  :: !Text
  , qsQueued  :: !Int
  , qsOldest  :: !Int  -- ^ Min timestamp of queued builds
  , qsNewest  :: !Int  -- ^ Max timestamp of queued builds
  } deriving (Show, Eq, Generic)

-- | Queue counts grouped by system type.
data SystemQueueRow = SystemQueueRow
  { sqSystem :: !Text
  , sqCount  :: !Int
  } deriving (Show, Eq, Generic)

-- | An actively running build step (for machines/steps view).
data ActiveStep = ActiveStep
  { asBuild     :: !Int
  , asStepNr    :: !Int
  , asSystem    :: !Text
  , asDrvPath   :: !(Maybe Text)
  , asMachine   :: !Text
  , asStartTime :: !(Maybe Int)
  , asProject   :: !Text
  , asJobset    :: !Text
  , asJob       :: !Text
  , asBusy      :: !Int
  } deriving (Show, Eq, Generic)

-- | Navigation bar badge counts (queue size, running builds, bridge pending, running evals).
data NavCounts = NavCounts
  { ncQueued        :: !Int
  , ncRunning       :: !Int
  , ncBridgePending :: !Int
  , ncRunningEvals  :: !Int
  } deriving (Show, Eq, Generic)

-- | A news item for the overview page.
data NewsItem = NewsItem
  { niId         :: !Int
  , niContents   :: !Text
  , niCreateTime :: !Int
  , niAuthor     :: !Text
  } deriving (Show, Eq, Generic)
