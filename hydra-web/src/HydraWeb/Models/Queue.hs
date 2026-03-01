-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Queue and machine status types for the build queue dashboard.
{-# LANGUAGE DeriveGeneric #-}

module HydraWeb.Models.Queue
  ( QueueSummary (..)
  , ActiveStep (..)
  , NavCounts (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Aggregate queue counts per jobset (for queue-summary view).
data QueueSummary = QueueSummary
  { qsProject  :: !Text
  , qsJobset   :: !Text
  , qsNrQueued :: !Int
  } deriving (Show, Eq, Generic)

-- | An actively running build step (for machines/steps view).
data ActiveStep = ActiveStep
  { asBuild     :: !Int
  , asStepNr    :: !Int
  , asDrvPath   :: !(Maybe Text)
  , asMachine   :: !Text
  , asSystem    :: !Text
  , asStartTime :: !(Maybe Int)
  , asJob       :: !Text
  , asProject   :: !Text
  , asJobset    :: !Text
  } deriving (Show, Eq, Generic)

-- | Navigation bar badge counts (queue size, running builds, bridge pending).
data NavCounts = NavCounts
  { ncQueued        :: !Int
  , ncRunning       :: !Int
  , ncBridgePending :: !Int
  } deriving (Show, Eq, Generic)
