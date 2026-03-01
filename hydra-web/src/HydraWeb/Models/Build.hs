-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Build-related data types matching Hydra's PostgreSQL schema.
{-# LANGUAGE DeriveGeneric #-}

module HydraWeb.Models.Build
  ( Build (..)
  , BuildStep (..)
  , BuildOutput (..)
  , BuildProduct (..)
  , BuildMetric (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | A Hydra build record. Fields are nullable where Hydra's schema allows NULL.
data Build = Build
  { buildId            :: !Int
  , buildFinished      :: !Int
  , buildTimestamp      :: !Int
  , buildJob           :: !Text
  , buildSystem        :: !Text
  , buildNixName       :: !(Maybe Text)
  , buildDescription   :: !(Maybe Text)
  , buildDrvPath       :: !Text
  , buildStatus        :: !(Maybe Int)  -- ^ 0=success, 1=failed, 2=dep-failed, etc.
  , buildStartTime     :: !(Maybe Int)
  , buildStopTime      :: !(Maybe Int)
  , buildIsCachedBuild :: !(Maybe Int)
  , buildSize          :: !(Maybe Int)
  , buildClosureSize   :: !(Maybe Int)
  , buildPriority      :: !Int
  , buildGlobalPriority :: !Int
  , buildProject       :: !Text   -- ^ Denormalized from jobsets JOIN
  , buildJobset        :: !Text   -- ^ Denormalized from jobsets JOIN
  } deriving (Show, Eq, Generic)

-- | A single step within a build (build or substitution).
data BuildStep = BuildStep
  { stepBuild     :: !Int
  , stepNr        :: !Int
  , stepType      :: !Int   -- ^ 0=build, 1=substitution
  , stepDrvPath   :: !(Maybe Text)
  , stepBusy      :: !Int   -- ^ 0=not running, 1=running
  , stepStatus    :: !(Maybe Int)
  , stepStartTime :: !(Maybe Int)
  , stepStopTime  :: !(Maybe Int)
  , stepMachine   :: !Text
  } deriving (Show, Eq, Generic)

-- | A named output of a build (e.g. "out", "lib", "dev").
data BuildOutput = BuildOutput
  { boName :: !(Maybe Text)
  , boPath :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

-- | A build product (downloadable artifact).
data BuildProduct = BuildProduct
  { bpProductNr :: !Int
  , bpType      :: !Text
  , bpSubtype   :: !Text
  , bpFileSize  :: !(Maybe Int)
  , bpSha256    :: !(Maybe Text)
  , bpPath      :: !(Maybe Text)
  , bpName      :: !Text
  } deriving (Show, Eq, Generic)

-- | A build metric (timing, size, etc.).
data BuildMetric = BuildMetric
  { bmName      :: !Text
  , bmUnit      :: !(Maybe Text)
  , bmValue     :: !Double
  , bmTimestamp  :: !Int
  } deriving (Show, Eq, Generic)
