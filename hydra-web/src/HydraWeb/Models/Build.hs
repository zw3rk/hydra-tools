-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Build-related data types matching Hydra's PostgreSQL schema.
-- Covers builds, build steps, outputs, products, metrics, and inputs.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Models.Build
  ( Build (..)
  , BuildStep (..)
  , BuildOutput (..)
  , BuildProduct (..)
  , BuildMetric (..)
  , BuildInput (..)
  , buildStatusText
  , stepStatusText
  , isAborted
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | A Hydra build record. Fields match the "builds" table plus
-- denormalized project/jobset from the "jobsets" table via JOIN.
data Build = Build
  { buildId             :: !Int
  , buildFinished       :: !Int
  , buildTimestamp      :: !Int
  , buildJobsetId       :: !Int
  , buildJob            :: !Text
  , buildNixName        :: !(Maybe Text)
  , buildSystem         :: !Text
  , buildPriority       :: !Int
  , buildGlobalPriority :: !Int
  , buildStartTime      :: !(Maybe Int)
  , buildStopTime       :: !(Maybe Int)
  , buildIsCachedBuild  :: !(Maybe Int)
  , buildStatus         :: !(Maybe Int)   -- ^ 0=success, 1=failed, etc.
  , buildDrvPath        :: !Text
  , buildIsCurrent      :: !(Maybe Int)
  , buildProject        :: !Text          -- ^ Denormalized from jobsets JOIN
  , buildJobset         :: !Text          -- ^ Denormalized from jobsets JOIN
  -- Extended fields loaded only by getBuild (single-build detail view).
  , buildDescription    :: !(Maybe Text)
  , buildLicense        :: !(Maybe Text)
  , buildHomepage       :: !(Maybe Text)
  , buildMaintainers    :: !(Maybe Text)
  , buildSize           :: !(Maybe Int)
  , buildClosureSize    :: !(Maybe Int)
  , buildReleaseName    :: !(Maybe Text)
  , buildKeep           :: !Int
  } deriving (Show, Eq, Generic)

-- | Human-readable build status string.
buildStatusText :: Build -> Text
buildStatusText b
  | buildFinished b == 0 = "Queued"
  | otherwise = case buildStatus b of
      Nothing  -> "Unknown"
      Just 0   -> "Succeeded"
      Just 1   -> "Failed"
      Just 2   -> "Dependency failed"
      Just 3   -> "Aborted"
      Just 4   -> "Cancelled"
      Just 6   -> "Failed with output"
      Just 7   -> "Timed out"
      Just 9   -> "Unsupported system"
      Just 10  -> "Log limit exceeded"
      Just 11  -> "Output size exceeded"
      Just 12  -> "Non-deterministic"
      _        -> "Failed"

-- | True for terminal failure statuses (abort, cancel, timeout, etc.).
isAborted :: Build -> Bool
isAborted b = case buildStatus b of
  Just s  -> s `elem` [3, 4, 7, 9, 10, 11 :: Int]
  Nothing -> False

-- | A single build step (build or substitution). Matches "buildsteps" table.
data BuildStep = BuildStep
  { stepBuild              :: !Int
  , stepNr                 :: !Int
  , stepType               :: !Int        -- ^ 0=build, 1=substitution
  , stepDrvPath            :: !(Maybe Text)
  , stepBusy               :: !Int        -- ^ 0=not running, 1=running
  , stepStatus             :: !(Maybe Int)
  , stepErrorMsg           :: !(Maybe Text)
  , stepStartTime          :: !(Maybe Int)
  , stepStopTime           :: !(Maybe Int)
  , stepMachine            :: !Text
  , stepSystem             :: !(Maybe Text)
  , stepPropagatedFrom     :: !(Maybe Int)
  , stepOverhead           :: !(Maybe Int)
  , stepTimesBuilt         :: !(Maybe Int)
  , stepIsNonDeterministic :: !(Maybe Bool)
  } deriving (Show, Eq, Generic)

-- | Human-readable step status string.
stepStatusText :: BuildStep -> Text
stepStatusText s
  | stepBusy s /= 0, Nothing <- stepStatus s = "Building"
  | Nothing <- stepStatus s = "Pending"
  | Just 0  <- stepStatus s = "Succeeded"
  | Just 1  <- stepStatus s = "Failed"
  | Just 3  <- stepStatus s = "Aborted"
  | Just 4  <- stepStatus s = "Cancelled"
  | Just 7  <- stepStatus s = "Timed out"
  | Just 8  <- stepStatus s = "Cached failure"
  | Just 9  <- stepStatus s = "Unsupported system"
  | Just 10 <- stepStatus s = "Log limit exceeded"
  | Just 11 <- stepStatus s = "Output limit exceeded"
  | Just 12 <- stepStatus s = "Non-deterministic"
  | otherwise = "Failed"

-- | A named output of a build (e.g. "out", "lib", "dev").
data BuildOutput = BuildOutput
  { boName :: !Text
  , boPath :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

-- | A build product (downloadable artifact).
data BuildProduct = BuildProduct
  { bpProductNr   :: !Int
  , bpType        :: !Text
  , bpSubtype     :: !Text
  , bpFileSize    :: !(Maybe Int)
  , bpSha256      :: !(Maybe Text)
  , bpPath        :: !(Maybe Text)
  , bpName        :: !Text
  , bpDefaultPath :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

-- | A build metric (timing, size, etc.).
data BuildMetric = BuildMetric
  { bmName  :: !Text
  , bmUnit  :: !(Maybe Text)
  , bmValue :: !Double
  } deriving (Show, Eq, Generic)

-- | An input to a build.
data BuildInput = BuildInput
  { biId               :: !Int
  , biName             :: !Text
  , biType             :: !Text
  , biUri              :: !(Maybe Text)
  , biRevision         :: !(Maybe Text)
  , biValue            :: !(Maybe Text)
  , biEmailResponsible :: !Int
  , biDependency       :: !(Maybe Int)
  , biPath             :: !(Maybe Text)
  , biSha256           :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
