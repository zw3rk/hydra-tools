-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Project and Jobset data types matching Hydra's PostgreSQL schema.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Models.Project
  ( Project (..)
  , Jobset (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | A Hydra project (top-level grouping of jobsets).
data Project = Project
  { projName        :: !Text
  , projDisplayName :: !Text
  , projDescription :: !(Maybe Text)
  , projEnabled     :: !Int
  , projHidden      :: !Int
  , projOwner       :: !(Maybe Text)
  , projHomepage    :: !(Maybe Text)
  , projJobsets     :: ![Text]  -- ^ Populated by JOIN with jobsets table
  } deriving (Show, Eq, Generic)

-- | A Hydra jobset within a project.
data Jobset = Jobset
  { jsName            :: !Text
  , jsId              :: !Int
  , jsProject         :: !Text
  , jsDescription     :: !(Maybe Text)
  , jsNixExprInput    :: !(Maybe Text)
  , jsNixExprPath     :: !(Maybe Text)
  , jsErrorMsg        :: !(Maybe Text)
  , jsFetchErrorMsg   :: !(Maybe Text)
  , jsErrorTime       :: !(Maybe Int)
  , jsLastCheckedTime :: !(Maybe Int)
  , jsTriggerTime     :: !(Maybe Int)
  , jsStartTime       :: !(Maybe Int)
  , jsEnabled         :: !Int      -- ^ 0=disabled, 1=enabled, 2=one-shot, 3=one-at-a-time
  , jsType            :: !Int      -- ^ 0=legacy, 1=flake
  , jsFlake           :: !(Maybe Text)
  , jsNrScheduled     :: !Int      -- ^ Computed from builds
  , jsNrFailed        :: !Int
  , jsNrSucceeded     :: !Int
  , jsNrTotal         :: !Int
  } deriving (Show, Eq, Generic)
