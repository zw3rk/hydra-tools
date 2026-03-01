-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Project and Jobset data types matching Hydra's PostgreSQL schema.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Models.Project
  ( Project (..)
  , Jobset (..)
  , hasErrorMsg
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
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
  -- Declarative jobset settings.
  , projDeclFile    :: !(Maybe Text)
  , projDeclType    :: !(Maybe Text)
  , projDeclValue   :: !(Maybe Text)
  -- Populated by extra queries.
  , projJobsets     :: ![Text]
  } deriving (Show, Eq, Generic)

-- | A Hydra jobset within a project. Build counts are computed
-- from correlated subqueries in the jobsetOverview query.
data Jobset = Jobset
  { jsName             :: !Text
  , jsId               :: !Int
  , jsProject          :: !Text
  , jsDescription      :: !(Maybe Text)
  , jsNixExprInput     :: !(Maybe Text)
  , jsNixExprPath      :: !(Maybe Text)
  , jsErrorMsg         :: !(Maybe Text)
  , jsErrorTime        :: !(Maybe Int)
  , jsLastCheckedTime  :: !(Maybe Int)
  , jsTriggerTime      :: !(Maybe Int)
  , jsEnabled          :: !Int      -- ^ 0=disabled, 1=enabled, 2=one-shot, 3=one-at-a-time
  , jsEnableEmail      :: !Int
  , jsHidden           :: !Int
  , jsEmailOverride    :: !Text
  , jsKeepNr           :: !Int
  , jsCheckInterval    :: !Int
  , jsSchedulingShares :: !Int
  , jsFetchErrorMsg    :: !(Maybe Text)
  , jsStartTime        :: !(Maybe Int)
  , jsType             :: !Int      -- ^ 0=legacy, 1=flake
  , jsFlake            :: !(Maybe Text)
  -- Computed from subqueries.
  , jsNrScheduled      :: !Int
  , jsNrFailed         :: !Int
  , jsNrSucceeded      :: !Int
  , jsNrTotal          :: !Int
  } deriving (Show, Eq, Generic)

-- | True if the jobset has a non-empty error or fetch-error message.
hasErrorMsg :: Jobset -> Bool
hasErrorMsg j =
  nonEmpty (jsErrorMsg j) || nonEmpty (jsFetchErrorMsg j)
  where
    nonEmpty Nothing  = False
    nonEmpty (Just t) = not (Text.null t)
