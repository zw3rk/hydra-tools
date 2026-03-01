-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Build diff computation: categorizes builds between two evaluations
-- into change categories (now-fail, now-succeed, new, still-fail, etc.).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.BuildDiff
  ( BuildDiff (..)
  , computeBuildDiff
  , defaultCategory
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import HydraWeb.Models.Build (Build (..), isAborted)

-- | Categorized builds between two evaluations.
data BuildDiff = BuildDiff
  { bdNowFail      :: ![Build]  -- ^ Were succeeding, now failing
  , bdNowSucceed   :: ![Build]  -- ^ Were failing, now succeeding
  , bdNew          :: ![Build]  -- ^ New builds (not in previous eval)
  , bdStillFail    :: ![Build]  -- ^ Were failing, still failing
  , bdStillSucceed :: ![Build]  -- ^ Were succeeding, still succeeding
  , bdUnfinished   :: ![Build]  -- ^ Not yet finished (queued)
  , bdAborted      :: ![Build]  -- ^ Terminal failure statuses
  } deriving (Show)

-- | Compute the build diff between current and previous eval's builds.
-- If there's no previous eval, all finished builds go to their
-- status category and unfinished go to bdUnfinished.
computeBuildDiff :: [Build] -> [Build] -> BuildDiff
computeBuildDiff current previous =
  let prevMap :: Map Text Build
      prevMap = Map.fromList [(buildKey b, b) | b <- previous]
      empty   = BuildDiff [] [] [] [] [] [] []
  in  foldr (classify prevMap) empty current

-- | Classify a single build into its diff category.
classify :: Map Text Build -> Build -> BuildDiff -> BuildDiff
classify prevMap b diff
  -- Unfinished (queued).
  | buildFinished b == 0
  = diff { bdUnfinished = b : bdUnfinished diff }
  -- Aborted/cancelled/timed out.
  | isAborted b
  = diff { bdAborted = b : bdAborted diff }
  -- No previous eval: classify by current status.
  | Map.null prevMap
  = case buildStatus b of
      Just 0  -> diff { bdStillSucceed = b : bdStillSucceed diff }
      _       -> diff { bdStillFail = b : bdStillFail diff }
  -- Build exists in previous eval.
  | Just prev <- Map.lookup (buildKey b) prevMap
  = case (buildStatus prev, buildStatus b) of
      (Just 0,  Just 0)  -> diff { bdStillSucceed = b : bdStillSucceed diff }
      (Just 0,  _)       -> diff { bdNowFail = b : bdNowFail diff }
      (_,       Just 0)  -> diff { bdNowSucceed = b : bdNowSucceed diff }
      _                  -> diff { bdStillFail = b : bdStillFail diff }
  -- New build (not in previous eval).
  | otherwise
  = diff { bdNew = b : bdNew diff }

-- | Build key for matching across evals: job + system.
buildKey :: Build -> Text
buildKey b = buildJob b <> "\0" <> buildSystem b

-- | Return the name of the first non-empty category (for default tab).
defaultCategory :: BuildDiff -> Maybe Text
defaultCategory d
  | not (null (bdNowFail d))      = Just "now-fail"
  | not (null (bdStillFail d))    = Just "still-fail"
  | not (null (bdNowSucceed d))   = Just "now-succeed"
  | not (null (bdUnfinished d))   = Just "unfinished"
  | not (null (bdStillSucceed d)) = Just "still-succeed"
  | not (null (bdNew d))          = Just "new"
  | not (null (bdAborted d))      = Just "aborted"
  | otherwise                     = Nothing
