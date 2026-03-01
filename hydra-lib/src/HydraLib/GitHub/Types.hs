-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | GitHub API types shared across hydra-tools components.
-- Primarily check-run related types for CI status reporting.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraLib.GitHub.Types
  ( CheckRunStatus (..)
  , CheckRunConclusion (..)
  , CheckRunOutput (..)
  , CheckRunPayload (..)
  , CheckRun (..)
  , TokenLease (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GitHub.REST (Token)

-- | Status of a GitHub check-run.
data CheckRunStatus = Queued | InProgress | Completed
  deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRunStatus where
  toJSON = \case
    Queued     -> "queued"
    InProgress -> "in_progress"
    Completed  -> "completed"

instance FromJSON CheckRunStatus where
  parseJSON = \case
    "queued"      -> return Queued
    "in_progress" -> return InProgress
    "completed"   -> return Completed
    _             -> fail "Invalid CheckRunStatus"

-- | Conclusion of a completed GitHub check-run.
data CheckRunConclusion
  = ActionRequired | Cancelled | Failure | Neutral | Success
  | Skipped | Stale | TimedOut
  deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRunConclusion where
  toJSON = \case
    ActionRequired -> "action_required"
    Cancelled      -> "cancelled"
    Failure        -> "failure"
    Neutral        -> "neutral"
    Success        -> "success"
    Skipped        -> "skipped"
    Stale          -> "stale"
    TimedOut       -> "timed_out"

instance FromJSON CheckRunConclusion where
  parseJSON = \case
    "action_required" -> return ActionRequired
    "cancelled"       -> return Cancelled
    "failure"         -> return Failure
    "neutral"         -> return Neutral
    "success"         -> return Success
    "skipped"         -> return Skipped
    "stale"           -> return Stale
    "timed_out"       -> return TimedOut
    _                 -> fail "Invalid CheckRunConclusion"

-- | Output attached to a check-run (title, summary, and optional full text).
data CheckRunOutput = CheckRunOutput
  { title   :: Text
  , summary :: Text
  , text    :: Maybe Text
  } deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRunOutput where
  toJSON = Aeson.genericToJSON $ aesonDrop 0 camelCase
instance FromJSON CheckRunOutput where
  parseJSON = Aeson.genericParseJSON $ aesonDrop 0 camelCase

-- | Payload for creating or updating a GitHub check-run.
data CheckRunPayload = CheckRunPayload
  { name        :: Text
  , headSha     :: Text
  , detailsUrl  :: Maybe Text
  , externalId  :: Maybe Text
  , status      :: CheckRunStatus
  , conclusion  :: Maybe CheckRunConclusion
  , startedAt   :: Maybe UTCTime
  , completedAt :: Maybe UTCTime
  , output      :: Maybe CheckRunOutput
  } deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRunPayload where
  toJSON = Aeson.genericToJSON $ aesonDrop 0 camelCase
instance FromJSON CheckRunPayload where
  parseJSON = Aeson.genericParseJSON $ aesonDrop 0 camelCase

-- | A check-run associated with a GitHub repository.
data CheckRun = CheckRun
  { owner   :: Text
  , repo    :: Text
  , payload :: CheckRunPayload
  } deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRun where
  toJSON = Aeson.genericToJSON $ aesonDrop 0 camelCase
instance FromJSON CheckRun where
  parseJSON = Aeson.genericParseJSON $ aesonDrop 0 camelCase

-- | A GitHub installation access token with optional expiry.
data TokenLease = TokenLease
  { token  :: Token
  , expiry :: Maybe UTCTime
  } deriving (Show)
