-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Core Hydra types shared across hydra-tools components.
-- Includes build status codes, notification types, and API data types
-- for talking to the Hydra CI server.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraLib.Hydra.Types
  ( -- * ID type aliases
    JobSetId
  , EvalId
  , BuildId

    -- * Notifications (PostgreSQL LISTEN/NOTIFY)
  , Notification (..)

    -- * Build status
  , BuildStatus (..)

    -- * Hydra API request/response types
  , HydraLogin (..)
  , HydraPush (..)
  , HydraProject (..)
  , HydraJobset (..)
  , HydraJobsetResp (..)
  , defHydraProject
  , defHydraJobset
  , defHydraFlakeJobset
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Text (Text)
import GHC.Generics (Generic)

type JobSetId = Int
type EvalId = Int
type BuildId = Int

-- | Hydra PostgreSQL LISTEN/NOTIFY notification types.
data Notification
  = EvalStarted JobSetId
  | EvalAdded JobSetId EvalId
  | EvalCached JobSetId EvalId
  | EvalFailed JobSetId
  | BuildQueued BuildId
  | BuildStarted BuildId
  | BuildFinished BuildId [BuildId]
  deriving (Show, Eq)

-- | Build outcome status codes, matching Hydra's integer encoding.
data BuildStatus
  = Succeeded           -- ^ 0
  | Failed              -- ^ 1
  | DependencyFailed    -- ^ 2
  | Aborted             -- ^ 3, 9
  | Cancelled           -- ^ 4
  | FailedWithOutput    -- ^ 6
  | TimedOut            -- ^ 7
  | LogLimitExceeded    -- ^ 10
  | OutputSizeLimitExceeded -- ^ 11
  | NonDeterministicBuild   -- ^ 12
  | Other
  deriving (Eq)

instance Show BuildStatus where
  show = \case
    Succeeded -> "Build succeeded"
    Failed -> "Build failed"
    DependencyFailed -> "Build dependency failed"
    Aborted -> "Build aborted"
    Cancelled -> "Build cancelled"
    FailedWithOutput -> "Build failed with output"
    TimedOut -> "Build timed out"
    LogLimitExceeded -> "Build log limit exceeded"
    OutputSizeLimitExceeded -> "Build output size limit exceeded"
    NonDeterministicBuild -> "Build is non-deterministic"
    Other -> "Build failed due to unknown reason"

instance Enum BuildStatus where
  toEnum = \case
    0  -> Succeeded
    1  -> Failed
    2  -> DependencyFailed
    3  -> Aborted
    9  -> Aborted
    4  -> Cancelled
    6  -> FailedWithOutput
    7  -> TimedOut
    10 -> LogLimitExceeded
    11 -> OutputSizeLimitExceeded
    12 -> NonDeterministicBuild
    _  -> Other
  fromEnum = \case
    Succeeded -> 0
    Failed -> 1
    DependencyFailed -> 2
    Aborted -> 3
    Cancelled -> 4
    FailedWithOutput -> 6
    TimedOut -> 7
    LogLimitExceeded -> 10
    OutputSizeLimitExceeded -> 11
    NonDeterministicBuild -> 12
    Other -> 99

data HydraLogin = HydraLogin
  { lUsername :: Text
  , lPassword :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON HydraLogin where
  toJSON = genericToJSON $ aesonDrop 1 camelCase
instance FromJSON HydraLogin where
  parseJSON = genericParseJSON $ aesonDrop 1 camelCase

data HydraPush = HydraPush
  { pJobsets :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON HydraPush where
  toJSON = genericToJSON $ aesonDrop 1 camelCase
instance FromJSON HydraPush where
  parseJSON = genericParseJSON $ aesonDrop 1 camelCase

data HydraProject = HydraProject
  { hpName        :: Text
  , hpDisplayname :: Text
  , hpDescription :: Text
  , hpHomepage    :: Text
  , hpOwner       :: Text
  , hpEnabled     :: Bool
  , hpVisible     :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON HydraProject where
  toJSON = genericToJSON $ aesonDrop 2 camelCase
instance FromJSON HydraProject where
  parseJSON = genericParseJSON $ aesonDrop 2 camelCase

data HydraJobset = HydraJobset
  { hjName             :: Text
  , hjDescription      :: Text
  , hjEnabled          :: Int
  , hjEnableemail      :: Bool
  , hjVisible          :: Bool
  , hjEmailoverride    :: Text
  , hjKeepnr           :: Int
  , hjCheckinterval    :: Int
  , hjSchedulingshares :: Int
  , hjType             :: Int
  , hjFlake            :: Text
  } deriving (Eq, Generic, Read, Show)

instance ToJSON HydraJobset where
  toJSON = genericToJSON $ aesonDrop 2 camelCase
instance FromJSON HydraJobset where
  parseJSON = genericParseJSON $ aesonDrop 2 camelCase

data HydraJobsetResp = HydraJobsetResp
  { respRedirect :: Text
  , respUri      :: Maybe Text
  , respName     :: Maybe Text
  , respType     :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON HydraJobsetResp where
  toJSON = genericToJSON $ aesonDrop 4 camelCase
instance FromJSON HydraJobsetResp where
  parseJSON = genericParseJSON $ aesonDrop 4 camelCase

defHydraProject :: HydraProject
defHydraProject = HydraProject
  { hpEnabled     = True
  , hpVisible     = True
  , hpOwner       = "bridge"
  , hpName        = ""
  , hpDisplayname = ""
  , hpDescription = ""
  , hpHomepage    = ""
  }

defHydraJobset :: HydraJobset
defHydraJobset = HydraJobset
  { hjEnabled          = 1
  , hjVisible          = True
  , hjKeepnr           = 2
  , hjSchedulingshares = 42
  , hjCheckinterval    = 0
  , hjEnableemail      = False
  , hjEmailoverride    = ""
  , hjType             = 0
  , hjName             = ""
  , hjDescription      = ""
  , hjFlake            = ""
  }

defHydraFlakeJobset :: HydraJobset
defHydraFlakeJobset = defHydraJobset { hjType = 1 }
