{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Hydra.Client
  ( HydraClientEnv (..),
    JobSetId,
    EvalId,
    BuildId,
    Notification (..),
    BuildStatus (..),
    HydraLogin (..),
    HydraPush (..),
    HydraProject (..),
    HydraJobset (..),
    HydraJobsetResp (..),
    HydraAPI,
    defHydraProject,
    defHydraJobset,
    defHydraFlakeJobset,
    mkProject,
    mkJobset,
    getJobset,
    rmJobset,
    login,
    push,
    restartBuild,
    isAuthError,
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.Types (Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status (..))
import Servant
import Servant.Client (ClientEnv, ClientError (..), ClientM, ResponseF (..), client)

-- Hydra client environment that includes credentials for re-authentication
data HydraClientEnv = HydraClientEnv
  { hceHost :: Text,
    hceUser :: Text,
    hcePass :: Text,
    hceClientEnv :: ClientEnv
  }

type JobSetId = Int

type EvalId = Int

type BuildId = Int

data Notification
  = EvalStarted JobSetId
  | EvalAdded JobSetId EvalId
  | EvalCached JobSetId EvalId
  | EvalFailed JobSetId
  | BuildQueued BuildId
  | BuildStarted BuildId
  | BuildFinished BuildId [BuildId]
  deriving (Show, Eq)

data BuildStatus
  = Succeeded
  | Failed
  | DependencyFailed
  | Aborted
  | Cancelled
  | FailedWithOutput
  | TimedOut
  | LogLimitExceeded
  | OutputSizeLimitExceeded
  | NonDeterministicBuild
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
    (0) -> Succeeded
    (1) -> Failed
    (2) -> DependencyFailed
    (3) -> Aborted
    (9) -> Aborted
    (4) -> Cancelled
    (6) -> FailedWithOutput
    (7) -> TimedOut
    (10) -> LogLimitExceeded
    (11) -> OutputSizeLimitExceeded
    (12) -> NonDeterministicBuild
    (_) -> Other
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
  { lUsername :: Text,
    lPassword :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON HydraLogin where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

instance FromJSON HydraLogin where
  parseJSON = genericParseJSON $ aesonDrop 1 camelCase

data HydraPush = HydraPush
  { pJobsets :: Text -- formatted as project:jobset (comma separated)
  }
  deriving (Show, Eq, Generic)

instance ToJSON HydraPush where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

instance FromJSON HydraPush where
  parseJSON = genericParseJSON $ aesonDrop 1 camelCase

data HydraProject = HydraProject
  { hpName :: Text,
    hpDisplayname :: Text,
    hpDescription :: Text,
    hpHomepage :: Text,
    hpOwner :: Text,
    hpEnabled :: Bool,
    hpVisible :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON HydraProject where
  toJSON = genericToJSON $ aesonDrop 2 camelCase

instance FromJSON HydraProject where
  parseJSON = genericParseJSON $ aesonDrop 2 camelCase

data HydraJobset = HydraJobset
  { hjName :: Text,
    hjDescription :: Text,
    -- nixexprinput
    -- nixexprpath
    -- errormsg
    -- errortime
    -- lastcheckedtime
    -- triggertime
    --
    hjEnabled :: Int,
    hjEnableemail :: Bool,
    -- enable_dynamic_run_command
    hjVisible :: Bool,
    hjEmailoverride :: Text,
    hjKeepnr :: Int,
    hjCheckinterval :: Int,
    hjSchedulingshares :: Int,
    -- fetcherrormsg
    -- startime
    hjType :: Int,
    hjFlake :: Text
    -- inputs
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON HydraJobset where
  toJSON = genericToJSON $ aesonDrop 2 camelCase

instance FromJSON HydraJobset where
  parseJSON = genericParseJSON $ aesonDrop 2 camelCase

data HydraJobsetResp = HydraJobsetResp
  { respRedirect :: Text,
    respUri :: Maybe Text,
    respName :: Maybe Text,
    respType :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON HydraJobsetResp where
  toJSON = genericToJSON $ aesonDrop 4 camelCase

instance FromJSON HydraJobsetResp where
  parseJSON = genericParseJSON $ aesonDrop 4 camelCase

type HydraAPI =
  "project"
    :> Capture "project-id" Text
    :> ReqBody '[JSON] HydraProject
    -- allow 200 (update) and 201 (created) responses.
    :> UVerb 'PUT '[JSON] '[WithStatus 200 Object, WithStatus 201 Object]
    :<|> "jobset"
      :> Capture "project-id" Text
      :> Capture "jobset-id" Text
      :> ReqBody '[JSON] HydraJobset
      -- allow 200 (update) and 201 (created) responses.
      :> UVerb 'PUT '[JSON] '[WithStatus 200 Object, WithStatus 201 Object]
    :<|> "jobset"
      :> Capture "project-id" Text
      :> Capture "jobset-id" Text
      :> Get '[JSON] Value
    :<|> "jobset"
      :> Capture "project-id" Text
      :> Capture "jobset-id" Text
      :> Delete '[JSON] Value
    :<|> "login"
      :> Header "Origin" Text
      :> ReqBody '[JSON] HydraLogin
      :> Post '[JSON] Value
    :<|> "api"
      :> "push"
      :> Header "Origin" Text
      :> QueryParam "jobsets" Text
      :> QueryParam "force" Bool
      :> Post '[JSON] Value
    -- Not actually part of the API but this is what the restart button does.
    :<|> "build"
      :> Capture "build-id" Int
      :> "restart"
      -- Responds with a redirect, just ignore that.
      :> Get '[] NoContent

defHydraProject :: HydraProject
defHydraProject =
  HydraProject
    { hpEnabled = True,
      hpVisible = True,
      hpOwner = "bridge",
      hpName = "",
      hpDisplayname = "",
      hpDescription = "",
      hpHomepage = ""
    }

defHydraJobset :: HydraJobset
defHydraJobset =
  HydraJobset
    { hjEnabled = 1,
      hjVisible = True,
      hjKeepnr = 2,
      hjSchedulingshares = 42,
      hjCheckinterval = 0,
      hjEnableemail = False,
      hjEmailoverride = "",
      hjType = 0,
      hjName = "",
      hjDescription = "",
      hjFlake = ""
    }

defHydraFlakeJobset :: HydraJobset
defHydraFlakeJobset = defHydraJobset {hjType = 1}

mkProject :: Text -> HydraProject -> ClientM (Union '[WithStatus 200 Object, WithStatus 201 Object])
mkJobset :: Text -> Text -> HydraJobset -> ClientM (Union '[WithStatus 200 Object, WithStatus 201 Object])
getJobset :: Text -> Text -> ClientM Value
rmJobset :: Text -> Text -> ClientM Value
login :: Maybe Text -> HydraLogin -> ClientM Value
push :: Maybe Text -> Maybe Text -> Maybe Bool -> ClientM Value
restartBuild :: Int -> ClientM NoContent
-- This will provide us with the definitions for mkProject, mkJobset, ... push,
-- by generating a @client@ for the specified @HydraAPI@.
mkProject
  :<|> mkJobset
  :<|> getJobset
  :<|> rmJobset
  :<|> login
  :<|> push
  :<|> restartBuild = client (Proxy @HydraAPI)

-- Check if error is due to authentication failure (403 Forbidden)
isAuthError :: ClientError -> Bool
isAuthError (FailureResponse _ (Response {responseStatusCode = Status {statusCode = 403}})) = True
isAuthError _ = False
