-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Servant API type and client functions for the Hydra CI backend.
-- Provides type-safe access to project, jobset, login, push, and
-- build-restart endpoints.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HydraLib.Hydra.Client
  ( HydraClientEnv (..)
  , HydraAPI
  , mkProject
  , mkJobset
  , getJobset
  , rmJobset
  , login
  , push
  , restartBuild
  , isAuthError
  ) where

import Data.Aeson (Object, Value)
import Data.Text (Text)
import Network.HTTP.Types (Status (..))
import Servant
import Servant.Client (ClientEnv, ClientError (..), ClientM, ResponseF (..), client)

import HydraLib.Hydra.Types

-- | Client environment for authenticated Hydra API calls.
data HydraClientEnv = HydraClientEnv
  { hceHost      :: Text
  , hceUser      :: Text
  , hcePass      :: Text
  , hceClientEnv :: ClientEnv
  }

-- | Servant API type for the Hydra CI backend.
type HydraAPI =
  "project"
    :> Capture "project-id" Text
    :> ReqBody '[JSON] HydraProject
    :> UVerb 'PUT '[JSON] '[WithStatus 200 Object, WithStatus 201 Object]
  :<|> "jobset"
    :> Capture "project-id" Text
    :> Capture "jobset-id" Text
    :> ReqBody '[JSON] HydraJobset
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
  :<|> "api" :> "push"
    :> Header "Origin" Text
    :> QueryParam "jobsets" Text
    :> QueryParam "force" Bool
    :> Post '[JSON] Value
  :<|> "build"
    :> Capture "build-id" Int
    :> "restart"
    :> Get '[] NoContent

mkProject :: Text -> HydraProject -> ClientM (Union '[WithStatus 200 Object, WithStatus 201 Object])
mkJobset :: Text -> Text -> HydraJobset -> ClientM (Union '[WithStatus 200 Object, WithStatus 201 Object])
getJobset :: Text -> Text -> ClientM Value
rmJobset :: Text -> Text -> ClientM Value
login :: Maybe Text -> HydraLogin -> ClientM Value
push :: Maybe Text -> Maybe Text -> Maybe Bool -> ClientM Value
restartBuild :: Int -> ClientM NoContent
mkProject
  :<|> mkJobset
  :<|> getJobset
  :<|> rmJobset
  :<|> login
  :<|> push
  :<|> restartBuild = client (Proxy @HydraAPI)

-- | Check if a client error is a 403 Forbidden (authentication failure).
isAuthError :: ClientError -> Bool
isAuthError (FailureResponse _ (Response {responseStatusCode = Status {statusCode = 403}})) = True
isAuthError _ = False
