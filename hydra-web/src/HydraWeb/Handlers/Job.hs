-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handlers for job routes: latest-build redirects and shields.io badge.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Job
  ( jobLatestHandler
  , jobLatestFinishedHandler
  , jobLatestForSystemHandler
  , jobShieldHandler
    -- * Response types (re-exported for API.hs route declarations)
  , ShieldBadge
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Error.Class (throwError)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Servant (err302, err404, ServerError (..))

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Builds (latestBuildForJob)
import HydraWeb.Models.Build (Build (..))

-- | Shields.io badge response format.
data ShieldBadge = ShieldBadge
  { sbSchemaVersion :: !Int
  , sbLabel         :: !Text
  , sbMessage       :: !Text
  , sbColor         :: !Text
  } deriving (Generic)

instance ToJSON ShieldBadge where
  toJSON b = object
    [ "schemaVersion" .= sbSchemaVersion b
    , "label"         .= sbLabel b
    , "message"       .= sbMessage b
    , "color"         .= sbColor b
    ]

-- | Throw a 302 redirect to the given URL.
redirect302 :: Text -> AppM a
redirect302 url = throwError err302
  { errHeaders = [("Location", TE.encodeUtf8 url)]
  , errBody    = ""
  }

-- | Redirect to the latest successful build for a job.
-- GET /job/:project/:jobset/:job/latest
jobLatestHandler :: Text -> Text -> Text -> AppM ShieldBadge
jobLatestHandler project jobset job = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mBuild <- liftIO $ withConn pool $ \conn ->
    latestBuildForJob conn project jobset job Nothing
  case mBuild of
    Nothing    -> throwError err404
    Just build -> redirect302 (bp <> "/build/" <> Text.pack (show (buildId build)))

-- | Same as jobLatest — both look for finished+succeeded.
-- GET /job/:project/:jobset/:job/latest-finished
jobLatestFinishedHandler :: Text -> Text -> Text -> AppM ShieldBadge
jobLatestFinishedHandler = jobLatestHandler

-- | Redirect to the latest build for a specific system.
-- GET /job/:project/:jobset/:job/latest-for/:system
jobLatestForSystemHandler :: Text -> Text -> Text -> Text -> AppM ShieldBadge
jobLatestForSystemHandler project jobset job system = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mBuild <- liftIO $ withConn pool $ \conn ->
    latestBuildForJob conn project jobset job (Just system)
  case mBuild of
    Nothing    -> throwError err404
    Just build -> redirect302 (bp <> "/build/" <> Text.pack (show (buildId build)))

-- | Return a shields.io-compatible badge for the latest build status.
-- GET /job/:project/:jobset/:job/shield
jobShieldHandler :: Text -> Text -> Text -> AppM ShieldBadge
jobShieldHandler project jobset job = do
  pool <- asks appPool
  mBuild <- liftIO $ withConn pool $ \conn ->
    latestBuildForJob conn project jobset job Nothing
  pure $ case mBuild of
    Nothing -> ShieldBadge 1 "hydra build" "unknown" "lightgrey"
    Just build -> case buildStatus build of
      Just 0  -> ShieldBadge 1 "hydra build" "passing" "green"
      _       -> ShieldBadge 1 "hydra build" "failing" "red"
