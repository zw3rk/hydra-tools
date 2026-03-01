-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for build pages (GET /build/:id) and HTMX tab partials.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Build
  ( buildHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Error.Class (throwError)
import Data.Text (Text)
import Lucid
import Servant (err404)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Builds
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Build (buildPage)
import HydraWeb.View.Components (showT)

-- | Render the build detail page with steps, outputs, products, etc.
buildHandler :: Int -> AppM (Html ())
buildHandler bid = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  result <- liftIO $ withConn pool $ \conn -> do
    mBuild <- getBuild conn bid
    case mBuild of
      Nothing -> pure Nothing
      Just build -> do
        steps    <- getBuildSteps conn bid
        outputs  <- getBuildOutputs conn bid
        products <- getBuildProducts conn bid
        metrics  <- getBuildMetrics conn bid
        inputs   <- getBuildInputs conn bid
        evalIDs  <- getBuildEvals conn bid
        constits <- getConstituents conn bid
        nc       <- navCounts conn
        pure $ Just (build, steps, outputs, products, metrics,
                     inputs, evalIDs, constits, nc)
  case result of
    Nothing -> throwError err404
    Just (build, steps, outputs, products, metrics,
          inputs, evalIDs, constits, counts) -> do
      let pd = PageData
            { pdTitle    = "Build #" <> showT bid
            , pdBasePath = bp
            , pdCounts   = counts
            }
      pure $ pageLayout pd $
        buildPage bp build steps outputs products metrics inputs evalIDs constits
