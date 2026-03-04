-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the bridge status page.
-- SSE streaming is now handled by the unified stream endpoints.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Bridges
  ( bridgesHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Lucid (Html)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Bridges (bridgeFullStatus)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Bridges (bridgesPage)

-- | GET /bridges — render the bridge status page with live SSE wrapper.
bridgesHandler :: AppM (Html ())
bridgesHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (status, counts) <- liftIO $ withConn pool $ \conn -> do
    s  <- bridgeFullStatus conn
    nc <- navCounts conn
    pure (s, nc)
  let pd = PageData
        { pdTitle    = "Bridge Status"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = Nothing
        }
  pure $ pageLayout pd $ bridgesPage bp status
