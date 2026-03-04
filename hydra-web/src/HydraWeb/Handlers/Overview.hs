-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the overview page (GET / and GET /projects).
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.Handlers.Overview
  ( overviewHandler
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Lucid

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Projects (visibleProjects)
import HydraWeb.DB.Queue (navCounts, newsItems)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Overview (overviewPage)

-- | Render the overview page with all visible projects, metric cards, and news.
overviewHandler :: AppM (Html ())
overviewHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (projects, counts, news) <- liftIO $ withConn pool $ \conn -> do
    ps <- visibleProjects conn
    nc <- navCounts conn
    ni <- newsItems conn 5 `catch` (\(_ :: SomeException) -> pure [])
    pure (ps, nc, ni)
  let pd = PageData
        { pdTitle    = "Overview"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ overviewPage bp counts projects news
