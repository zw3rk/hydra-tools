-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the overview page (GET /).
-- Shows all visible projects in a table.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Overview
  ( overviewHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Lucid

import HydraWeb.Types (AppM (..), App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Projects (visibleProjects)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.Models.Project (Project (..))
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Components (projectURL)

-- | Render the overview page with all visible projects.
overviewHandler :: AppM (Html ())
overviewHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (projects, counts) <- liftIO $ withConn pool $ \conn -> do
    ps <- visibleProjects conn
    nc <- navCounts conn
    pure (ps, nc)
  let pd = PageData
        { pdTitle    = "Overview"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ do
    h1_ "Projects"
    table_ $ do
      thead_ $ tr_ $ do
        th_ "Project"
        th_ "Description"
      tbody_ $
        mapM_ (renderProject bp) projects

-- | Render a single project table row.
renderProject :: Text -> Project -> Html ()
renderProject bp p = do
  let cls = if projEnabled p == 0 then [class_ "disabled"] else []
  tr_ cls $ do
    td_ $ do
      a_ [href_ (projectURL bp (projName p))] $ toHtml (projDisplayName p)
      case projHomepage p of
        Just hp -> do
          " "
          a_ [href_ hp, class_ "small external", target_ "_blank"] "↗"
        Nothing -> pure ()
    td_ $ toHtml (fromMaybe "" (projDescription p))
