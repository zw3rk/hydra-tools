-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for /:org/:repo catch-all URL pattern.
-- Looks up the org/repo mapping and delegates to the project handler.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.OrgRepo
  ( orgRepoHandler
  ) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Lucid (Html)
import Servant (err404)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.OrgMap (lookupByOrgRepo)
import HydraWeb.DB.Projects (getProject, jobsetOverview)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.Models.Project (Project (..))
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Project (projectPage)

-- | GET /:org/:repo — lookup org/repo mapping and render project page.
orgRepoHandler :: Text -> Text -> AppM (Html ())
orgRepoHandler org repo = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  result <- liftIO $ withConn pool $ \conn -> do
    mProjectName <- lookupByOrgRepo conn org repo
    case mProjectName of
      Nothing -> pure Nothing
      Just projectName -> do
        mp <- getProject conn projectName
        case mp of
          Nothing -> pure Nothing
          Just project -> do
            js <- jobsetOverview conn projectName
            nc <- navCounts conn
            pure $ Just (project, js, nc)
  case result of
    Nothing -> throwError err404
    Just (project, jobsets, counts) -> do
      let pd = PageData
            { pdTitle    = projDisplayName project
            , pdBasePath = bp
            , pdCounts   = counts
            , pdUser     = Nothing
            }
      pure $ pageLayout pd $ projectPage bp project jobsets
