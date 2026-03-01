-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the project page (GET /project/:name).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Project
  ( projectHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Lucid
import Servant (err404)
import Control.Monad.Error.Class (throwError)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Projects (getProject, jobsetOverview)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.Models.Project (Project (..))
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Project (projectPage)

-- | Render the project page with jobset overview.
projectHandler :: Text -> AppM (Html ())
projectHandler name = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (mProject, jobsets, counts) <- liftIO $ withConn pool $ \conn -> do
    mp <- getProject conn name
    js <- jobsetOverview conn name
    nc <- navCounts conn
    pure (mp, js, nc)
  case mProject of
    Nothing -> throwError err404
    Just project -> do
      let pd = PageData
            { pdTitle    = projDisplayName project
            , pdBasePath = bp
            , pdCounts   = counts
            }
      pure $ pageLayout pd $ projectPage bp project jobsets
