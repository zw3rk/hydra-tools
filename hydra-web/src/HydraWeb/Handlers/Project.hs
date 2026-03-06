-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
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
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Projects (getProject, jobsetOverview)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.DB.OrgMap (lookupByProject)
import HydraWeb.Models.Project (Project (..), Jobset (..))
import HydraWeb.Visibility (isProjectAccessible, isSuperAdmin)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Project (projectPage)

-- | Render the project page with jobset overview.
-- Returns 404 if the project is hidden and the user is not a super-admin.
projectHandler :: Maybe Text -> Text -> AppM (Html ())
projectHandler mCookie name = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  (mProject, accessible, jobsets, counts, mOrgRepo) <- liftIO $ withConn pool $ \conn -> do
    mp <- getProject conn name
    acc <- isProjectAccessible conn name mUser
    js <- jobsetOverview conn name
    nc <- navCounts conn
    mor <- lookupByProject conn name
    pure (mp, acc, js, nc, mor)
  case mProject of
    Nothing -> throwError err404
    Just project
      | not accessible -> throwError err404
      | otherwise -> do
          -- Filter hidden jobsets for non-admin users.
          let visibleJs = if isSuperAdmin mUser
                            then jobsets
                            else filter (\j -> jsHidden j == 0) jobsets
              pd = PageData
                { pdTitle    = projDisplayName project
                , pdBasePath = bp
                , pdCounts   = counts
                , pdUser     = mUser
                }
          pure $ pageLayout pd $ projectPage bp mOrgRepo project visibleJs
