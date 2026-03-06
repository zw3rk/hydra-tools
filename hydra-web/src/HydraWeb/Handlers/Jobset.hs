-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the jobset page (GET /jobset/:project/:jobset).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Jobset
  ( jobsetHandler
  ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Lucid
import Servant (err404)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Projects (getJobset)
import HydraWeb.DB.Evals (jobsetEvals, allJobsetEvalsCount)
import HydraWeb.DB.OrgMap (lookupByProject)
import HydraWeb.Models.Project (Jobset (..))
import HydraWeb.Visibility (isProjectAccessible, isSuperAdmin, isAuthenticated)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Jobset (jobsetPage)

-- | Render the jobset page with paginated evaluations.
-- Returns 404 if the project or jobset is hidden and user is not super-admin.
jobsetHandler :: Maybe Text -> Text -> Text -> Maybe Int -> AppM (Html ())
jobsetHandler mCookie project jobset mPage = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  let page    = min 10000 (max 1 (fromMaybe 1 mPage))
      perPage = 20
      offset  = (page - 1) * perPage
  -- Single connection: look up jobset, check visibility, then fetch evals.
  mResult <- liftIO $ withConn pool $ \conn -> do
    mJs <- getJobset conn project jobset
    case mJs of
      Nothing -> pure Nothing
      Just js -> do
        accessible <- isProjectAccessible conn project mUser
        if not accessible || (jsHidden js /= 0 && not (isSuperAdmin mUser))
          then pure Nothing
          else do
            es  <- jobsetEvals conn (jsId js) offset perPage
            tc  <- allJobsetEvalsCount conn (jsId js)
            mor <- lookupByProject conn project
            pure $ Just (js, es, tc, mor)
  case mResult of
    Nothing -> throwError err404
    Just (js, evals, total, mOrgRepo) -> do
      let pd = PageData
            { pdTitle    = project <> ":" <> jsName js
            , pdBasePath = bp
            , pdCounts   = counts
            , pdUser     = mUser
            }
      pure $ pageLayout pd $ jobsetPage bp mOrgRepo js evals total page perPage (isAuthenticated mUser)
