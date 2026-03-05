-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the jobset page (GET /jobset/:project/:jobset).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Jobset
  ( jobsetHandler
  ) where

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
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.Models.Project (Jobset (..))
import HydraWeb.Visibility (isProjectAccessible, isSuperAdmin)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Jobset (jobsetPage)

-- | Render the jobset page with paginated evaluations.
-- Returns 404 if the project or jobset is hidden and user is not super-admin.
jobsetHandler :: Maybe Text -> Text -> Text -> Maybe Int -> AppM (Html ())
jobsetHandler mCookie project jobset mPage = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  let page    = min 10000 (max 1 (fromMaybe 1 mPage))
      perPage = 20
      offset  = (page - 1) * perPage
  -- Look up the jobset and project hidden status; return 404 if not visible.
  mResult <- liftIO $ withConn pool $ \conn -> do
    mJs <- getJobset conn project jobset
    case mJs of
      Nothing -> pure Nothing
      Just js -> do
        accessible <- isProjectAccessible conn project mUser
        pure $ Just (js, accessible)
  case mResult of
    Nothing -> throwError err404
    Just (js, accessible)
      -- Project-level check (includes hidden flag + repo privacy).
      | not accessible -> throwError err404
      -- Jobset-level hidden flag check (super-admin bypass).
      | jsHidden js /= 0 && not (isSuperAdmin mUser) -> throwError err404
      | otherwise -> do
          (evals, total, counts) <- liftIO $ withConn pool $ \conn -> do
            es <- jobsetEvals conn (jsId js) offset perPage
            tc <- allJobsetEvalsCount conn (jsId js)
            nc <- navCounts conn
            pure (es, tc, nc)
          let pd = PageData
                { pdTitle    = project <> ":" <> jsName js
                , pdBasePath = bp
                , pdCounts   = counts
                , pdUser     = mUser
                }
          let isAuth = case mUser of { Just _ -> True; Nothing -> False }
          pure $ pageLayout pd $ jobsetPage bp js evals total page perPage isAuth
