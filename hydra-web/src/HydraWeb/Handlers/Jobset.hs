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
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Jobset (jobsetPage)

-- | Render the jobset page with paginated evaluations.
jobsetHandler :: Maybe Text -> Text -> Text -> Maybe Int -> AppM (Html ())
jobsetHandler mCookie project jobset mPage = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  let page    = min 10000 (max 1 (maybe 1 id mPage))
      perPage = 20
      offset  = (page - 1) * perPage
  -- Look up the jobset first; return 404 early if it doesn't exist.
  mJs <- liftIO $ withConn pool $ \conn -> getJobset conn project jobset
  case mJs of
    Nothing -> throwError err404
    Just js -> do
      (evals, total, counts) <- liftIO $ withConn pool $ \conn -> do
        es <- jobsetEvals conn (jsId js) offset perPage
        tc <- allJobsetEvalsCount conn (jsId js)
        nc <- navCounts conn
        pure (es, tc, nc)
      mUser <- liftIO $ getOptionalUser pool mCookie
      let pd = PageData
            { pdTitle    = project <> ":" <> jsName js
            , pdBasePath = bp
            , pdCounts   = counts
            , pdUser     = mUser
            }
      pure $ pageLayout pd $ jobsetPage bp js evals total page perPage
