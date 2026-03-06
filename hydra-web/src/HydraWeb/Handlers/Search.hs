-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the search page (GET /search).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Search
  ( searchHandler
  ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Search (search, SearchResults (..))
import HydraWeb.Models.Project (Project (..), Jobset (..))
import HydraWeb.Models.Build (Build (..))
import HydraWeb.Visibility (filterByProjectAccess)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Search (searchPage)

-- | Render the search page, optionally with results if a query is provided.
searchHandler :: Maybe Text -> Maybe Text -> AppM (Html ())
searchHandler mCookie mQuery = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  -- Cap query at 200 chars to prevent excessive DB load.
  let q = Text.take 200 $ maybe "" Text.strip mQuery
  mResults <- if Text.null q
    then pure Nothing
    else liftIO $ withConn pool $ \conn -> do
      mr <- search conn q 10
      -- Post-filter search results by Phase 2 (private repo) visibility.
      case mr of
        Nothing -> pure Nothing
        Just sr -> do
          ps <- filterByProjectAccess conn mUser projName (srProjects sr)
          js <- filterByProjectAccess conn mUser jsProject (srJobsets sr)
          bs <- filterByProjectAccess conn mUser buildProject (srBuilds sr)
          ds <- filterByProjectAccess conn mUser buildProject (srBuildsDrv sr)
          pure $ Just sr { srProjects = ps, srJobsets = js
                         , srBuilds = bs, srBuildsDrv = ds }
  let pd = PageData
        { pdTitle    = "Search"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = mUser
        }
  pure $ pageLayout pd $ searchPage bp q mResults
