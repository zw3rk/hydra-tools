-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the search page (GET /search).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Search
  ( searchHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Search (search)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Search (searchPage)

-- | Render the search page, optionally with results if a query is provided.
searchHandler :: Maybe Text -> Maybe Text -> AppM (Html ())
searchHandler mCookie mQuery = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  -- Cap query at 200 chars to prevent excessive DB load.
  let q = Text.take 200 $ maybe "" Text.strip mQuery
  (mResults, counts) <- liftIO $ withConn pool $ \conn -> do
    nc <- navCounts conn
    if Text.null q
      then pure (Nothing, nc)
      else do
        mr <- search conn q 10
        pure (mr, nc)
  let pd = PageData
        { pdTitle    = "Search"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = mUser
        }
  pure $ pageLayout pd $ searchPage bp q mResults
