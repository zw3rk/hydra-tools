-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the bridge status page.
-- SSE streaming is now handled by the unified stream endpoints.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Bridges
  ( bridgesHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Lucid (Html)

import Data.Text (Text)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Bridges (bridgeFullStatus)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.Models.Bridge (BridgeStatus (..), GitHubBridgeStatus (..),
                               GitHubRepoRow (..), GitHubRecentSend (..))
import HydraWeb.Visibility (filterByRepoAccess)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Bridges (bridgesPage)

-- | GET /bridges — render the bridge status page with live SSE wrapper.
-- Filters GitHub bridge data by project visibility so hidden/private
-- repo notifications are not exposed to unauthorized users.
bridgesHandler :: Maybe Text -> AppM (Html ())
bridgesHandler mCookie = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  (filtered, counts) <- liftIO $ withConn pool $ \conn -> do
    s  <- bridgeFullStatus conn
    -- Filter GitHub bridge data by (owner, repo) → project visibility.
    fs <- filterGitHubBridge conn mUser s
    nc <- navCounts conn
    pure (fs, nc)
  let pd = PageData
        { pdTitle    = "Bridge Status"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = mUser
        }
  pure $ pageLayout pd $ bridgesPage bp filtered
  where
    -- | Filter owner/repo breakdowns and recent sends in the GitHub bridge.
    -- Aggregate counts (total/sent/pending/failed) are kept unfiltered
    -- since they don't reveal project names.
    filterGitHubBridge conn mUser bs = case bsGitHub bs of
      Nothing -> pure bs
      Just gh -> do
        byRepo <- filterByRepoAccess conn mUser grrOwner grrRepo (ghsByRepo gh)
        recent <- filterByRepoAccess conn mUser grsOwner grsRepo (ghsRecentSent gh)
        pure bs { bsGitHub = Just gh { ghsByRepo = byRepo
                                     , ghsRecentSent = recent } }
