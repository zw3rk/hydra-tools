-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Profile page handler: user info and API token CRUD.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Profile
  ( profileHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Lucid (Html)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Auth (listAPITokens)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Profile (profilePage)
import HydraWeb.Models.User (GFUser (..))

-- | GET /profile — render the user profile page.
-- TODO: requires authenticated user from middleware (placeholder for now).
profileHandler :: AppM (Html ())
profileHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  -- TODO: get user from request context via auth middleware.
  -- For now, render an empty page indicating login is required.
  counts <- liftIO $ withConn pool navCounts
  let pd = PageData
        { pdTitle    = "Profile"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  -- Placeholder: in production, this would use the authenticated user.
  let dummyUser = GFUser 0 0 "anonymous" Nothing Nothing Nothing False 0 0
  tokens <- liftIO $ withConn pool $ \conn -> listAPITokens conn 0
  pure $ pageLayout pd $ profilePage bp dummyUser tokens Nothing
