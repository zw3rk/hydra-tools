-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Profile page handler: user info and API token CRUD.
-- Requires session-based authentication via Cookie header.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Profile
  ( profileHandler
  ) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Lucid (Html)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (requireAuth)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Auth (listAPITokens)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.Models.User (GFUser (..))
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Profile (profilePage)

-- | GET /profile — render the user profile page.
-- Requires authentication: redirects to login if no valid session.
profileHandler :: Maybe Text -> AppM (Html ())
profileHandler cookie = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  -- Authenticate: require a valid session.
  result <- liftIO $ requireAuth pool cookie
  user <- case result of
    Left err  -> throwError err
    Right u   -> pure u
  -- Fetch the user's API tokens and nav counts.
  (tokens, counts) <- liftIO $ withConn pool $ \conn -> do
    ts <- listAPITokens conn (gfuId user)
    nc <- navCounts conn
    pure (ts, nc)
  let pd = PageData
        { pdTitle    = "Profile"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = Just user
        }
  pure $ pageLayout pd $ profilePage bp user tokens Nothing
