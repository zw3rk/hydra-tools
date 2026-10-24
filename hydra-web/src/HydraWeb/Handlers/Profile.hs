-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
{-# LANGUAGE OverloadedStrings #-}

-- | Profile page handler: user info and API token CRUD.
-- Requires session-based authentication via Cookie header.
module HydraWeb.Handlers.Profile
  ( profileHandler,
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import HydraWeb.Auth.Middleware (requireAuth)
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Auth (listAPITokens)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.Models.User (GFUser (..))
import HydraWeb.Types (App (..), AppM)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Profile (profilePage)
import Lucid (Html)

-- | GET /profile — render the user profile page.
-- Requires authentication: redirects to login if no valid session.
profileHandler :: Maybe Text -> AppM (Html ())
profileHandler cookie = do
  pool <- asks appPool
  bp <- asks (cfgBasePath . appConfig)
  -- Authenticate: require a valid session.
  result <- liftIO $ requireAuth pool cookie
  user <- case result of
    Left err -> throwError err
    Right u -> pure u
  -- Fetch the user's API tokens and nav counts.
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  tokens <- liftIO $ withConn pool $ \conn ->
    listAPITokens conn (gfuId user)
  let pd =
        PageData
          { pdTitle = "Profile",
            pdBasePath = bp,
            pdCounts = counts,
            pdUser = Just user
          }
  pure $ pageLayout pd $ profilePage bp user tokens Nothing
