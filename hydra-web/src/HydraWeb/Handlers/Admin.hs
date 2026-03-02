-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Admin dashboard handler: user management.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Admin
  ( adminHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Lucid (Html)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Auth (listGFUsers)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Admin (adminPage)

-- | GET /admin — render the admin dashboard.
-- TODO: requires super-admin authentication check.
adminHandler :: AppM (Html ())
adminHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (users, counts) <- liftIO $ withConn pool $ \conn -> do
    us <- listGFUsers conn
    nc <- navCounts conn
    pure (us, nc)
  let pd = PageData
        { pdTitle    = "Admin Dashboard"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ adminPage bp users
