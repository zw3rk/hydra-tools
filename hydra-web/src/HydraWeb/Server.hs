-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Servant server wiring: combines handlers into a WAI Application.
-- Uses hoistServer to thread the App context through all handlers.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HydraWeb.Server
  ( mkApp
  ) where

import Data.Proxy (Proxy (..))
import Network.Wai (Application)
import Servant
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (ssMaxAge, MaxAge(..))

import HydraWeb.API (HydraWebAPI, StaticAPI, FullAPI)
import HydraWeb.Config (Config (..))
import HydraWeb.Types (App (..), AppM, runAppM)
import HydraWeb.Handlers.Overview (overviewHandler)

-- | Build the WAI Application from an App context.
mkApp :: App -> Application
mkApp app = serve (Proxy @FullAPI) (server app)

-- | Full server combining dynamic handlers and static file serving.
server :: App -> Server FullAPI
server app = hoistServer (Proxy @HydraWebAPI) (runAppM app) apiServer
        :<|> staticServer (cfgStaticDir $ appConfig app)

-- | Dynamic route handlers.
apiServer :: ServerT HydraWebAPI AppM
apiServer = overviewHandler

-- | Static file server with 1-hour cache for CSS/JS assets.
staticServer :: FilePath -> Server StaticAPI
staticServer dir = serveDirectoryWith settings
  where
    settings = (defaultWebAppSettings dir)
      { ssMaxAge = MaxAgeSeconds 3600 }
