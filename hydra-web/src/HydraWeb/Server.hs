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

import Data.Text (Text)
import Servant
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (ssMaxAge, MaxAge(..))

import HydraWeb.API (HydraWebAPI, JSONAPI, JobAPI, StaticAPI, FullAPI)
import HydraWeb.Config (Config (..))
import HydraWeb.Types (App (..), AppM, runAppM)
import HydraWeb.Handlers.Overview (overviewHandler)
import HydraWeb.Handlers.Project (projectHandler)
import HydraWeb.Handlers.Jobset (jobsetHandler)
import HydraWeb.Handlers.Eval (evalHandler, evalTabHandler, latestEvalsHandler)
import HydraWeb.Handlers.Build (buildHandler)
import HydraWeb.Handlers.Queue (queueHandler, queueSummaryHandler, machinesHandler, stepsHandler)
import HydraWeb.Handlers.Search (searchHandler)
import HydraWeb.Handlers.API (apiJobsetsHandler, apiNrQueueHandler, apiLatestBuildsHandler, apiQueueHandler)
import HydraWeb.Handlers.Job (jobLatestHandler, jobLatestFinishedHandler, jobLatestForSystemHandler, jobShieldHandler)

-- | Build the WAI Application from an App context.
mkApp :: App -> Application
mkApp app = serve (Proxy @FullAPI) (server app)

-- | Full server combining all route groups and static file serving.
server :: App -> Server FullAPI
server app = hoistServer (Proxy @HydraWebAPI) (runAppM app) htmlServer
        :<|> hoistServer (Proxy @JSONAPI) (runAppM app) jsonServer
        :<|> hoistServer (Proxy @JobAPI) (runAppM app) jobServer
        :<|> staticServer (cfgStaticDir $ appConfig app)

-- | HTML page handlers, wired in the same order as HydraWebAPI.
htmlServer :: ServerT HydraWebAPI AppM
htmlServer = overviewHandler
        :<|> projectHandler
        :<|> jobsetHandler
        :<|> evalHandler
        :<|> evalTabHandler
        :<|> buildHandler
        :<|> queueHandler
        :<|> queueSummaryHandler
        :<|> machinesHandler
        :<|> stepsHandler
        :<|> latestEvalsHandler
        :<|> searchHandler
        :<|> robotsHandler

-- | JSON API handlers, wired in the same order as JSONAPI.
jsonServer :: ServerT JSONAPI AppM
jsonServer = apiJobsetsHandler
        :<|> apiNrQueueHandler
        :<|> apiLatestBuildsHandler
        :<|> apiQueueHandler

-- | Job redirect and shield handlers, wired in the same order as JobAPI.
jobServer :: ServerT JobAPI AppM
jobServer project jobset job =
         jobLatestHandler project jobset job
    :<|> jobLatestFinishedHandler project jobset job
    :<|> jobLatestForSystemHandler project jobset job
    :<|> jobShieldHandler project jobset job

-- | Serve robots.txt (disallow all crawling).
robotsHandler :: AppM Text
robotsHandler = pure "User-agent: *\nDisallow: /*\n"

-- | Static file server with 1-hour cache for CSS/JS assets.
staticServer :: FilePath -> Server StaticAPI
staticServer dir = serveDirectoryWith settings
  where
    settings = (defaultWebAppSettings dir)
      { ssMaxAge = MaxAgeSeconds 3600 }
