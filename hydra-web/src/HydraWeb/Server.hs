-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
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
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Servant
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (ssMaxAge, MaxAge(..))

import HydraWeb.API
import HydraWeb.Config (Config (..))
import HydraWeb.Types (App (..), AppM, runAppM)

-- Page handlers
import HydraWeb.Handlers.Overview (overviewHandler)
import HydraWeb.Handlers.Project (projectHandler)
import HydraWeb.Handlers.Jobset (jobsetHandler)
import HydraWeb.Handlers.Eval (evalHandler, evalTabHandler, latestEvalsHandler)
import HydraWeb.Handlers.Build (buildHandler)
import HydraWeb.Handlers.BuildLog (buildLogHandler)
import HydraWeb.Handlers.Queue (queueHandler, queueSummaryHandler, machinesHandler, stepsHandler)
import HydraWeb.Handlers.Search (searchHandler)
import HydraWeb.Handlers.Bridges (bridgesHandler)
import HydraWeb.Handlers.RunningEvals (runningEvalsHandler)

-- Legacy redirects
import HydraWeb.Handlers.Redirect (legacyProjectRedirect, legacyJobsetRedirect)

-- Org/repo catch-all
import HydraWeb.Handlers.OrgRepo (orgRepoHandler)

-- Auth handlers
import HydraWeb.Handlers.Auth (handleLogin, handleGitHubAuth, handleGitHubCallback, handleLogout)
import HydraWeb.Handlers.Profile (profileHandler)
import HydraWeb.Handlers.Admin (adminServer)
import HydraWeb.Handlers.Actions (triggerEvalHandler, restartBuildHandler)

-- API handlers
import HydraWeb.Handlers.Proxy (proxyToBackend)
import HydraWeb.Handlers.API (apiJobsetsHandler, apiNrQueueHandler, apiLatestBuildsHandler, apiQueueHandler)
import HydraWeb.Handlers.Job (jobLatestHandler, jobLatestFinishedHandler, jobLatestForSystemHandler, jobShieldHandler)

-- SSE stream handlers
import HydraWeb.SSE.Stream (streamApp)

-- | Build the WAI Application from an App context.
-- Wraps the Servant application with security response headers.
mkApp :: App -> Application
mkApp app = securityHeadersMiddleware $ serve (Proxy @FullAPI) (server app)

-- | WAI middleware that injects standard security response headers.
securityHeadersMiddleware :: Wai.Middleware
securityHeadersMiddleware baseApp req sendResp =
  baseApp req $ \response ->
    sendResp $ Wai.mapResponseHeaders (++ securityHeaders) response
  where
    securityHeaders :: [HTTP.Header]
    securityHeaders =
      [ ("X-Content-Type-Options", "nosniff")
      , ("X-Frame-Options", "DENY")
      , ("Referrer-Policy", "strict-origin-when-cross-origin")
      ]

-- | Full server combining all route groups and static file serving.
server :: App -> Server FullAPI
server app = hoistServer (Proxy @HydraWebAPI) (runAppM app) htmlServer
        :<|> hoistServer (Proxy @LegacyRedirectAPI) (runAppM app) legacyServer
        :<|> hoistServer (Proxy @AuthAPI) (runAppM app) authServer
        :<|> hoistServer (Proxy @ProfileAPI) (runAppM app) profileHandler
        :<|> hoistServer (Proxy @AdminAPI) (runAppM app) adminServer
        :<|> hoistServer (Proxy @ActionsAPI) (runAppM app) actionsServer
        :<|> hoistServer (Proxy @JSONAPI) (runAppM app) jsonServer
        :<|> hoistServer (Proxy @JobAPI) (runAppM app) jobServer
        :<|> streamServer app
        :<|> Tagged (proxyToBackend app)
        :<|> staticServer (cfgStaticDir $ appConfig app)
        :<|> hoistServer (Proxy @OrgRepoAPI) (runAppM app) orgRepoHandler

-- | HTML page handlers, wired in the same order as HydraWebRoutes.
-- The Cookie header is received once and threaded to each handler
-- for nav bar user display.
htmlServer :: ServerT HydraWebAPI AppM
htmlServer mCookie =
             overviewHandler mCookie
        :<|> overviewHandler mCookie  -- /projects — same handler as overview
        :<|> projectHandler mCookie
        :<|> jobsetHandler mCookie
        :<|> evalHandler mCookie
        :<|> evalTabHandler mCookie   -- HTMX partial: needs cookie for visibility check
        :<|> buildHandler mCookie
        :<|> buildLogHandler mCookie
        :<|> queueHandler mCookie
        :<|> queueSummaryHandler mCookie
        :<|> machinesHandler mCookie
        :<|> stepsHandler mCookie
        :<|> latestEvalsHandler mCookie
        :<|> searchHandler mCookie
        :<|> bridgesHandler mCookie
        :<|> runningEvalsHandler mCookie
        :<|> robotsHandler

-- | Legacy redirect handlers (301 permanent redirects).
legacyServer :: ServerT LegacyRedirectAPI AppM
legacyServer = legacyProjectRedirect
          :<|> legacyJobsetRedirect

-- | User action handlers (POST endpoints for trigger/restart).
actionsServer :: ServerT ActionsAPI AppM
actionsServer mCookie =
         triggerEvalHandler mCookie
    :<|> restartBuildHandler mCookie

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

-- | Auth handlers, wired in the same order as AuthAPI.
-- The callback and logout routes now receive the Cookie header.
authServer :: ServerT AuthAPI AppM
authServer = handleLogin
        :<|> handleGitHubAuth
        :<|> handleGitHubCallback
        :<|> handleLogout

-- | SSE stream server — Raw WAI applications for each stream topic.
streamServer :: App -> Server StreamAPI
streamServer app = Tagged (streamApp app "global")
              :<|> Tagged (streamApp app "bridges")
              :<|> Tagged (streamApp app "queue")
              :<|> Tagged (streamApp app "machines")
              :<|> Tagged (streamApp app "running-evals")
              :<|> (\name -> Tagged (streamApp app ("project:" <> name)))
              :<|> (\p j -> Tagged (streamApp app ("jobset:" <> p <> ":" <> j)))

-- | Serve robots.txt (disallow all crawling).
robotsHandler :: AppM Text
robotsHandler = pure "User-agent: *\nDisallow: /*\n"

-- | Static file server with 1-hour cache for CSS/JS assets.
staticServer :: FilePath -> Server StaticAPI
staticServer dir = serveDirectoryWith settings
  where
    settings = (defaultWebAppSettings dir)
      { ssMaxAge = MaxAgeSeconds 3600 }
