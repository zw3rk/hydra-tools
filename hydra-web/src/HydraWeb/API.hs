-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Servant API type declaration for hydra-web.
-- Defines all routes at the type level. Adding a route here and forgetting
-- to wire a handler is a compile error.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HydraWeb.API
  ( HydraWebAPI
  , LegacyRedirectAPI
  , AuthAPI
  , ProfileAPI
  , AdminAPI
  , ActionsAPI
  , DownloadAPI
  , ProxyAPI
  , JSONAPI
  , JobAPI
  , StreamAPI
  , OrgRepoAPI
  , StaticAPI
  , FullAPI
  ) where

import Data.Text (Text)
import Lucid (Html)
import Servant
import Servant.HTML.Lucid (HTML)

import HydraWeb.Handlers.API (APIJobset, APIBuild)
import HydraWeb.Handlers.Job (ShieldBadge)

-- | The main application API — all dynamic routes.
-- Uses REST-style pluralized resource names.
-- The Cookie header is threaded at the top level so all page handlers
-- can resolve the logged-in user for the navigation bar.
type HydraWebAPI = Header "Cookie" Text :> HydraWebRoutes

-- | Inner routes, separated for type-level clarity.
type HydraWebRoutes =
  -- GET / — overview page (also serves as project list)
       Get '[HTML] (Html ())
  -- GET /projects — explicit project list
  :<|> "projects" :> Get '[HTML] (Html ())
  -- GET /projects/:name
  :<|> "projects" :> Capture "name" Text :> Get '[HTML] (Html ())
  -- GET /projects/:name/jobsets/:js?page=N
  :<|> "projects" :> Capture "name" Text :> "jobsets" :> Capture "js" Text
       :> QueryParam "page" Int :> Get '[HTML] (Html ())
  -- GET /eval/:id (flat — IDs are globally unique)
  :<|> "eval" :> Capture "id" Int :> Get '[HTML] (Html ())
  -- GET /eval/:id/tab/:name (HTMX partial)
  :<|> "eval" :> Capture "id" Int :> "tab" :> Capture "name" Text
       :> Get '[HTML] (Html ())
  -- GET /build/:id (flat — IDs are globally unique)
  :<|> "build" :> Capture "id" Int :> Get '[HTML] (Html ())
  -- GET /build/:id/nixlog/:stepnr — build step log viewer
  :<|> "build" :> Capture "id" Int :> "nixlog" :> Capture "stepnr" Int :> Get '[HTML] (Html ())
  -- GET /queue
  :<|> "queue" :> Get '[HTML] (Html ())
  -- GET /queue-summary
  :<|> "queue-summary" :> Get '[HTML] (Html ())
  -- GET /machines
  :<|> "machines" :> Get '[HTML] (Html ())
  -- GET /steps?page=N
  :<|> "steps" :> QueryParam "page" Int :> Get '[HTML] (Html ())
  -- GET /evals?page=N
  :<|> "evals" :> QueryParam "page" Int :> Get '[HTML] (Html ())
  -- GET /search?query=...
  :<|> "search" :> QueryParam "query" Text :> Get '[HTML] (Html ())
  -- GET /bridges
  :<|> "bridges" :> Get '[HTML] (Html ())
  -- GET /running-evals — currently evaluating jobsets
  :<|> "running-evals" :> Get '[HTML] (Html ())
  -- GET /robots.txt
  :<|> "robots.txt" :> Get '[PlainText] Text

-- | Legacy URL redirects (301 permanent).
-- Old URLs redirect to the new REST-style paths.
type LegacyRedirectAPI =
  -- /project/:name → /projects/:name
       "project" :> Capture "name" Text :> Get '[HTML] (Html ())
  -- /jobset/:p/:j → /projects/:p/jobsets/:j
  :<|> "jobset" :> Capture "project" Text :> Capture "jobset" Text
       :> QueryParam "page" Int :> Get '[HTML] (Html ())

-- | JSON API endpoints (backward-compatible with Hydra).
type JSONAPI =
  -- GET /api/jobsets?project=...
       "api" :> "jobsets" :> QueryParam "project" Text :> Get '[JSON] [APIJobset]
  -- GET /api/nrqueue
  :<|> "api" :> "nrqueue" :> Get '[PlainText] Text
  -- GET /api/latestbuilds?nr=N&project=...&jobset=...&job=...&system=...
  :<|> "api" :> "latestbuilds"
       :> QueryParam "nr" Int :> QueryParam "project" Text
       :> QueryParam "jobset" Text :> QueryParam "job" Text
       :> QueryParam "system" Text :> Get '[JSON] [APIBuild]
  -- GET /api/queue?nr=N
  :<|> "api" :> "queue" :> QueryParam "nr" Int :> Get '[JSON] [APIBuild]

-- | Job redirect and shield badge endpoints.
type JobAPI =
  "job" :> Capture "project" Text :> Capture "jobset" Text :> Capture "job" Text :>
    (    "latest"          :> Get '[JSON] ShieldBadge
    :<|> "latest-finished" :> Get '[JSON] ShieldBadge
    :<|> "latest-for" :> Capture "system" Text :> Get '[JSON] ShieldBadge
    :<|> "shield"          :> Get '[JSON] ShieldBadge
    )

-- | Authentication routes (login, OAuth flow, logout).
-- The callback and logout routes receive the Cookie header for
-- CSRF state validation and session cleanup respectively.
type AuthAPI =
  -- GET /login
       "login" :> Get '[HTML] (Html ())
  -- GET /auth/github — start OAuth flow
  :<|> "auth" :> "github" :> Get '[HTML] (Html ())
  -- GET /auth/github/callback?code=...&state=... (receives Cookie for CSRF validation)
  :<|> "auth" :> "github" :> "callback"
       :> Header "Cookie" Text
       :> QueryParam "code" Text :> QueryParam "state" Text
       :> Get '[HTML] (Html ())
  -- GET /logout (receives Cookie to clear the server-side session)
  :<|> "logout" :> Header "Cookie" Text :> Get '[HTML] (Html ())

-- | Profile page (user info + API token management).
-- Receives Cookie header for session-based authentication.
type ProfileAPI = "profile" :> Header "Cookie" Text :> Get '[HTML] (Html ())

-- | Admin dashboard with installation and org-map management.
-- All admin routes require Cookie header for super-admin authentication.
type AdminAPI = Header "Cookie" Text :>
  (    "admin" :> Get '[HTML] (Html ())
  :<|> "admin" :> "installations" :> Get '[HTML] (Html ())
  :<|> "admin" :> "installations" :> ReqBody '[FormUrlEncoded] [(Text, Text)]
       :> Post '[HTML] (Html ())
  :<|> "admin" :> "installations" :> Capture "id" Int :> "toggle"
       :> Post '[HTML] (Html ())
  :<|> "admin" :> "installations" :> Capture "id" Int :> "delete"
       :> Post '[HTML] (Html ())
  :<|> "admin" :> "org-map" :> Get '[HTML] (Html ())
  :<|> "admin" :> "org-map" :> ReqBody '[FormUrlEncoded] [(Text, Text)]
       :> Post '[HTML] (Html ())
  :<|> "admin" :> "org-map" :> "detect" :> Post '[HTML] (Html ())
  )

-- | User actions that modify state (POST endpoints).
-- Cookie header required for authentication.
type ActionsAPI = Header "Cookie" Text :>
  (    "projects" :> Capture "name" Text :> "jobsets" :> Capture "js" Text
       :> "trigger" :> Post '[PlainText] NoContent
  :<|> "build" :> Capture "id" Int :> "restart"
       :> Post '[PlainText] NoContent
  )

-- | Build product download endpoint (Raw WAI app for zero-copy file serving).
-- Trailing path segments are passed through for directory-based products.
type DownloadAPI = "build" :> Capture "id" Int :> "download"
                   :> Capture "productnr" Int :> Raw

-- | SSE stream endpoints (Raw WAI apps for long-lived connections).
type StreamAPI =
  -- GET /stream/global — nav count updates
       "stream" :> "global" :> Raw
  -- GET /stream/bridges — bridge status updates
  :<|> "stream" :> "bridges" :> Raw
  -- GET /stream/queue — queue updates
  :<|> "stream" :> "queue" :> Raw
  -- GET /stream/machines — active steps updates
  :<|> "stream" :> "machines" :> Raw
  -- GET /stream/running-evals — running evaluations updates
  :<|> "stream" :> "running-evals" :> Raw
  -- GET /stream/project/:name — per-project updates
  :<|> "stream" :> "project" :> Capture "name" Text :> Raw
  -- GET /stream/jobset/:p/:j — per-jobset updates
  :<|> "stream" :> "jobset" :> Capture "p" Text :> Capture "j" Text :> Raw

-- | Org/repo catch-all — must be last before StaticAPI.
-- Allows /:org/:repo URLs to resolve to a project page.
-- Cookie header for nav bar user display.
type OrgRepoAPI = Header "Cookie" Text :> Capture "org" Text :> Capture "repo" Text :> Get '[HTML] (Html ())

-- | Reverse proxy for write operations to Hydra backend (Raw WAI app).
type ProxyAPI = "api" :> Raw

-- | Static file serving API.
type StaticAPI = "static" :> Raw

-- | Full API combining all route groups and static files.
-- Route order matters: specific routes first, catch-alls last.
-- ProxyAPI must come AFTER JSONAPI so specific /api/* routes match first.
-- StaticAPI must come BEFORE OrgRepoAPI: the two-segment catch-all would
-- otherwise intercept /static/* requests when browsers send Accept: */*.
-- OrgRepoAPI must be last (catch-all for /:org/:repo).
type FullAPI =
       HydraWebAPI
  :<|> LegacyRedirectAPI
  :<|> AuthAPI
  :<|> ProfileAPI
  :<|> AdminAPI
  :<|> ActionsAPI
  :<|> DownloadAPI
  :<|> JSONAPI
  :<|> JobAPI
  :<|> StreamAPI
  :<|> ProxyAPI
  :<|> StaticAPI
  :<|> OrgRepoAPI
