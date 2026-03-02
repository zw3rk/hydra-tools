-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Servant API type declaration for hydra-web.
-- Defines all routes at the type level. Adding a route here and forgetting
-- to wire a handler is a compile error.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HydraWeb.API
  ( HydraWebAPI
  , AuthAPI
  , ProfileAPI
  , AdminAPI
  , ProxyAPI
  , JSONAPI
  , JobAPI
  , SSEAPI
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
type HydraWebAPI =
  -- GET / — overview page
       Get '[HTML] (Html ())
  -- GET /project/:name
  :<|> "project" :> Capture "name" Text :> Get '[HTML] (Html ())
  -- GET /jobset/:project/:jobset?page=N
  :<|> "jobset" :> Capture "project" Text :> Capture "jobset" Text
       :> QueryParam "page" Int :> Get '[HTML] (Html ())
  -- GET /eval/:id
  :<|> "eval" :> Capture "id" Int :> Get '[HTML] (Html ())
  -- GET /eval/:id/tab/:name (HTMX partial)
  :<|> "eval" :> Capture "id" Int :> "tab" :> Capture "name" Text
       :> Get '[HTML] (Html ())
  -- GET /build/:id
  :<|> "build" :> Capture "id" Int :> Get '[HTML] (Html ())
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
  -- GET /robots.txt
  :<|> "robots.txt" :> Get '[PlainText] Text

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
type AuthAPI =
  -- GET /login
       "login" :> Get '[HTML] (Html ())
  -- GET /auth/github — start OAuth flow
  :<|> "auth" :> "github" :> Get '[HTML] (Html ())
  -- GET /auth/github/callback?code=...&state=...
  :<|> "auth" :> "github" :> "callback"
       :> QueryParam "code" Text :> QueryParam "state" Text
       :> Get '[HTML] (Html ())
  -- GET /logout
  :<|> "logout" :> Get '[HTML] (Html ())

-- | Profile page (user info + API token management).
type ProfileAPI = "profile" :> Get '[HTML] (Html ())

-- | Admin dashboard (user management, super-admin toggle).
type AdminAPI = "admin" :> Get '[HTML] (Html ())

-- | Reverse proxy for write operations to Hydra backend (Raw WAI app).
type ProxyAPI = "api" :> Raw

-- | SSE stream endpoint (Raw WAI app for long-lived connections).
type SSEAPI = "bridges" :> "stream" :> Raw

-- | Static file serving API.
type StaticAPI = "static" :> Raw

-- | Full API combining all route groups and static files.
-- Note: ProxyAPI must come AFTER JSONAPI so specific /api/* routes match first.
type FullAPI =
       HydraWebAPI
  :<|> AuthAPI
  :<|> ProfileAPI
  :<|> AdminAPI
  :<|> JSONAPI
  :<|> JobAPI
  :<|> SSEAPI
  :<|> ProxyAPI
  :<|> StaticAPI
