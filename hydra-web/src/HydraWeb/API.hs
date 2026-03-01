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
  , StaticAPI
  , FullAPI
  ) where

import Data.Text (Text)
import Lucid (Html)
import Servant
import Servant.HTML.Lucid (HTML)

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
  -- GET /robots.txt
  :<|> "robots.txt" :> Get '[PlainText] Text

-- | Static file serving API.
type StaticAPI = "static" :> Raw

-- | Full API combining dynamic routes and static files.
type FullAPI = HydraWebAPI :<|> StaticAPI
