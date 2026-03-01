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

import Lucid (Html)
import Servant
import Servant.HTML.Lucid (HTML)

-- | The main application API. Routes are added here as we implement phases.
-- Phase 0: just the overview page.
type HydraWebAPI =
  -- GET / — overview page
  Get '[HTML] (Html ())

  -- Phase 1 routes will be added here:
  -- :<|> "project" :> Capture "name" Text :> Get '[HTML] (Html ())
  -- :<|> "jobset" :> Capture "project" Text :> Capture "jobset" Text :> Get '[HTML] (Html ())
  -- :<|> "build" :> Capture "id" Int :> Get '[HTML] (Html ())
  -- :<|> "eval" :> Capture "id" Int :> Get '[HTML] (Html ())
  -- :<|> "queue" :> Get '[HTML] (Html ())
  -- :<|> "search" :> Get '[HTML] (Html ())

-- | Static file serving API.
type StaticAPI = "static" :> Raw

-- | Full API combining dynamic routes and static files.
type FullAPI = HydraWebAPI :<|> StaticAPI
