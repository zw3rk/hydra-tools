-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | OpenAPI 3.0 documentation generated from the Servant API types.
-- Only the typed JSON endpoints (JSONAPI, JobAPI) are included — Raw
-- endpoints (downloads, SSE, proxy) are not representable in OpenAPI
-- and are documented elsewhere.
module HydraWeb.Docs
  ( apiOpenApi,
  )
where

import Control.Lens ((&), (.~), (?~))
import Data.OpenApi (OpenApi, description, info, title, version)
import Data.Proxy (Proxy (..))
import HydraWeb.API (JSONAPI, JobAPI)
-- Needed for ToSchema instances (imported for side effects).
import HydraWeb.Handlers.API ()
import HydraWeb.Handlers.Job ()
import Servant.API ((:<|>))
import Servant.OpenApi (toOpenApi)

-- | The subset of FullAPI that produces typed JSON responses.
-- Raw endpoints (download, SSE, proxy, static) cannot be represented
-- in servant-openapi3 and are excluded.
type DocumentedAPI = JSONAPI :<|> JobAPI

-- | Pre-computed OpenAPI 3.0 specification for the documented endpoints.
-- Served at GET /api/docs.
apiOpenApi :: OpenApi
apiOpenApi =
  toOpenApi (Proxy :: Proxy DocumentedAPI)
    & info . title .~ "Hydra Web API"
    & info . version .~ "0.2.0"
    & info . description ?~ "JSON API for Hydra CI — build status, queue, and job badges."
