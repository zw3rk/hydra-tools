-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Reverse proxy handler for forwarding write operations to the
-- upstream Hydra backend. Rewrites request paths and copies relevant
-- headers.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.Handlers.Proxy
  ( proxyToBackend
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (status502, hContentType, hAuthorization)
import qualified Network.Wai as Wai

import HydraWeb.Types (App (..))
import HydraWeb.Config (Config (..))

-- | Create a WAI Application that proxies requests to the Hydra backend.
-- Forwards the request method, path, query string, body, and relevant
-- headers. Returns the backend's response with a 60-second timeout.
proxyToBackend :: App -> Wai.Application
proxyToBackend app waiReq respond = do
  let cfg     = appConfig app
      mgr     = appHttpManager app
      backend = TE.unpack (cfgHydraBackendURL cfg)
      path    = Wai.rawPathInfo waiReq
      qs      = Wai.rawQueryString waiReq

  -- Read the request body.
  body <- Wai.strictRequestBody waiReq

  -- Build the backend request.
  initReq <- HC.parseRequest (backend ++ BS8.unpack path ++ BS8.unpack qs)
  let backendReq = initReq
        { HC.method         = Wai.requestMethod waiReq
        , HC.requestBody    = HC.RequestBodyLBS body
        , HC.requestHeaders = filterHeaders (Wai.requestHeaders waiReq)
        , HC.redirectCount  = 0  -- don't follow redirects
        }

  -- Forward to backend, catching connection errors.
  result <- try $ HC.httpLbs backendReq mgr
  case result of
    Left (e :: SomeException) ->
      respond $ Wai.responseLBS status502
        [("Content-Type", "text/plain")]
        (LBS.fromStrict $ TE.encodeUtf8 $ "proxy error: " <> TE.decodeLatin1 (BS8.pack (show e)))
    Right resp ->
      respond $ Wai.responseLBS
        (HC.responseStatus resp)
        (HC.responseHeaders resp)
        (HC.responseBody resp)

  where
    -- Only forward safe headers.
    filterHeaders :: [(BS.ByteString, BS.ByteString)] -> [(BS.ByteString, BS.ByteString)]
    filterHeaders = filter (\(name, _) -> name `elem` allowedHeaders)
    allowedHeaders =
      [ hContentType
      , hAuthorization
      , "X-Forwarded-For"
      , "X-Real-IP"
      , "Accept"
      ]
