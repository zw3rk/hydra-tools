-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Reverse proxy handler for forwarding write operations to the
-- upstream Hydra backend. Rewrites request paths and copies relevant
-- headers. Streams backend responses to avoid buffering large bodies.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.Handlers.Proxy
  ( proxyToBackend
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (status413, status502, hContentType, hContentLength,
                           hAuthorization, Header, HeaderName)
import qualified Network.Wai as Wai

import HydraWeb.Types (App (..))
import HydraWeb.Config (Config (..))

-- | Maximum request body size (10 MB).
maxRequestBodySize :: Int
maxRequestBodySize = 10 * 1024 * 1024

-- | Create a WAI Application that proxies requests to the Hydra backend.
-- Forwards the request method, path, query string, body, and relevant
-- headers. Streams the backend response to avoid memory exhaustion.
proxyToBackend :: App -> Wai.Application
proxyToBackend app waiReq respond = do
  let cfg     = appConfig app
      mgr     = appHttpManager app
      backend = Text.unpack (cfgHydraBackendURL cfg)
      path    = Wai.rawPathInfo waiReq
      qs      = Wai.rawQueryString waiReq

  -- Check Content-Length header to reject oversized request bodies early.
  let mContentLen = lookup hContentLength (Wai.requestHeaders waiReq)
      tooLarge    = case mContentLen of
        Just cl -> maybe True (> maxRequestBodySize) (readInt cl)
        Nothing -> False

  if tooLarge
    then respond $ Wai.responseLBS status413
           [("Content-Type", "text/plain")]
           "request body too large"
    else do
      -- Read the request body with a size limit.
      body <- readBoundedBody waiReq maxRequestBodySize

      case body of
        Nothing ->
          respond $ Wai.responseLBS status413
            [("Content-Type", "text/plain")]
            "request body too large"
        Just bodyBS -> do
          -- Build the backend request.
          initReq <- HC.parseRequest (backend ++ BS8.unpack path ++ BS8.unpack qs)
          let backendReq = initReq
                { HC.method         = Wai.requestMethod waiReq
                , HC.requestBody    = HC.RequestBodyLBS bodyBS
                , HC.requestHeaders = filterHeaders (Wai.requestHeaders waiReq)
                , HC.redirectCount  = 0  -- don't follow redirects
                }

          -- Forward to backend, streaming the response body to avoid buffering.
          result <- try $ HC.withResponse backendReq mgr $ \resp -> do
            let status  = HC.responseStatus resp
                headers = HC.responseHeaders resp
                reader  = HC.responseBody resp  -- IO ByteString, empty when done
            respond $ Wai.responseStream status headers $ \write flush -> do
              let loop = do
                    chunk <- reader
                    if BS.null chunk
                      then flush
                      else write (Builder.byteString chunk) >> loop
              loop
          case result of
            Left (_e :: SomeException) ->
              respond $ Wai.responseLBS status502
                [("Content-Type", "text/plain")]
                "bad gateway"
            Right waiResp -> pure waiResp

  where
    -- Only forward safe headers. X-Forwarded-For and X-Real-IP are excluded
    -- to prevent client-side IP spoofing; the reverse proxy should set those.
    filterHeaders :: [Header] -> [Header]
    filterHeaders = filter (\(name, _) -> name `elem` allowedHeaders)
    allowedHeaders :: [HeaderName]
    allowedHeaders =
      [ hContentType
      , hAuthorization
      , "Accept"
      ]

-- | Read request body up to a maximum size. Returns Nothing if exceeded.
readBoundedBody :: Wai.Request -> Int -> IO (Maybe LBS.ByteString)
readBoundedBody req maxSize = go 0 []
  where
    go !acc chunks = do
      chunk <- Wai.getRequestBodyChunk req
      if BS.null chunk
        then pure $ Just (LBS.fromChunks (reverse chunks))
        else let acc' = acc + BS.length chunk
             in if acc' > maxSize
                  then pure Nothing
                  else go acc' (chunk : chunks)

-- | Parse a ByteString as a non-negative integer.
readInt :: BS.ByteString -> Maybe Int
readInt bs = case BS8.readInt bs of
  Just (n, rest) | n >= 0, BS.null rest -> Just n
  _                                     -> Nothing
