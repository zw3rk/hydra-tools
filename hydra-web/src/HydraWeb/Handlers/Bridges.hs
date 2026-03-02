-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handlers for the bridge status page and SSE stream endpoint.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Bridges
  ( bridgesHandler
  , bridgesStreamApp
  ) where

import Control.Concurrent.STM (atomically, readTBQueue)
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Builder as Builder
import Lucid (Html)
import Network.HTTP.Types (status200, status503)
import Network.Wai (Application, responseLBS, responseStream)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Bridges (bridgeFullStatus)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.SSE.Hub (subscribe, unsubscribe)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Bridges (bridgesPage)

-- | GET /bridges — render the bridge status page with live SSE wrapper.
bridgesHandler :: AppM (Html ())
bridgesHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (status, counts) <- liftIO $ withConn pool $ \conn -> do
    s  <- bridgeFullStatus conn
    nc <- navCounts conn
    pure (s, nc)
  let pd = PageData
        { pdTitle    = "Bridge Status"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ bridgesPage bp status

-- | GET /bridges/stream — WAI Application serving an SSE stream.
-- Keeps the connection open and pushes bridge-update events as they
-- arrive from the Hub. Uses 'responseStream' for proper flushing.
bridgesStreamApp :: App -> Application
bridgesStreamApp app _req respond = do
  case appSSEHub app of
    Nothing ->
      respond $ responseLBS status503
        [("Content-Type", "text/plain")]
        "SSE not available"
    Just hub -> do
      (sid, q) <- subscribe hub
      respond $ responseStream status200
        [ ("Content-Type", "text/event-stream")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        , ("X-Accel-Buffering", "no")
        ]
        (\write flush -> finally
          (forever $ do
            msg <- atomically $ readTBQueue q
            write (Builder.byteString msg)
            flush
          )
          (unsubscribe hub sid)
        )
