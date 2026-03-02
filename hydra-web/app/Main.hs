-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Entry point for the hydra-web server.
-- Loads configuration, creates a DB pool, starts the SSE listener,
-- and launches Warp with no write timeout (for SSE connections).
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async (withAsync)
import Data.IORef (newIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.String (fromString)
import Network.Wai.Handler.Warp
  (defaultSettings, setPort, setHost, setTimeout, runSettings)
import System.IO (hFlush, stdout)

import HydraWeb.Config (Config (..), loadConfig)
import HydraWeb.DB.Pool (createPool)
import HydraWeb.SSE.Hub (newHub)
import HydraWeb.SSE.Listener (listenAndBroadcast)
import HydraWeb.Server (mkApp)
import HydraWeb.Types (App (..))

main :: IO ()
main = do
  cfg  <- loadConfig
  pool <- createPool (cfgDatabaseURL cfg)
  hub  <- newHub

  -- SSE listener shutdown signal.
  running <- newIORef True

  let app = App
        { appPool   = pool
        , appConfig = cfg
        , appSSEHub = Just hub
        }

  let (host, port) = parseListenAddr (cfgListenAddr cfg)
      -- Set timeout to 0 for long-lived SSE connections.
      settings = setPort port
               $ setHost (fromString host)
               $ setTimeout 0
                 defaultSettings

  -- Start the SSE listener in a background thread.
  withAsync (listenAndBroadcast (cfgDatabaseURL cfg) pool hub running) $ \_ -> do
    Text.putStrLn $ "hydra-web listening on " <> cfgListenAddr cfg
    hFlush stdout
    runSettings settings (mkApp app)
    -- Signal the listener to stop on server shutdown.
    writeIORef running False

-- | Parse "host:port" into components. Defaults to 127.0.0.1:4000.
parseListenAddr :: Text -> (String, Int)
parseListenAddr addr =
  case Text.breakOnEnd ":" addr of
    ("", p) -> ("127.0.0.1", readPort p)
    (h, p)  -> (Text.unpack $ Text.dropEnd 1 h, readPort p)
  where
    readPort p = case reads (Text.unpack p) of
      [(n, "")] -> n
      _         -> 4000
