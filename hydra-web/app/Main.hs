-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Entry point for the hydra-web server.
-- Loads configuration, creates a DB pool, and starts Warp.
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.Wai.Handler.Warp (run, defaultSettings, setPort, setHost, runSettings)
import Data.Streaming.Network (HostPreference)
import Data.String (fromString)
import System.IO (hFlush, stdout)

import HydraWeb.Config (Config (..), loadConfig)
import HydraWeb.DB.Pool (createPool)
import HydraWeb.Server (mkApp)
import HydraWeb.Types (App (..))

main :: IO ()
main = do
  cfg <- loadConfig
  pool <- createPool (cfgDatabaseURL cfg)

  let app = App
        { appPool   = pool
        , appConfig = cfg
        }

  let (host, port) = parseListenAddr (cfgListenAddr cfg)
      settings = setPort port
               $ setHost (fromString host)
                 defaultSettings

  Text.putStrLn $ "hydra-web listening on " <> cfgListenAddr cfg
  hFlush stdout
  runSettings settings (mkApp app)

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
