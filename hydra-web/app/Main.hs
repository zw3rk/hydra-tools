-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Entry point for the hydra-web server.
-- Loads configuration, creates a DB pool, starts the SSE listener,
-- runs migrations, seeds installations, and launches Warp.
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.String (fromString)
import Network.Wai.Handler.Warp
  (defaultSettings, setPort, setHost, setTimeout, runSettings)
import System.IO (hFlush, stdout, hPutStrLn, stderr)

import Network.HTTP.Client.TLS (newTlsManager)

import HydraWeb.Auth.Encrypt (newEncryptor)
import HydraWeb.Auth.Session (cleanupExpiredSessions)
import HydraWeb.Config (Config (..), GitHubConfig (..), loadConfig)
import HydraWeb.DB.Migrate (runMigrations)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import HydraWeb.DB.Pool (createPool, withConn)
import HydraWeb.DB.Auth (bootstrapSuperAdmins)
import HydraWeb.DB.Installations (seedFromConfig)
import HydraWeb.DB.OrgMap (autoDetectMappings)
import HydraWeb.SSE.Hub (newHub)
import HydraWeb.GitHub.RepoCheck (repoVisibilityLoop)
import HydraWeb.SSE.Listener (listenAndBroadcast)
import HydraWeb.Server (mkApp)
import HydraWeb.Types (App (..))

main :: IO ()
main = do
  cfg  <- loadConfig
  pool <- createPool (cfgDatabaseURL cfg)
  hub  <- newHub
  mgr  <- newTlsManager

  -- Run database migrations for auth + installation tables.
  withConn pool runMigrations

  -- Bootstrap super-admins from config.
  withConn pool $ \conn -> bootstrapSuperAdmins conn (cfgSuperAdmins cfg)

  -- Seed GitHub installations from env var (one-time migration).
  let installPairs = ghInstallationIDs (cfgGitHub cfg)
  if null installPairs
    then pure ()
    else do
      withConn pool $ seedFromConfig installPairs
      hPutStrLn stderr $ "Seeded " ++ show (length installPairs) ++ " installation(s) from env"

  -- Auto-detect org/repo mappings from flake URIs.
  withConn pool autoDetectMappings

  -- Initialize encryption (Nothing if key is empty).
  let mEnc = newEncryptor (cfgEncryptionKey cfg)

  let app = App
        { appPool        = pool
        , appConfig      = cfg
        , appSSEHub      = Just hub
        , appEncryptor   = mEnc
        , appHttpManager = mgr
        }

  let (host, port) = parseListenAddr (cfgListenAddr cfg)
      -- Set timeout to 0 for long-lived SSE connections.
      settings = setPort port
               $ setHost (fromString host)
               $ setTimeout 0
                 defaultSettings

  -- Conditionally start the repo visibility background checker.
  -- Only runs when GitHub App credentials are configured.
  let ghCfg = cfgGitHub cfg
      withRepoVisibility action
        | ghAppID ghCfg > 0 && not (null (ghAppKeyFile ghCfg)) =
            withAsync (repoVisibilityLoop pool mgr 3600
                        (ghAppKeyFile ghCfg) "hydra-web" (ghAppID ghCfg)) $ \_ ->
              action
        | otherwise = action

  -- Start background threads: SSE listener, session cleanup, and repo visibility.
  withAsync (listenAndBroadcast (cfgDatabaseURL cfg) pool hub) $ \_ ->
    withAsync (sessionCleanupLoop pool) $ \_ ->
      withRepoVisibility $ do
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

-- | Periodically clean up expired sessions (every hour).
sessionCleanupLoop :: Pool Connection -> IO ()
sessionCleanupLoop pool = go
  where
    go = do
      result <- try (cleanupExpiredSessions pool) :: IO (Either SomeException Int)
      case result of
        Right n | n > 0 -> hPutStrLn stderr $ "Cleaned up " ++ show n ++ " expired session(s)"
        Left e          -> hPutStrLn stderr $ "Session cleanup error: " ++ show e
        Right _         -> pure ()
      threadDelay (3600 * 1000000)  -- 1 hour
      go
