{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async as Async
import Data.ByteString.Char8 qualified as C8
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple
import Lib.Bridge.GitHubToHydra (app, hydraClient, hydraClientEnv, GitHubToHydraEnv (..))
import Lib.Bridge.HydraToGitHub
  ( HydraToGitHubEnv (..),
    fetchGitHubTokens,
    notificationWatcher,
    runHydraToGitHubT,
    statusHandlers,
  )
import Lib.GitHub (gitHubKey)
import Lib.Hydra (HydraClientEnv (..))
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv, lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Read environment variables
  host <- fromMaybe "localhost" <$> lookupEnv "HYDRA_HOST"
  db <- fromMaybe "localhost" <$> lookupEnv "HYDRA_DB"
  db_user <- fromMaybe mempty <$> lookupEnv "HYDRA_DB_USER"
  db_pass <- fromMaybe mempty <$> lookupEnv "HYDRA_DB_PASS"
  api_user <- maybe mempty Text.pack <$> lookupEnv "HYDRA_USER"
  api_pass <- maybe mempty Text.pack <$> lookupEnv "HYDRA_PASS"
  port <- maybe 8080 read <$> lookupEnv "PORT"
  stateDir <- getEnv "HYDRA_STATE_DIR"
  ghEndpointUrl <- maybe "https://api.github.com" cs <$> lookupEnv "GITHUB_ENDPOINT_URL"
  ghUserAgent <- maybe "hydra-github-bridge" cs <$> lookupEnv "GITHUB_USER_AGENT"
  ghKey <- maybe mempty C8.pack <$> lookupEnv "GITHUB_WEBHOOK_SECRET"

  -- Authenticate to GitHub
  ghAppId <- read <$> getEnv "GITHUB_APP_ID"
  ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
  ghAppInstallIds <- read <$> getEnv "GITHUB_APP_INSTALL_IDS"

  ghTokens <- fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent ghAppInstallIds
  ghTokensRef <- newIORef ghTokens

  putStrLn $ "Connecting to Hydra at " <> host
  env <- hydraClientEnv (Text.pack host) api_user api_pass

  putStrLn $ "Server is starting on port " ++ show port

  -- Start the app loop
  let numWorkers = 10 -- default number of workers

      hydraToGitHubEnv =
        HydraToGitHubEnv
          { htgEnvHydraHost = host,
            htgEnvHydraStateDir = stateDir,
            htgEnvGhAppId = ghAppId,
            htgEnvGhAppKeyFile = ghAppKeyFile,
            htgEnvGhEndpointUrl = ghEndpointUrl,
            htgEnvGhUserAgent = ghUserAgent,
            htgEnvGhAppInstallIds = ghAppInstallIds,
            htgEnvGhTokens = ghTokensRef
          }

      gitHubToHydraEnv =
        GitHubToHydraEnv
          { gthEnvHydraClient = hceClientEnv env,
            gthEnvGitHubKey = gitHubKey ghKey
          }

  Async.mapConcurrently_
    id
    [ Async.replicateConcurrently_
        numWorkers
        ( withConnect
            (ConnectInfo db 5432 db_user db_pass "hydra")
            (runHydraToGitHubT hydraToGitHubEnv . statusHandlers)
        ),
      withConnect
        (ConnectInfo db 5432 db_user db_pass "hydra")
        (hydraClient env),
      withConnect
        (ConnectInfo db 5432 db_user db_pass "hydra")
        (runHydraToGitHubT hydraToGitHubEnv . notificationWatcher),
      withConnect 
        (ConnectInfo db 5432 db_user db_pass "hydra")
        (run port . app gitHubToHydraEnv)
    ]
