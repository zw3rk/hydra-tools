{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Async as Async
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.IORef (IORef, newIORef)
import Data.List (find)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock (NominalDiffTime)
import Database.PostgreSQL.Simple
import Lib (HydraClientEnv (..), app, gitHubKey, hydraClient, hydraClientEnv)
import Lib.Bridge (notificationWatcher, statusHandlers)
import Lib.GitHub qualified as GitHub
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv, lookupEnv)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
    stdin,
    stdout,
  )

fetchGitHubTokens :: Int -> FilePath -> Text -> BS.ByteString -> IO [(String, GitHub.TokenLease)]
fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent = do
  putStrLn "Fetching GitHub App installations..."
  ghAppInstalls <- GitHub.fetchInstallations ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent
  putStrLn $ "Found " <> show (length ghAppInstalls) <> " installations"
  forM_ ghAppInstalls $ \(owner, installId) -> do
    Text.putStrLn $ "\t- " <> owner <> " (" <> Text.pack (show installId) <> ")"

  forM ghAppInstalls $ \(owner, installId) -> do
    lease <- GitHub.fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent installId
    Text.putStrLn $ "Fetched new GitHub App installation token valid for " <> owner <> " until " <> Text.pack (show lease.expiry)
    return (Text.unpack owner, lease)

getValidGitHubToken ::
  IORef [(String, GitHub.TokenLease)] ->
  Text ->
  ByteString ->
  IO [(String, GitHub.TokenLease)]
getValidGitHubToken ghTokens ghEndpointUrl ghUserAgent =
  let buffer = 5 :: NominalDiffTime
   in GitHub.getValidToken buffer ghTokens $ \owner -> do
        putStrLn $ "GitHub token expired or will expire within the next " <> show buffer <> ", fetching a new one..."
        ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
        ghAppInstallIds <- getEnv "GITHUB_APP_INSTALL_IDS" >>= return . read @[(String, Int)]
        let ghAppInstallId = fmap snd . find ((owner ==) . fst) $ ghAppInstallIds
        ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
        maybe
          (error $ "No configured GitHub App Installation ID " <> owner)
          (GitHub.fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent)
          ghAppInstallId

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Read environment variables
  host <- maybe "localhost" id <$> lookupEnv "HYDRA_HOST"
  db <- maybe "localhost" id <$> lookupEnv "HYDRA_DB"
  db_user <- maybe mempty id <$> lookupEnv "HYDRA_DB_USER"
  db_pass <- maybe mempty id <$> lookupEnv "HYDRA_DB_PASS"
  api_user <- maybe mempty Text.pack <$> lookupEnv "HYDRA_API_USER"
  api_pass <- maybe mempty Text.pack <$> lookupEnv "HYDRA_API_PASS"
  port <- maybe 8080 read <$> lookupEnv "PORT"
  stateDir <- getEnv "HYDRA_STATE_DIR"
  ghEndpointUrl <- Text.pack . maybe "https://api.github.com" id <$> lookupEnv "GITHUB_ENDPOINT_URL"
  ghUserAgent <- maybe "hydra-github-bridge" cs <$> lookupEnv "GITHUB_USER_AGENT"
  ghKey <- maybe mempty C8.pack <$> lookupEnv "GITHUB_WEBHOOK_SECRET"

  -- Authenticate to GitHub
  ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
  ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
  -- ghTokens is basically [(String, Token)]
  ghTokens <- fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent >>= newIORef

  putStrLn $ "Connecting to Hydra at " <> host
  env <- hydraClientEnv (Text.pack host) api_user api_pass

  putStrLn $ "Server is starting on port " ++ show port

  -- Start the app loop
  let numWorkers = 10 -- default number of workers
      getValidGitHubToken' = getValidGitHubToken ghTokens ghEndpointUrl ghUserAgent
  eres <-
    Async.race
      ( Async.race
          ( Async.replicateConcurrently_
              numWorkers
              ( withConnect
                  (ConnectInfo db 5432 db_user db_pass "hydra")
                  (statusHandlers ghEndpointUrl ghUserAgent getValidGitHubToken')
              )
          )
          ( withConnect
              (ConnectInfo db 5432 db_user db_pass "hydra")
              (notificationWatcher host stateDir)
          )
      )
      ( Async.race
          ( withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn -> do
              hydraClient env conn
          )
          ( withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn -> do
              run port (app (hceClientEnv env) conn (gitHubKey ghKey))
          )
      )

  either
    ( either
        (const . putStrLn $ "statusHandler exited")
        (const . putStrLn $ "withConnect exited")
    )
    ( either
        (const . putStrLn $ "hydraClient exited")
        (const . putStrLn $ "app exited")
    )
    eres
