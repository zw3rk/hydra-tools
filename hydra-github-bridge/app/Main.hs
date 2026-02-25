{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Async as Async
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
import Lib.Bridge (app, hydraClient, hydraClientEnv, notificationWatcher, statusHandlers)
import Lib.GitHub (TokenLease (..), fetchAppInstallationToken, fetchInstallations, gitHubKey)
import Lib.Hydra (HydraClientEnv (..))
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv, lookupEnv)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
    stdin,
    stdout,
  )
import Data.Maybe (fromMaybe)

fetchGitHubTokens ::
  Int ->
  FilePath ->
  Text ->
  ByteString ->
  [(Text, Int)] ->
  IO [(String, TokenLease)]
fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent ghAppInstallIds = do
  -- Fetch installations
  putStrLn "Fetching GitHub App installations..."
  ghAppInstalls <- fetchInstallations ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent
  putStrLn $ "Found " <> show (length ghAppInstalls) <> " installations"
  forM_ ghAppInstalls $ \(owner, installId) -> do
    Text.putStrLn $ "\t- " <> owner <> " (" <> Text.pack (show installId) <> ")"

  -- Filter out installations not configured
  appInstalls <- flip filterM ghAppInstalls $ \inst@(owner, installId) -> do
    let found = inst `elem` ghAppInstallIds
    unless found $
      Text.putStrLn $
        "Warning: No configured GitHub App Installation ID: "
          <> owner
          <> " ("
          <> Text.show installId
          <> ")"

    pure True

  -- Fetch app installation tokens
  forM appInstalls $ \(owner, installId) -> do
    lease <- fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent installId
    Text.putStrLn $ "Fetched new GitHub App installation token valid for " <> owner <> " until " <> Text.pack (show lease.expiry)
    return (Text.unpack owner, lease)

getValidGitHubToken ::
  Int ->
  FilePath ->
  Text ->
  ByteString ->
  [(Text, Int)] ->
  IORef [(String, TokenLease)] ->
  IO [(String, TokenLease)]
getValidGitHubToken ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent ghAppInstallIds ghTokens =
  -- Fetch tokens from in-memory state. If they are set to expire within 5 seconds,
  -- refresh them from GitHub.
  let buffer = 5 :: NominalDiffTime
   in getValidToken buffer ghTokens $ \owner -> do
        putStrLn $ "GitHub token expired or will expire within the next " <> show buffer <> ", fetching a new one..."

        -- Lookup the installation for the owner in the configured App Installation IDs.
        -- If found, fetch the token. If we don't know about it, we don't want to use it
        -- (eg, if a stranger found and installed our app).
        let ghAppInstallId = fmap snd . find ((owner ==) . Text.unpack . fst) $ ghAppInstallIds
        case ghAppInstallId of
          Nothing -> do
            Text.putStrLn $ "Warning: No configured GitHub App Installation ID " <> Text.pack owner
            pure Nothing
          Just inst ->
            Just <$> fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent inst

-- | Look up tokens from in-memory state. If they are set to expire within 'buffer', run
-- the 'fetch' action.
getValidToken ::
  NominalDiffTime ->
  IORef [(String, TokenLease)] ->
  (String -> IO (Maybe TokenLease)) ->
  IO [(String, TokenLease)]
getValidToken buffer lease fetch = do
  leases' <- readIORef lease
  now <- getCurrentTime
  leases'' <- forM leases' $ \lease'@(owner, tok) -> do
    case tok.expiry of
      Just expiry' | addUTCTime buffer now < expiry' -> return (owner, tok)
      _ -> maybe lease' (owner,) <$> fetch owner
  writeIORef lease leases''
  return leases''

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
  -- ghTokens is basically [(String, Token)]
  ghTokens <- fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent ghAppInstallIds
  ghTokensRef <- newIORef ghTokens

  putStrLn $ "Connecting to Hydra at " <> host
  env <- hydraClientEnv (Text.pack host) api_user api_pass

  putStrLn $ "Server is starting on port " ++ show port

  -- Start the app loop
  let numWorkers = 10 -- default number of workers
      getValidGitHubToken' =
        getValidGitHubToken
          ghAppId
          ghAppKeyFile
          ghEndpointUrl
          ghUserAgent
          ghAppInstallIds
          ghTokensRef

  Async.mapConcurrently_
    id
    [ Async.replicateConcurrently_
        numWorkers
        ( withConnect
            (ConnectInfo db 5432 db_user db_pass "hydra")
            (statusHandlers ghEndpointUrl ghUserAgent getValidGitHubToken')
        ),
      withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn ->
        hydraClient env conn,
      withConnect
        (ConnectInfo db 5432 db_user db_pass "hydra")
        (notificationWatcher host stateDir),
      withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn -> do
        run port (app (hceClientEnv env) conn (gitHubKey ghKey))
    ]
