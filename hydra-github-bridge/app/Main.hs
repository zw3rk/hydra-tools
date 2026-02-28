{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async as Async
import Control.Monad
import Data.Aeson (toJSON)
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
import Database.PostgreSQL.Simple.Notification (getNotification)
import Debug.Trace (traceShowId)
import Lib.Bridge (app, hydraClient, hydraClientEnv, statusHandlers)
import Lib.Bridge.HydraToGitHub (handleHydraNotification, toHydraNotification)
import Lib.GitHub (TokenLease (..), fetchAppInstallationToken, fetchInstallations, gitHubKey)
import Lib.GitHub qualified as GitHub
import Lib.Hydra (HydraClientEnv (..))
import Lib.SSE (StatusCache, broadcastCheckRuns, newStatusCache, runSSEServer)
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv, lookupEnv)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
    stdin,
    stdout,
  )

-- | Variant of the notification watcher that also broadcasts check-run
-- statuses to the SSE cache for real-time client consumption.
notificationWatcherWithSSE ::
  StatusCache ->
  String ->
  String ->
  Text ->
  Bool ->
  Connection ->
  IO ()
notificationWatcherWithSSE cache host stateDir checkRunPrefix filterJobs conn = do
  _ <- execute_ conn "LISTEN eval_started"
  _ <- execute_ conn "LISTEN eval_added"
  _ <- execute_ conn "LISTEN eval_cached"
  _ <- execute_ conn "LISTEN eval_failed"
  _ <- execute_ conn "LISTEN build_queued"
  _ <- execute_ conn "LISTEN cached_build_queued"
  _ <- execute_ conn "LISTEN build_started"
  _ <- execute_ conn "LISTEN build_finished"
  _ <- execute_ conn "LISTEN cached_build_finished"
  forever $ do
    putStrLn "Waiting for notification..."
    note <- toHydraNotification . traceShowId <$> getNotification conn
    statuses <- handleHydraNotification conn (cs host) stateDir checkRunPrefix filterJobs note

    -- Broadcast to SSE subscribers before queuing for GitHub API delivery.
    broadcastCheckRuns cache statuses

    -- Queue for GitHub API delivery (existing behaviour).
    forM_ statuses $
      ( \(GitHub.CheckRun owner repo payload) -> do
          Text.putStrLn $ "QUEUEING [" <> owner <> "/" <> repo <> "/" <> payload.headSha <> "] " <> payload.name <> ":" <> Text.pack (show payload.status)
          [Only _id'] <-
            query
              conn
              "with status_upsert as (insert into github_status (owner, repo, headSha, name) values (?, ?, ?, ?) on conflict (owner, repo, headSha, name) do update set name = excluded.name returning id) insert into github_status_payload (status_id, payload) select (select id from status_upsert), ? returning id"
              (owner, repo, payload.headSha, payload.name, (toJSON payload)) ::
              IO [Only Int]
          execute_ conn "NOTIFY github_status"
      )

fetchGitHubTokens :: Int -> FilePath -> Text -> ByteString -> IO [(String, TokenLease)]
fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent = do
  putStrLn "Fetching GitHub App installations..."
  ghAppInstalls <- fetchInstallations ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent
  putStrLn $ "Found " <> show (length ghAppInstalls) <> " installations"
  forM_ ghAppInstalls $ \(owner, installId) -> do
    Text.putStrLn $ "\t- " <> owner <> " (" <> Text.pack (show installId) <> ")"

  forM ghAppInstalls $ \(owner, installId) -> do
    lease <- fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent installId
    Text.putStrLn $ "Fetched new GitHub App installation token valid for " <> owner <> " until " <> Text.pack (show lease.expiry)
    return (Text.unpack owner, lease)

getValidGitHubToken ::
  IORef [(String, TokenLease)] ->
  Text ->
  ByteString ->
  IO [(String, TokenLease)]
getValidGitHubToken ghTokens ghEndpointUrl ghUserAgent =
  let buffer = 5 :: NominalDiffTime
   in getValidToken buffer ghTokens $ \owner -> do
        putStrLn $ "GitHub token expired or will expire within the next " <> show buffer <> ", fetching a new one..."
        ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
        ghAppInstallIds <- getEnv "GITHUB_APP_INSTALL_IDS" >>= return . read @[(String, Int)]
        let ghAppInstallId = fmap snd . find ((owner ==) . fst) $ ghAppInstallIds
        ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
        maybe
          (error $ "No configured GitHub App Installation ID " <> owner)
          (fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent)
          ghAppInstallId

getValidToken :: NominalDiffTime -> IORef [(String, TokenLease)] -> (String -> IO TokenLease) -> IO [(String, TokenLease)]
getValidToken buffer lease fetch = do
  leases' <- readIORef lease
  now <- getCurrentTime
  leases'' <- forM leases' $ \(owner, tok) -> do
    case tok.expiry of
      Just expiry' | addUTCTime buffer now < expiry' -> return (owner, tok)
      _ -> (owner,) <$> fetch owner
  writeIORef lease leases''
  return leases''

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
  api_user <- maybe mempty Text.pack <$> lookupEnv "HYDRA_USER"
  api_pass <- maybe mempty Text.pack <$> lookupEnv "HYDRA_PASS"
  port <- maybe 8080 read <$> lookupEnv "PORT"
  stateDir <- getEnv "HYDRA_STATE_DIR"
  ghEndpointUrl <- Text.pack . maybe "https://api.github.com" id <$> lookupEnv "GITHUB_ENDPOINT_URL"
  ghUserAgent <- maybe "hydra-github-bridge" cs <$> lookupEnv "GITHUB_USER_AGENT"
  -- Webhook secret for signature verification.
  -- Prefer GITHUB_WEBHOOK_SECRET, fall back to KEY for backwards compatibility.
  ghKey <- do
    v <- lookupEnv "GITHUB_WEBHOOK_SECRET"
    case v of
      Just k  -> pure (C8.pack k)
      Nothing -> maybe mempty C8.pack <$> lookupEnv "KEY"
  checkRunPrefix <- maybe "ci/hydra-build:" Text.pack <$> lookupEnv "CHECK_RUN_PREFIX"
  filterJobs <- maybe True (\v -> v == "true" || v == "1") <$> lookupEnv "FILTER_JOBS"

  -- SSE configuration
  sseEnabled <- maybe True (\v -> v == "true" || v == "1") <$> lookupEnv "SSE_ENABLED"
  ssePort <- maybe 8812 read <$> lookupEnv "SSE_PORT"
  let sseTtl = 86400 :: NominalDiffTime -- 24 hours

  -- Authenticate to GitHub
  ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
  ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
  ghTokens <- fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent >>= newIORef

  putStrLn $ "Connecting to Hydra at " <> host
  env <- hydraClientEnv (Text.pack host) api_user api_pass

  putStrLn $ "Server is starting on port " ++ show port
  when sseEnabled $
    putStrLn $ "SSE server will start on port " ++ show ssePort

  -- Initialize SSE status cache
  cache <- newStatusCache

  -- Start the app loop
  let numWorkers = 10
      getValidGitHubToken' = getValidGitHubToken ghTokens ghEndpointUrl ghUserAgent

  -- Create partial index for the optimized unsent-payload query.
  -- CONCURRENTLY cannot run inside a transaction, so we use a
  -- dedicated connection with autocommit.
  withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \migConn -> do
    putStrLn "Ensuring partial index idx_github_status_payload_unsent exists..."
    _ <- execute_ migConn
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS \
      \idx_github_status_payload_unsent \
      \ON github_status_payload (status_id, id DESC) \
      \WHERE sent IS NULL AND tries < 5"
    putStrLn "Index ready."

  Async.mapConcurrently_
    id $
    [ Async.replicateConcurrently_
        numWorkers
        ( withConnect
            (ConnectInfo db 5432 db_user db_pass "hydra")
            (statusHandlers ghEndpointUrl ghUserAgent getValidGitHubToken')
        ),
      withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn ->
        hydraClient env conn,
      -- Use the SSE-broadcasting variant of the notification watcher.
      withConnect
        (ConnectInfo db 5432 db_user db_pass "hydra")
        (notificationWatcherWithSSE cache host stateDir checkRunPrefix filterJobs),
      withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn -> do
        run port (app (hceClientEnv env) conn (gitHubKey ghKey)),
      -- Periodically prune stale notifications for old commits that
      -- have already been superseded by newer evaluations.  Only marks
      -- a payload as sent when a later payload for the same
      -- (owner, repo, name) has already been successfully delivered.
      withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn ->
        forever $ do
          threadDelay (5 * 60 * 1000000) -- 5 minutes
          pruned <- execute_ conn
            "UPDATE github_status_payload SET sent = NOW() \
            \WHERE id IN ( \
            \  SELECT p.id \
            \  FROM github_status_payload p \
            \  JOIN github_status s ON s.id = p.status_id \
            \  WHERE p.sent IS NULL AND p.tries < 5 \
            \    AND EXISTS ( \
            \      SELECT 1 \
            \      FROM github_status_payload p2 \
            \      JOIN github_status s2 ON s2.id = p2.status_id \
            \      WHERE s2.owner = s.owner AND s2.repo = s.repo AND s2.name = s.name \
            \        AND p2.sent IS NOT NULL AND p2.id > p.id \
            \    ) \
            \)"
          when (pruned > 0) $
            putStrLn $ "Pruned " ++ show pruned ++ " stale notification(s)"
    ]
    ++ [ runSSEServer cache ssePort sseTtl | sseEnabled ]
