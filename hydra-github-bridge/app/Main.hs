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
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.IORef (IORef, newIORef)
import Data.Set qualified as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Database.PostgreSQL.Simple
import Debug.Trace (traceShowId)
import Lib.Bridge (app, hydraClient, hydraClientEnv, statusHandlers)
import Lib.Bridge.CheckRunGroups (CheckRunGroups, GroupTracker, groupFlusher, newGroupTracker, noCheckRunGroups, parseCheckRunGroups)
import Lib.Bridge.HydraToGitHub (handleHydraNotification, queueCheckRun, toHydraNotification)
import Lib.Bridge.JobsetAllowlist (noJobsetAllowlist, parseJobsetAllowlist)
import Lib.Bridge.PublicUrl (normalizePublicBase)
import Lib.Bridge.Source (parseRepoFlakeDirs)
import Lib.Bridge.Sync (SyncConfig (..), syncLoop)
import Lib.Connection (guardedGetNotification, mkConnectionString, withReconnect)
import Lib.GitHub (TokenLease (..), fetchAppInstallationToken, fetchInstallations, gitHubKey)
import Lib.GitHub.TokenCache (refreshTokensAt, retainConfiguredOwners)
import Lib.Hydra (HydraClientEnv (..))
import Lib.SSE (StatusCache, broadcastCheckRuns, newStatusCache, runSSEServer)
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv, lookupEnv)
import System.Exit (die)
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
  -- | Seconds without a notification before the connection is checked
  Int ->
  StatusCache ->
  Text ->
  String ->
  Text ->
  Bool ->
  CheckRunGroups ->
  GroupTracker ->
  Connection ->
  IO ()
notificationWatcherWithSSE notifTimeoutSec cache publicBase stateDir checkRunPrefix filterJobs groups tracker conn = do
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
    -- Nothing when the timeout passed and the connection is still alive; a
    -- dead connection throws ConnectionLost for withReconnect.
    mRaw <- guardedGetNotification conn (notifTimeoutSec * 1000000)
    forM_ mRaw $ \raw -> do
      let n = traceShowId raw
      case toHydraNotification n of
        Nothing -> putStrLn $ "Ignoring unrecognised notification: " ++ show n
        Just note -> do
          statuses <- handleHydraNotification conn publicBase stateDir checkRunPrefix filterJobs groups tracker note

          -- Broadcast to SSE subscribers before queuing for GitHub API delivery.
          broadcastCheckRuns cache statuses

          -- Queue for GitHub API delivery (existing behaviour).
          forM_ statuses (queueCheckRun conn)

-- | Fetch tokens only for app installations listed in GITHUB_APP_INSTALL_IDS.
-- Anyone can install a public GitHub App, so the discovered list may include
-- owners this bridge does not serve.
fetchGitHubTokens :: Int -> FilePath -> Text -> ByteString -> [(String, Int)] -> IO [(String, TokenLease)]
fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent ghAppInstallIds = do
  putStrLn "Fetching GitHub App installations..."
  allInstalls <- fetchInstallations ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent
  putStrLn $ "Found " <> show (length allInstalls) <> " installations"
  let configuredOwners = map (Text.pack . fst) ghAppInstallIds
      configured (owner, _) = owner `elem` configuredOwners
      ghAppInstalls = retainConfiguredOwners configuredOwners allInstalls
  forM_ allInstalls $ \inst@(owner, installId) -> do
    Text.putStrLn $
      "\t- "
        <> owner
        <> " ("
        <> Text.pack (show installId)
        <> ")"
        <> if configured inst then "" else " skipped: not in GITHUB_APP_INSTALL_IDS"

  forM ghAppInstalls $ \(owner, installId) -> do
    lease <- fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent installId
    Text.putStrLn $ "Fetched new GitHub App installation token valid for " <> owner <> " until " <> Text.pack (show lease.expiry)
    return (Text.unpack owner, lease)

getValidGitHubToken ::
  IORef [(String, TokenLease)] ->
  Text ->
  ByteString ->
  IO [(String, TokenLease)]
getValidGitHubToken ghTokens ghEndpointUrl ghUserAgent = do
  now <- getCurrentTime
  let buffer = 5 :: NominalDiffTime
  refreshTokensAt now buffer (.expiry) ghTokens $ \lease -> do
    putStrLn $ "GitHub token expired or will expire within the next " <> show buffer <> ", fetching a new one..."
    ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
    ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
    -- Renew the same installation that issued the lease.
    fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent lease.installationId

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Read environment variables
  host <- maybe "localhost" id <$> lookupEnv "HYDRA_HOST"
  publicUrl <- lookupEnv "HYDRA_PUBLIC_URL"
  publicBase <-
    either
      (\err -> die $ "Invalid Hydra public URL: " <> Text.unpack err)
      pure
      (normalizePublicBase $ Text.pack $ maybe host id publicUrl)
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
      Just k -> pure (C8.pack k)
      Nothing -> maybe mempty C8.pack <$> lookupEnv "KEY"
  checkRunPrefix <- maybe "ci/hydra-build:" Text.pack <$> lookupEnv "CHECK_RUN_PREFIX"
  filterJobs <- maybe True (\v -> v == "true" || v == "1") <$> lookupEnv "FILTER_JOBS"
  checkRunGroups <- maybe (pure noCheckRunGroups) (either fail pure . parseCheckRunGroups) =<< lookupEnv "CHECK_RUN_GROUPS"

  -- SSE configuration
  sseEnabled <- maybe True (\v -> v == "true" || v == "1") <$> lookupEnv "SSE_ENABLED"
  ssePort <- maybe 8812 read <$> lookupEnv "SSE_PORT"
  let sseTtl = 86400 :: NominalDiffTime -- 24 hours

  -- Sync configuration
  syncEnabled <- maybe True (\v -> v == "true" || v == "1") <$> lookupEnv "SYNC_ENABLED"
  syncInterval <- maybe 900 read <$> lookupEnv "SYNC_INTERVAL"
  syncDryRun <- maybe False (\v -> v == "true" || v == "1") <$> lookupEnv "SYNC_DRY_RUN"
  repoFlakeDirsRaw <- maybe "[]" id <$> lookupEnv "REPO_FLAKE_DIRS"
  repoFlakeDirs <- either die pure (parseRepoFlakeDirs repoFlakeDirsRaw)

  -- Optional per-repo jobset allowlist, e.g.
  -- {"simplex-chat/simplex-chat": ["pullrequest-7729"]}.  Listed repos only
  -- have the named jobsets touched by the webhooks and the sync.
  jobsetAllowlist <- do
    v <- lookupEnv "JOBSET_ALLOWLIST"
    case parseJobsetAllowlist <$> v of
      Nothing -> pure noJobsetAllowlist
      Just (Right allowlist) -> pure allowlist
      Just (Left e) -> die $ "Invalid JOBSET_ALLOWLIST: " <> e
  putStrLn $ "Jobset allowlist: " ++ show jobsetAllowlist

  -- Authenticate to GitHub and derive the set of allowed orgs from
  -- configured installations. Only webhooks from these orgs will be processed.
  ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
  ghAppInstallIds <- getEnv "GITHUB_APP_INSTALL_IDS" >>= return . read @[(String, Int)]
  ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
  ghTokens <- fetchGitHubTokens ghAppId ghAppKeyFile ghEndpointUrl ghUserAgent ghAppInstallIds >>= newIORef
  let allowedOrgs = Set.fromList $ map (Text.pack . fst) ghAppInstallIds
  putStrLn $ "Allowed orgs: " ++ show (Set.toList allowedOrgs)

  putStrLn $ "Connecting to Hydra at " <> host
  env <- hydraClientEnv (Text.pack host) api_user api_pass

  putStrLn $ "Server is starting on port " ++ show port
  when sseEnabled $
    putStrLn $
      "SSE server will start on port " ++ show ssePort
  when syncEnabled $
    putStrLn $
      "Sync worker will run every "
        ++ show syncInterval
        ++ "s"
        ++ if syncDryRun then " (DRY RUN)" else ""

  -- Initialize SSE status cache
  cache <- newStatusCache
  groupTracker <- newGroupTracker

  -- Start the app loop.  Each worker holds a row lock while it talks to
  -- GitHub, so more workers only help while sends are slow; GitHub's rate
  -- limit is shared by all of them.
  numWorkers <- maybe 2 read <$> lookupEnv "STATUS_WORKERS"
  notifTimeoutSec <- maybe 120 read <$> lookupEnv "NOTIFICATION_TIMEOUT_SEC"
  -- The LISTEN loops (status workers and the notification watcher) detect a
  -- dead connection and reconnect; the other components still crash the
  -- process, and systemd restarts it.
  let connStr = mkConnectionString (ConnectInfo db 5432 db_user db_pass "hydra")
  let getValidGitHubToken' = getValidGitHubToken ghTokens ghEndpointUrl ghUserAgent

  -- Create partial index for the optimized unsent-payload query.
  -- CONCURRENTLY cannot run inside a transaction, so we use a
  -- dedicated connection with autocommit.
  withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \migConn -> do
    putStrLn "Ensuring partial index idx_github_status_payload_unsent exists..."
    _ <-
      execute_
        migConn
        "CREATE INDEX CONCURRENTLY IF NOT EXISTS \
        \idx_github_status_payload_unsent \
        \ON github_status_payload (status_id, id DESC) \
        \WHERE sent IS NULL AND tries < 5"
    putStrLn "Index ready."

    -- The queue is drained in (priority, id) order (see 'queueCheckRun').
    -- Backfill the priority of the existing backlog when the column is
    -- first added; sent payloads keep the default.
    [Only hasPriority] <-
      query_
        migConn
        "SELECT EXISTS (SELECT 1 FROM information_schema.columns \
        \WHERE table_name = 'github_status_payload' AND column_name = 'priority')"
    unless hasPriority $ withTransaction migConn $ do
      putStrLn "Adding github_status_payload.priority..."
      _ <-
        execute_
          migConn
          "ALTER TABLE github_status_payload \
          \ADD COLUMN priority SMALLINT NOT NULL DEFAULT 2"
      backfilled <-
        execute_
          migConn
          "UPDATE github_status_payload p \
          \SET priority = CASE WHEN s.name LIKE 'ci/eval%' THEN 0 ELSE 1 END \
          \FROM github_status s \
          \WHERE s.id = p.status_id AND p.sent IS NULL \
          \  AND (s.name LIKE 'ci/eval%' OR s.name LIKE '%required')"
      putStrLn $ "Backfilled priority of " ++ show backfilled ++ " payload(s)."
    putStrLn "Ensuring partial index idx_github_status_payload_queue exists..."
    _ <-
      execute_
        migConn
        "CREATE INDEX CONCURRENTLY IF NOT EXISTS \
        \idx_github_status_payload_queue \
        \ON github_status_payload (priority, id) \
        \WHERE sent IS NULL AND tries < 5"
    putStrLn "Index ready."

    -- Earlier versions only marked the payload they sent, leaving older
    -- unsent payloads for the same status in the queue forever (the queue
    -- query skips them).  Mark them as sent so the index walk does not
    -- have to step over them.
    superseded <-
      execute_
        migConn
        "UPDATE github_status_payload p SET sent = NOW() \
        \WHERE p.sent IS NULL \
        \  AND EXISTS ( \
        \    SELECT 1 FROM github_status_payload p2 \
        \    WHERE p2.status_id = p.status_id AND p2.id > p.id AND p2.sent IS NULL \
        \  )"
    putStrLn $ "Marked " ++ show superseded ++ " superseded payload(s) as sent."

  Async.mapConcurrently_
    id
    $ [ Async.replicateConcurrently_
          numWorkers
          ( withReconnect connStr 5 $
              statusHandlers notifTimeoutSec ghEndpointUrl ghUserAgent getValidGitHubToken'
          ),
        withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn ->
          hydraClient env conn,
        -- Use the SSE-broadcasting variant of the notification watcher.
        withReconnect connStr 5 $
          notificationWatcherWithSSE notifTimeoutSec cache publicBase stateDir checkRunPrefix filterJobs checkRunGroups groupTracker,
        -- Report the check-run groups the watcher marked as out of date.
        withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn ->
          groupFlusher
            20
            publicBase
            checkRunPrefix
            checkRunGroups
            groupTracker
            (\runs -> broadcastCheckRuns cache runs >> mapM_ (queueCheckRun conn) runs)
            conn,
        withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn -> do
          run port (app allowedOrgs jobsetAllowlist repoFlakeDirs (hceClientEnv env) conn (gitHubKey ghKey)),
        -- Periodically prune stale notifications for old commits that
        -- have already been superseded by newer evaluations.  Only marks
        -- a payload as sent when a later payload for the same
        -- (owner, repo, name) has already been successfully delivered.
        withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn ->
          forever $ do
            threadDelay (5 * 60 * 1000000) -- 5 minutes
            pruned <-
              execute_
                conn
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
              putStrLn $
                "Pruned " ++ show pruned ++ " stale notification(s)"
      ]
      ++ [runSSEServer cache ssePort sseTtl | sseEnabled]
      ++ [ withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn ->
             syncLoop
               SyncConfig
                 { scInterval = syncInterval,
                   scDryRun = syncDryRun,
                   scEndpoint = ghEndpointUrl,
                   scUserAgent = ghUserAgent,
                   scTokensRef = ghTokens,
                   scGetTokens = getValidGitHubToken',
                   scJobsetAllowlist = jobsetAllowlist,
                   scRepoFlakeDirs = repoFlakeDirs
                 }
               conn
         | syncEnabled
         ]
