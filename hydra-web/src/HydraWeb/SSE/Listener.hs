-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | SSE broadcaster for bridge status updates and nav-count refresher.
-- Two concurrent loops both broadcast to TopicBridges:
-- 1. PostgreSQL LISTEN — real-time on github_status/step_finished
-- 2. Periodic poll — 10-second nav-count refresh + 30-second bridge heartbeat
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.SSE.Listener
  ( listenAndBroadcast
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TVar, atomically, writeTVar)
import Control.Exception (SomeException, try, bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close, execute_)
import Database.PostgreSQL.Simple.Notification (getNotification)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import HydraWeb.DB.Bridges (bridgeFullStatus)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.Models.Queue (NavCounts)
import HydraWeb.SSE.Hub (Hub, Topic (..), broadcastTo)
import HydraWeb.View.Pages.Bridges (renderGitHubDataBS, renderAtticDataBS)

-- | Start both the PG LISTEN loop and the polling loop. Blocks forever.
-- Shutdown is handled by the parent thread via 'withAsync' cancellation.
-- Also refreshes the shared nav-counts TVar every ~10 seconds.
listenAndBroadcast :: Text -> Pool Connection -> Hub -> TVar NavCounts -> IO ()
listenAndBroadcast dbURL pool hub navVar =
  withAsync (pgListenLoop dbURL pool hub) $ \_ ->
    globalPollLoop pool hub navVar

-- ── PostgreSQL LISTEN loop ────────────────────────────────────────────

-- | LISTEN for bridge-related notifications and broadcast to TopicBridges.
-- Automatically reconnects on connection errors with 5s backoff.
-- Runs forever; shutdown via async cancellation.
pgListenLoop :: Text -> Pool Connection -> Hub -> IO ()
pgListenLoop dbURL pool hub = loop
  where
    loop = do
      result <- try (listenSession dbURL pool hub)
      case result of
        Right () -> pure ()
        Left (e :: SomeException) -> do
          hPutStrLn stderr $ "SSE listener error, reconnecting in 5s: " ++ show e
          threadDelay (5 * 1000000)
          loop

-- | Run a single LISTEN session on a dedicated connection.
listenSession :: Text -> Pool Connection -> Hub -> IO ()
listenSession dbURL pool hub =
  bracket (connectPostgreSQL (TE.encodeUtf8 dbURL)) close $ \listenConn -> do
    _ <- execute_ listenConn "LISTEN github_status"
    _ <- execute_ listenConn "LISTEN step_finished"
    hPutStrLn stderr "SSE listener connected: [github_status, step_finished]"

    -- Send an initial broadcast so clients get data immediately.
    broadcastBridgeStatus pool hub

    -- Main notification loop with 30s heartbeat timeout.
    let go = do
          mNotif <- timeout (30 * 1000000) (getNotification listenConn)
          case mNotif of
            Nothing -> do
              -- Heartbeat — broadcast bridge status periodically.
              broadcastBridgeStatus pool hub
              go
            Just _notif -> do
              -- Notification received — debounce by waiting 500ms.
              threadDelay (500 * 1000)
              drainNotifications listenConn
              broadcastBridgeStatus pool hub
              go
    go

-- | Drain pending notifications without blocking.
drainNotifications :: Connection -> IO ()
drainNotifications conn = do
  mNotif <- timeout 0 (getNotification conn)
  case mNotif of
    Just _  -> drainNotifications conn
    Nothing -> pure ()

-- ── Global polling loop ───────────────────────────────────────────────

-- | Periodic polling loop: refreshes nav-counts every ~10s and bridge
-- status every ~30s. The nav-count refresh is cheap (4 COUNT queries)
-- and keeps the TVar fresh for all handlers. Bridge status is heavier
-- so it runs at 3× the nav-count interval.
globalPollLoop :: Pool Connection -> Hub -> TVar NavCounts -> IO ()
globalPollLoop pool hub navVar = loop (0 :: Int)
  where
    loop tick = do
      -- Refresh nav counts every tick (~10s).
      result <- try $ refreshNavCounts pool navVar
      case result of
        Right () -> pure ()
        Left (e :: SomeException) ->
          hPutStrLn stderr $ "Nav-count refresh error: " ++ show e

      -- Broadcast bridge status every 3rd tick (~30s).
      if tick `mod` 3 == 0
        then do
          bResult <- try $ broadcastBridgeStatus pool hub
          case bResult of
            Right () -> pure ()
            Left (e :: SomeException) ->
              hPutStrLn stderr $ "Poll loop error: " ++ show e
        else pure ()

      threadDelay (10 * 1000000)
      loop (tick + 1)

-- | Query fresh nav counts and write them to the shared TVar.
refreshNavCounts :: Pool Connection -> TVar NavCounts -> IO ()
refreshNavCounts pool navVar = do
  nc <- withConn pool navCounts
  atomically $ writeTVar navVar nc

-- ── Broadcast helpers ─────────────────────────────────────────────────

-- | Query and broadcast bridge status to TopicBridges subscribers.
-- Sends two separate SSE events — one for each tab — so that tab
-- structure and display state are never disturbed.
broadcastBridgeStatus :: Pool Connection -> Hub -> IO ()
broadcastBridgeStatus pool hub = do
  status <- withConn pool bridgeFullStatus
  broadcastTo hub TopicBridges (formatSSE "github-bridge-update" (renderGitHubDataBS status))
  broadcastTo hub TopicBridges (formatSSE "attic-bridge-update" (renderAtticDataBS status))


-- | Format an HTML fragment as an SSE event.
-- event: <eventName>\ndata: <line1>\ndata: <line2>\n...\n\n
formatSSE :: ByteString -> ByteString -> ByteString
formatSSE eventName html = BS8.unlines $
  ("event: " <> eventName) :
  map ("data: " <>) (BS8.lines html) ++
  [""]
