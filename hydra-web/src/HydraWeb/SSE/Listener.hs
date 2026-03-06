-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | SSE broadcaster for bridge status updates.
-- Two concurrent loops both broadcast to TopicBridges:
-- 1. PostgreSQL LISTEN — real-time on github_status/step_finished
-- 2. Periodic poll — 30-second heartbeat for subscribers
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.SSE.Listener
  ( listenAndBroadcast
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, try, bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, readIORef)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close, execute_)
import Database.PostgreSQL.Simple.Notification (getNotification)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import HydraWeb.DB.Bridges (bridgeFullStatus)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.SSE.Hub (Hub, Topic (..), broadcastTo)
import HydraWeb.View.Pages.Bridges (renderGitHubDataBS, renderAtticDataBS)

-- | Start both the PG LISTEN loop and the polling loop. Blocks until shutdown.
listenAndBroadcast :: Text -> Pool Connection -> Hub -> IORef Bool -> IO ()
listenAndBroadcast dbURL pool hub running =
  withAsync (pgListenLoop dbURL pool hub running) $ \_ ->
    globalPollLoop pool hub running

-- ── PostgreSQL LISTEN loop ────────────────────────────────────────────

-- | LISTEN for bridge-related notifications and broadcast to TopicBridges.
-- Automatically reconnects on connection errors with 5s backoff.
pgListenLoop :: Text -> Pool Connection -> Hub -> IORef Bool -> IO ()
pgListenLoop dbURL pool hub running = loop
  where
    loop = do
      alive <- readIORef running
      if not alive then pure ()
      else do
        result <- try (listenSession dbURL pool hub running)
        case result of
          Right () -> pure ()
          Left (e :: SomeException) -> do
            hPutStrLn stderr $ "SSE listener error, reconnecting in 5s: " ++ show e
            threadDelay (5 * 1000000)
            loop

-- | Run a single LISTEN session on a dedicated connection.
listenSession :: Text -> Pool Connection -> Hub -> IORef Bool -> IO ()
listenSession dbURL pool hub running =
  bracket (connectPostgreSQL (TE.encodeUtf8 dbURL)) close $ \listenConn -> do
    _ <- execute_ listenConn "LISTEN github_status"
    _ <- execute_ listenConn "LISTEN step_finished"
    hPutStrLn stderr "SSE listener connected: [github_status, step_finished]"

    -- Send an initial broadcast so clients get data immediately.
    broadcastBridgeStatus pool hub

    -- Main notification loop with 30s heartbeat timeout.
    let go = do
          alive <- readIORef running
          if not alive then pure ()
          else do
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

-- | Periodic polling loop for bridge status updates.
-- Queue and machines pages no longer use SSE (they were replacing
-- full tables with single-line count summaries — a regression).
-- Runs every 30 seconds.
globalPollLoop :: Pool Connection -> Hub -> IORef Bool -> IO ()
globalPollLoop pool hub running = loop
  where
    loop = do
      alive <- readIORef running
      if not alive then pure ()
      else do
        result <- try $ broadcastBridgeStatus pool hub
        case result of
          Right () -> pure ()
          Left (e :: SomeException) ->
            hPutStrLn stderr $ "Poll loop error: " ++ show e

        threadDelay (30 * 1000000)
        loop

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
