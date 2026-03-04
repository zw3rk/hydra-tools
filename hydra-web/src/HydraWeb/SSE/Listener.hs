-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | SSE broadcaster with two concurrent loops:
-- 1. PostgreSQL LISTEN for real-time bridge notifications
-- 2. Periodic polling for queue/machines/evals/nav counts
-- Both loops broadcast to topic-specific subscribers via the Hub.
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
import HydraWeb.View.Pages.Bridges (renderBridgesContentBS)
import HydraWeb.SSE.Fragments (renderNavCountsBS, renderQueueContentBS,
                                renderMachinesContentBS, renderRunningEvalsBS)

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

    -- Main notification loop with 10s heartbeat timeout.
    let go = do
          alive <- readIORef running
          if not alive then pure ()
          else do
            mNotif <- timeout (10 * 1000000) (getNotification listenConn)
            case mNotif of
              Nothing -> do
                -- Heartbeat — broadcast unconditionally.
                broadcastBridgeStatus pool hub
                go
              Just _notif -> do
                -- Notification received — debounce by waiting 500ms.
                threadDelay (500 * 1000)
                drainNotifications listenConn
                broadcastBridgeStatus pool hub
                -- Also piggyback queue/machines updates on step_finished.
                broadcastQueueStatus pool hub
                broadcastMachinesStatus pool hub
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

-- | Periodic polling loop for nav counts, queue, machines, and running evals.
-- Runs every 10 seconds.
globalPollLoop :: Pool Connection -> Hub -> IORef Bool -> IO ()
globalPollLoop pool hub running = loop
  where
    loop = do
      alive <- readIORef running
      if not alive then pure ()
      else do
        -- Poll and broadcast all topic-specific content.
        result <- try $ do
          broadcastNavCounts pool hub
          broadcastQueueStatus pool hub
          broadcastMachinesStatus pool hub
          broadcastRunningEvals pool hub
        case result of
          Right () -> pure ()
          Left (e :: SomeException) ->
            hPutStrLn stderr $ "Poll loop error: " ++ show e

        -- Sleep 10 seconds between polls.
        threadDelay (10 * 1000000)
        loop

-- ── Broadcast helpers ─────────────────────────────────────────────────

-- | Query and broadcast bridge status to TopicBridges subscribers.
broadcastBridgeStatus :: Pool Connection -> Hub -> IO ()
broadcastBridgeStatus pool hub = do
  status <- withConn pool bridgeFullStatus
  let html = renderBridgesContentBS status
      sseMsg = formatSSE "bridge-update" html
  broadcastTo hub TopicBridges sseMsg

-- | Query and broadcast nav counts to TopicGlobal (all subscribers).
broadcastNavCounts :: Pool Connection -> Hub -> IO ()
broadcastNavCounts pool hub = do
  bs <- withConn pool renderNavCountsBS
  let sseMsg = formatSSE "nav-update" bs
  broadcastTo hub TopicGlobal sseMsg

-- | Query and broadcast queue status to TopicQueue subscribers.
broadcastQueueStatus :: Pool Connection -> Hub -> IO ()
broadcastQueueStatus pool hub = do
  bs <- withConn pool renderQueueContentBS
  let sseMsg = formatSSE "queue-update" bs
  broadcastTo hub TopicQueue sseMsg

-- | Query and broadcast machine status to TopicMachines subscribers.
broadcastMachinesStatus :: Pool Connection -> Hub -> IO ()
broadcastMachinesStatus pool hub = do
  bs <- withConn pool renderMachinesContentBS
  let sseMsg = formatSSE "machines-update" bs
  broadcastTo hub TopicMachines sseMsg

-- | Query and broadcast running evals to TopicRunningEvals subscribers.
broadcastRunningEvals :: Pool Connection -> Hub -> IO ()
broadcastRunningEvals pool hub = do
  bs <- withConn pool renderRunningEvalsBS
  let sseMsg = formatSSE "running-evals-update" bs
  broadcastTo hub TopicRunningEvals sseMsg

-- | Format an HTML fragment as an SSE event.
-- event: <eventName>\ndata: <line1>\ndata: <line2>\n...\n\n
formatSSE :: ByteString -> ByteString -> ByteString
formatSSE eventName html = BS8.unlines $
  ("event: " <> eventName) :
  map ("data: " <>) (BS8.lines html) ++
  [""]
