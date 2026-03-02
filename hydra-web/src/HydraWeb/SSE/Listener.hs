-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | PostgreSQL LISTEN-based SSE broadcaster. Opens a dedicated
-- connection (not from pool) for LISTEN, subscribes to notification
-- channels, and broadcasts rendered bridge content to all SSE
-- subscribers with debouncing and heartbeat support.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.SSE.Listener
  ( listenAndBroadcast
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try, bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close, execute_)
import Database.PostgreSQL.Simple.Notification (getNotification)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import HydraWeb.DB.Bridges (bridgeFullStatus)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.Models.Bridge (BridgeStatus (..))
import HydraWeb.SSE.Hub (Hub, broadcast)
import HydraWeb.View.Pages.Bridges (renderBridgesContentBS)

-- | Start the LISTEN loop. Blocks until an unrecoverable error.
-- Automatically reconnects on connection errors with a 5s delay.
-- The IORef Bool is checked periodically; set to False to stop.
listenAndBroadcast :: Text -> Pool Connection -> Hub -> IORef Bool -> IO ()
listenAndBroadcast dbURL pool hub running = loop
  where
    loop = do
      alive <- readIORef running
      if not alive
        then pure ()
        else do
          result <- try (listenLoop dbURL pool hub running)
          case result of
            Right () -> pure () -- clean shutdown
            Left (e :: SomeException) -> do
              hPutStrLn stderr $ "SSE listener error, reconnecting in 5s: " ++ show e
              threadDelay (5 * 1000000)
              loop

-- | Run a single LISTEN session on a dedicated connection.
listenLoop :: Text -> Pool Connection -> Hub -> IORef Bool -> IO ()
listenLoop dbURL pool hub running =
  bracket (connectPostgreSQL (TE.encodeUtf8 dbURL)) close $ \listenConn -> do
    -- Subscribe to bridge notification channels.
    _ <- execute_ listenConn "LISTEN github_status"
    _ <- execute_ listenConn "LISTEN step_finished"
    hPutStrLn stderr "SSE listener connected: [github_status, step_finished]"

    -- Send an initial broadcast so clients get data immediately.
    broadcastBridgeStatus pool hub

    -- Debounce state: when we last broadcast.
    lastBroadcast <- newIORef (0 :: Int)

    -- Main notification loop with 10s heartbeat timeout.
    let go = do
          alive <- readIORef running
          if not alive
            then pure ()
            else do
              -- Wait for notification with 10s timeout (heartbeat).
              mNotif <- timeout (10 * 1000000) (getNotification listenConn)
              case mNotif of
                Nothing -> do
                  -- Heartbeat — broadcast unconditionally.
                  broadcastBridgeStatus pool hub
                  go
                Just _notif -> do
                  -- Notification received — debounce by waiting 500ms
                  -- for more notifications before broadcasting.
                  threadDelay (500 * 1000)
                  -- Drain any pending notifications (non-blocking).
                  drainNotifications listenConn
                  _ <- readIORef lastBroadcast
                  broadcastBridgeStatus pool hub
                  writeIORef lastBroadcast 1
                  go
    go

-- | Drain all pending notifications without blocking.
drainNotifications :: Connection -> IO ()
drainNotifications conn = do
  mNotif <- timeout 0 (getNotification conn)
  case mNotif of
    Just _  -> drainNotifications conn
    Nothing -> pure ()

-- | Query bridge status, render as HTML fragment, format as SSE event,
-- and broadcast to all subscribers.
broadcastBridgeStatus :: Pool Connection -> Hub -> IO ()
broadcastBridgeStatus pool hub = do
  status <- withConn pool $ \conn -> bridgeFullStatus conn
  let html = renderBridgesContentBS status
      sseMsg = formatSSE "bridge-update" html
  broadcast hub sseMsg

-- | Format an HTML fragment as an SSE event.
-- event: <eventName>\ndata: <line1>\ndata: <line2>\n...\n\n
formatSSE :: ByteString -> ByteString -> ByteString
formatSSE eventName html = BS8.unlines $
  ("event: " <> eventName) :
  map ("data: " <>) (BS8.lines html) ++
  [""]
