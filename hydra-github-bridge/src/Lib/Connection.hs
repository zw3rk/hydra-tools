{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL connection management with automatic dead-connection detection
-- and reconnection.
--
-- The core problem: @getNotification@ blocks indefinitely. If the underlying
-- TCP connection silently dies (e.g. network partition, firewall timeout),
-- the blocking call hangs forever with no error and no timeout.
--
-- This module provides:
--
-- * TCP keepalive parameters on the connection string so the kernel detects
--   dead connections within ~60s.
-- * A guarded @getNotification@ wrapper that periodically health-checks the
--   connection on timeout, distinguishing \"just quiet\" from \"actually dead\".
-- * A reconnection bracket that catches dead-connection exceptions and
--   re-establishes the connection automatically, without crashing the process.
module Lib.Connection
  ( ConnectionLost (..),
    withReconnect,
    guardedGetNotification,
    mkConnectionString,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception
  ( Exception,
    displayException,
    finally,
    throwIO,
    try,
  )
import Data.ByteString.Char8 (ByteString)
import Database.PostgreSQL.Simple
  ( ConnectInfo,
    Connection,
    Only,
    close,
    connectPostgreSQL,
    postgreSQLConnectionString,
    query_,
  )
import Database.PostgreSQL.Simple.Notification (Notification, getNotification)
import Lib.Exception (tryNonAsync)
import System.Timeout (timeout)

-- | Thrown when a PostgreSQL connection is detected as dead via health check.
-- Caught by 'withReconnect' to trigger automatic reconnection.
data ConnectionLost = ConnectionLost String
  deriving (Show)

instance Exception ConnectionLost

-- | Build a libpq connection string from 'ConnectInfo' with TCP keepalive
-- parameters for dead-connection detection.
--
-- Keepalive settings: first probe after 30s idle, retry every 10s, give up
-- after 3 failures. On Linux, @tcp_user_timeout@ provides a hard 30s upper
-- bound. Total worst-case detection time: ~60s.
--
-- Uses 'postgreSQLConnectionString' for proper escaping of credentials.
mkConnectionString :: ConnectInfo -> ByteString
mkConnectionString info =
  postgreSQLConnectionString info
    <> " keepalives=1"
    <> " keepalives_idle=30"
    <> " keepalives_interval=10"
    <> " keepalives_count=3"
    <> " tcp_user_timeout=30000"

-- | Run an action on a fresh connection, and run it again on a new
-- connection whenever it throws 'ConnectionLost'.
--
-- Only connecting is retried on other errors (@SqlError@ when the server
-- refuses connections, @IOException@ on network errors), so that a database
-- that is briefly down doesn't crash the process.  Any other exception from
-- the action itself propagates, as do async exceptions, so programming errors
-- still crash the process and @mapConcurrently_@ cancellation still works.
--
-- On reconnection, the action is re-entered from the top with a fresh
-- 'Connection', so any LISTEN registrations are re-issued by the caller.
withReconnect ::
  ByteString ->
  -- | Delay in seconds before reconnecting
  Int ->
  (Connection -> IO a) ->
  IO a
withReconnect connStr delaySec action = loop
  where
    loop = do
      conn <- connectWithRetry
      result <- try (action conn) `finally` close conn
      case result of
        Right a -> pure a
        Left (ConnectionLost reason) -> do
          putStrLn $ "Connection lost (" <> reason <> "), reconnecting in " <> show delaySec <> "s..."
          threadDelay (delaySec * 1000000)
          loop

    connectWithRetry = do
      result <- tryNonAsync (connectPostgreSQL connStr)
      case result of
        Right conn -> pure conn
        Left err -> do
          putStrLn $ "Connecting to PostgreSQL failed (" <> displayException err <> "), retrying in " <> show delaySec <> "s..."
          threadDelay (delaySec * 1000000)
          connectWithRetry

-- | Wait for a PostgreSQL notification with timeout and health check.
--
-- Returns @Just notification@ if one arrives within the timeout.
-- Returns @Nothing@ if the timeout fires but the connection is alive (just quiet).
-- Throws 'ConnectionLost' if waiting fails or the health check fails
-- (connection is dead).
--
-- The timeout (in microseconds) should be tuned to the expected notification
-- frequency. The health check uses a short (5s) @SELECT 1@ probe.
guardedGetNotification ::
  Connection ->
  -- | Timeout in microseconds
  Int ->
  IO (Maybe Notification)
guardedGetNotification conn timeoutUs = do
  -- A server restart surfaces here as an IOError (EOF on the socket).
  mRaw <- tryNonAsync $ timeout timeoutUs (getNotification conn)
  case mRaw of
    Left err -> throwIO $ ConnectionLost (displayException err)
    Right (Just raw) -> pure (Just raw)
    Right Nothing -> do
      -- Timed out — is the connection dead or just quiet?
      -- query_, not execute_: execute_ rejects a statement that returns rows.
      health <- tryNonAsync $ timeout (5 * 1000000) (query_ conn "SELECT 1" :: IO [Only Int])
      case health of
        Right (Just _) -> pure Nothing -- Connection alive, just quiet.
        Right Nothing -> throwIO $ ConnectionLost "health check timed out"
        Left err -> throwIO $ ConnectionLost (displayException err)
