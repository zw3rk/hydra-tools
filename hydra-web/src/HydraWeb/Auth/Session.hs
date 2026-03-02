-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Session lifecycle: creation, validation, cleanup.
-- Sessions are server-side with 7-day TTL and 5-minute touch interval.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Auth.Session
  ( -- * Session management
    generateSessionId
  , createSessionForUser
  , getSessionUser
  , clearSession
  , cleanupExpiredSessions
    -- * Constants
  , sessionCookieName
  , sessionTTL
  ) where

import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection)
import Numeric (showHex)

import HydraWeb.DB.Auth
  (createSession, getSession, touchSession, deleteSession,
   deleteExpiredSessions, getGFUserById)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.Models.User (GFUser (..), GFSession (..))

-- | Session cookie name.
sessionCookieName :: ByteString
sessionCookieName = "hydra_session"

-- | Session TTL in seconds (7 days).
sessionTTL :: Int
sessionTTL = 7 * 24 * 3600

-- | Touch interval in seconds (5 minutes).
-- Avoids excessive DB writes on every request.
touchInterval :: Int
touchInterval = 5 * 60

-- | Generate a cryptographically random 256-bit session ID, hex-encoded.
generateSessionId :: IO Text
generateSessionId = do
  bytes <- getRandomBytes 32 :: IO ByteString
  pure $ Text.pack $ concatMap (\b -> showHex' b) $ BS.unpack bytes
  where
    showHex' b = let s = showHex b "" in if length s == 1 then '0':s else s

-- | Create a new session for a user and return the session ID.
createSessionForUser :: Pool Connection -> Int -> IO Text
createSessionForUser pool userId = do
  sid <- generateSessionId
  now <- round <$> getPOSIXTime :: IO Int
  let expiresAt = now + sessionTTL
  withConn pool $ \conn -> createSession conn sid userId expiresAt
  pure sid

-- | Look up the user for a session cookie value. Touches the session
-- if enough time has elapsed since the last touch.
getSessionUser :: Pool Connection -> Text -> IO (Maybe GFUser)
getSessionUser pool sid = do
  now <- round <$> getPOSIXTime :: IO Int
  withConn pool $ \conn -> do
    mSession <- getSession conn sid
    case mSession of
      Nothing -> pure Nothing
      Just sess -> do
        -- Touch if stale (> 5 min since last activity).
        let elapsed = now - gfsLastActiveAt sess
        if elapsed > touchInterval
          then touchSession conn sid
          else pure ()
        getGFUserById conn (gfsUserId sess)

-- | Destroy a session by ID.
clearSession :: Pool Connection -> Text -> IO ()
clearSession pool sid = withConn pool $ \conn -> deleteSession conn sid

-- | Delete all expired sessions. Returns count deleted.
cleanupExpiredSessions :: Pool Connection -> IO Int
cleanupExpiredSessions pool = withConn pool deleteExpiredSessions
