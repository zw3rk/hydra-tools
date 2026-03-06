-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler-level auth helpers (requireAuth, requireSuperAdmin, getOptionalUser)
-- for use with Servant's Header "Cookie" Text extraction.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Auth.Middleware
  ( -- * Handler-level auth helpers
    requireAuth
  , requireSuperAdmin
  , getOptionalUser
  , extractSessionId
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import Servant (err401, err403, ServerError (..))

import HydraWeb.Auth.Session (getSessionUser, sessionCookieName)
import HydraWeb.Models.User (GFUser (..))

-- | Simple cookie parser: "name=value; name2=value2" → [(name, value)].
parseCookies :: ByteString -> [(ByteString, ByteString)]
parseCookies = map parsePair . BS8.split ';'
  where
    parsePair s = case BS8.break (== '=') (BS8.strip s) of
      (name, rest) -> (name, BS.drop 1 rest)

-- ── Handler-level auth helpers ─────────────────────────────────────

-- | Extract the session ID from a raw Cookie header value.
-- Parses "hydra_session=<sid>; ..." and returns the session ID.
extractSessionId :: Text -> Maybe Text
extractSessionId cookieStr =
  let cookies = parseCookies (TE.encodeUtf8 cookieStr)
  in  TE.decodeUtf8 <$> lookup sessionCookieName cookies

-- | Require an authenticated user. Returns the user or throws 401.
-- Takes a Maybe Cookie header value (from Servant's Header extraction).
requireAuth :: Pool Connection -> Maybe Text -> IO (Either ServerError GFUser)
requireAuth pool mCookie = do
  case mCookie >>= extractSessionId of
    Nothing -> pure $ Left err401
      { errBody = "Login required", errHeaders = [] }
    Just sid -> do
      mUser <- getSessionUser pool sid
      case mUser of
        Nothing -> pure $ Left err401
          { errBody = "Session expired", errHeaders = [] }
        Just user -> pure (Right user)

-- | Require a super-admin. Returns the user or throws 401/403.
requireSuperAdmin :: Pool Connection -> Maybe Text -> IO (Either ServerError GFUser)
requireSuperAdmin pool mCookie = do
  result <- requireAuth pool mCookie
  case result of
    Left err -> pure (Left err)
    Right user
      | gfuIsSuperAdmin user -> pure (Right user)
      | otherwise -> pure $ Left err403
          { errBody = "Super-admin access required", errHeaders = [] }

-- | Get the authenticated user if present, Nothing otherwise. Does not throw.
getOptionalUser :: Pool Connection -> Maybe Text -> IO (Maybe GFUser)
getOptionalUser pool mCookie = do
  result <- requireAuth pool mCookie
  pure $ either (const Nothing) Just result
