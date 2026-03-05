-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | WAI middleware for extracting authenticated users from requests.
-- Supports two authentication methods:
--   1. Session cookie (browser sessions)
--   2. Bearer token (API tokens)
--
-- Also provides handler-level auth helpers (requireAuth, requireSuperAdmin)
-- for use with Servant's Header "Cookie" Text extraction.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Auth.Middleware
  ( -- * WAI request-level auth
    getUserFromRequest
    -- * Handler-level auth helpers
  , requireAuth
  , requireSuperAdmin
  , getOptionalUser
  , extractSessionId
  ) where

import Crypto.Hash (SHA256, Digest, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import Network.Wai (Request (..))
import Numeric (showHex)
import Servant (err401, err403, ServerError (..))

import HydraWeb.Auth.Session (getSessionUser, sessionCookieName)
import HydraWeb.DB.Auth (getAPITokenByHash, touchAPIToken, getGFUserById)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.Models.User (GFUser (..))

-- | Extract the authenticated user from a request.
-- Tries Bearer token first (for API clients), then session cookie.
getUserFromRequest :: Pool Connection -> Request -> IO (Maybe GFUser)
getUserFromRequest pool req = do
  -- Try Bearer token.
  case extractBearerToken req of
    Just token -> validateAPIToken pool token
    Nothing -> do
      -- Try session cookie.
      case extractSessionCookie req of
        Just sid -> getSessionUser pool sid
        Nothing  -> pure Nothing

-- | Extract "Bearer <token>" from the Authorization header.
extractBearerToken :: Request -> Maybe Text
extractBearerToken req =
  case lookup "Authorization" (requestHeaders req) of
    Just hdr ->
      let val = BS8.strip hdr
      in if BS8.isPrefixOf "Bearer " val
         then Just $ TE.decodeUtf8 (BS8.drop 7 val)
         else Nothing
    Nothing -> Nothing

-- | Extract the session ID from the session cookie.
extractSessionCookie :: Request -> Maybe Text
extractSessionCookie req = do
  cookieHeader <- lookup "Cookie" (requestHeaders req)
  let cookies = parseCookies cookieHeader
  val <- lookup sessionCookieName cookies
  Just $ TE.decodeUtf8 val

-- | Simple cookie parser: "name=value; name2=value2" → [(name, value)].
parseCookies :: ByteString -> [(ByteString, ByteString)]
parseCookies = map parsePair . BS8.split ';'
  where
    parsePair s = case BS8.break (== '=') (BS8.strip s) of
      (name, rest) -> (name, BS.drop 1 rest)

-- | Validate a Bearer API token. Hash it, look up in DB, return user.
validateAPIToken :: Pool Connection -> Text -> IO (Maybe GFUser)
validateAPIToken pool rawToken = do
  let tokenHash = hashToken rawToken
  withConn pool $ \conn -> do
    mResult <- getAPITokenByHash conn tokenHash
    case mResult of
      Nothing         -> pure Nothing
      Just (tid, uid) -> do
        touchAPIToken conn tid
        getGFUserById conn uid

-- | SHA-256 hash of a token, hex-encoded.
hashToken :: Text -> Text
hashToken t =
  let digest = hash (TE.encodeUtf8 t) :: Digest SHA256
      bytes  = BS.unpack (convert digest :: ByteString)
  in  TE.decodeUtf8 $ BS8.pack $ concatMap showHex' bytes
  where
    showHex' b = let s = showHex b "" in if length s == 1 then '0':s else s

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
