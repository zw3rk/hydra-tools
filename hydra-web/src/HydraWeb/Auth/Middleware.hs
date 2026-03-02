-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | WAI middleware for extracting authenticated users from requests.
-- Supports two authentication methods:
--   1. Session cookie (browser sessions)
--   2. Bearer token (API tokens)
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Auth.Middleware
  ( authMiddleware
  , getUserFromRequest
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
import Network.Wai (Middleware, Request (..), vault)
import Numeric (showHex)
import qualified Data.Vault.Lazy as Vault

import HydraWeb.Auth.Session (getSessionUser, sessionCookieName)
import HydraWeb.DB.Auth (getAPITokenByHash, touchAPIToken, getGFUserById)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.Models.User (GFUser)

-- | WAI middleware that extracts user from session cookie or Bearer token
-- and stores it in the request vault.
authMiddleware :: Pool Connection -> Vault.Key (Maybe GFUser) -> Middleware
authMiddleware pool key app req respond = do
  mUser <- getUserFromRequest pool req
  let vault' = Vault.insert key mUser (vault req)
      req'   = req { vault = vault' }
  app req' respond

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
