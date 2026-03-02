-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database operations for authentication: users, sessions, API tokens.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Auth
  ( -- * Users
    upsertGFUser
  , getGFUserById
  , getGFUserByGitHubLogin
  , listGFUsers
  , setSuperAdmin
  , bootstrapSuperAdmins
    -- * Sessions
  , createSession
  , getSession
  , touchSession
  , deleteSession
  , deleteExpiredSessions
    -- * GitHub tokens
  , upsertGitHubToken
  , getGitHubToken
    -- * API tokens
  , createAPIToken
  , getAPITokenByHash
  , touchAPIToken
  , listAPITokens
  , deleteAPIToken
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.Simple
  (Connection, query, query_, execute, Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import HydraWeb.Models.User (GFUser (..), GFSession (..), GFAPIToken (..))

-- ── Users ────────────────────────────────────────────────────────────

-- | Upsert a user by GitHub ID. Returns the user's internal ID.
upsertGFUser :: Connection -> Int -> Text -> Maybe Text -> Maybe Text
             -> Maybe Text -> IO Int
upsertGFUser conn ghId login displayName email avatarURL = do
  [Only uid] <- query conn [sql|
    INSERT INTO gf_users (github_id, github_login, display_name, email, avatar_url)
    VALUES (?, ?, ?, ?, ?)
    ON CONFLICT (github_id) DO UPDATE SET
      github_login = EXCLUDED.github_login,
      display_name = COALESCE(EXCLUDED.display_name, gf_users.display_name),
      email        = COALESCE(EXCLUDED.email, gf_users.email),
      avatar_url   = COALESCE(EXCLUDED.avatar_url, gf_users.avatar_url),
      updated_at   = now()
    RETURNING id
  |] (ghId, login, displayName, email, avatarURL)
  pure uid

-- | Fetch a user by internal ID.
getGFUserById :: Connection -> Int -> IO (Maybe GFUser)
getGFUserById conn uid = do
  rows <- query conn [sql|
    SELECT id, github_id, github_login, display_name, email, avatar_url,
           is_super_admin,
           EXTRACT(EPOCH FROM created_at)::int,
           EXTRACT(EPOCH FROM updated_at)::int
    FROM gf_users WHERE id = ?
  |] (Only uid)
  pure $ case rows of
    []    -> Nothing
    (r:_) -> Just (scanUser r)

-- | Fetch a user by GitHub login.
getGFUserByGitHubLogin :: Connection -> Text -> IO (Maybe GFUser)
getGFUserByGitHubLogin conn login = do
  rows <- query conn [sql|
    SELECT id, github_id, github_login, display_name, email, avatar_url,
           is_super_admin,
           EXTRACT(EPOCH FROM created_at)::int,
           EXTRACT(EPOCH FROM updated_at)::int
    FROM gf_users WHERE github_login = ?
  |] (Only login)
  pure $ case rows of
    []    -> Nothing
    (r:_) -> Just (scanUser r)

-- | List all users ordered by login.
listGFUsers :: Connection -> IO [GFUser]
listGFUsers conn = do
  rows <- query_ conn [sql|
    SELECT id, github_id, github_login, display_name, email, avatar_url,
           is_super_admin,
           EXTRACT(EPOCH FROM created_at)::int,
           EXTRACT(EPOCH FROM updated_at)::int
    FROM gf_users ORDER BY github_login
  |]
  pure $ map scanUser rows

scanUser :: (Int, Int, Text, Maybe Text, Maybe Text, Maybe Text, Bool, Int, Int)
         -> GFUser
scanUser (uid, ghId, login, dn, email, avatar, admin, created, updated) =
  GFUser uid ghId login dn email avatar admin created updated

-- | Set/unset super-admin flag for a user.
setSuperAdmin :: Connection -> Int -> Bool -> IO ()
setSuperAdmin conn uid isAdmin = do
  _ <- execute conn [sql|
    UPDATE gf_users SET is_super_admin = ?, updated_at = now() WHERE id = ?
  |] (isAdmin, uid)
  pure ()

-- | Ensure a list of GitHub logins are super-admins.
-- Used on startup for HYDRA_WEB_SUPER_ADMINS env var.
bootstrapSuperAdmins :: Connection -> [Text] -> IO ()
bootstrapSuperAdmins _ [] = pure ()
bootstrapSuperAdmins conn logins = do
  mapM_ (\login -> execute conn [sql|
    UPDATE gf_users SET is_super_admin = true, updated_at = now()
    WHERE github_login = ? AND NOT is_super_admin
  |] (Only login)) logins

-- ── Sessions ─────────────────────────────────────────────────────────

-- | Create a new session in the database.
createSession :: Connection -> Text -> Int -> Int -> IO ()
createSession conn sid userId expiresAt = do
  _ <- execute conn [sql|
    INSERT INTO gf_sessions (id, user_id, expires_at)
    VALUES (?, ?, to_timestamp(?))
  |] (sid, userId, expiresAt)
  pure ()

-- | Get a session by ID, if it hasn't expired.
getSession :: Connection -> Text -> IO (Maybe GFSession)
getSession conn sid = do
  rows <- query conn [sql|
    SELECT id, user_id,
           EXTRACT(EPOCH FROM created_at)::int,
           EXTRACT(EPOCH FROM expires_at)::int,
           EXTRACT(EPOCH FROM last_active_at)::int
    FROM gf_sessions
    WHERE id = ? AND expires_at > now()
  |] (Only sid)
  pure $ case rows of
    []    -> Nothing
    (r:_) -> Just (scanSession r)

scanSession :: (Text, Int, Int, Int, Int) -> GFSession
scanSession (sid, uid, created, expires, lastActive) =
  GFSession sid uid created expires lastActive

-- | Touch a session's last_active_at timestamp.
touchSession :: Connection -> Text -> IO ()
touchSession conn sid = do
  _ <- execute conn [sql|
    UPDATE gf_sessions SET last_active_at = now() WHERE id = ?
  |] (Only sid)
  pure ()

-- | Delete a session by ID.
deleteSession :: Connection -> Text -> IO ()
deleteSession conn sid = do
  _ <- execute conn [sql|
    DELETE FROM gf_sessions WHERE id = ?
  |] (Only sid)
  pure ()

-- | Delete all expired sessions.
deleteExpiredSessions :: Connection -> IO Int
deleteExpiredSessions conn = do
  n <- execute conn [sql|
    DELETE FROM gf_sessions WHERE expires_at <= now()
  |] ()
  pure (fromIntegral n)

-- ── GitHub tokens ────────────────────────────────────────────────────

-- | Store or replace an encrypted GitHub access token.
upsertGitHubToken :: Connection -> Int -> ByteString -> IO ()
upsertGitHubToken conn userId encToken = do
  _ <- execute conn [sql|
    INSERT INTO gf_github_tokens (user_id, access_token_enc)
    VALUES (?, ?)
    ON CONFLICT (user_id) DO UPDATE SET
      access_token_enc = EXCLUDED.access_token_enc,
      obtained_at = now()
  |] (userId, encToken)
  pure ()

-- | Get the encrypted GitHub token for a user.
getGitHubToken :: Connection -> Int -> IO (Maybe ByteString)
getGitHubToken conn userId = do
  rows <- query conn [sql|
    SELECT access_token_enc FROM gf_github_tokens WHERE user_id = ?
  |] (Only userId)
  pure $ case rows of
    []          -> Nothing
    (Only t:_)  -> Just t

-- ── API tokens ───────────────────────────────────────────────────────

-- | Create a new API token. Returns the token's internal ID.
createAPIToken :: Connection -> Int -> Text -> Text -> Text -> IO Int
createAPIToken conn userId label tokenHash tokenPrefix = do
  [Only tid] <- query conn [sql|
    INSERT INTO gf_api_tokens (user_id, label, token_hash, token_prefix)
    VALUES (?, ?, ?, ?)
    RETURNING id
  |] (userId, label, tokenHash, tokenPrefix)
  pure tid

-- | Look up an API token by its SHA-256 hash, if not expired.
getAPITokenByHash :: Connection -> Text -> IO (Maybe (Int, Int))
getAPITokenByHash conn hash = do
  rows <- query conn [sql|
    SELECT id, user_id FROM gf_api_tokens
    WHERE token_hash = ? AND (expires_at IS NULL OR expires_at > now())
  |] (Only hash)
  pure $ case rows of
    []        -> Nothing
    ((i,u):_) -> Just (i, u)

-- | Update last_used_at for a token.
touchAPIToken :: Connection -> Int -> IO ()
touchAPIToken conn tid = do
  _ <- execute conn [sql|
    UPDATE gf_api_tokens SET last_used_at = now() WHERE id = ?
  |] (Only tid)
  pure ()

-- | List all API tokens for a user (excluding hash for security).
listAPITokens :: Connection -> Int -> IO [GFAPIToken]
listAPITokens conn userId = do
  rows <- query conn [sql|
    SELECT id, user_id, label, '', token_prefix,
           EXTRACT(EPOCH FROM created_at)::int,
           EXTRACT(EPOCH FROM last_used_at)::int,
           EXTRACT(EPOCH FROM expires_at)::int
    FROM gf_api_tokens WHERE user_id = ?
    ORDER BY created_at DESC
  |] (Only userId)
  pure [GFAPIToken i u l h p c lu e | (i, u, l, h, p, c, lu, e) <- rows]

-- | Delete an API token.
deleteAPIToken :: Connection -> Int -> Int -> IO ()
deleteAPIToken conn tokenId userId = do
  _ <- execute conn [sql|
    DELETE FROM gf_api_tokens WHERE id = ? AND user_id = ?
  |] (tokenId, userId)
  pure ()
