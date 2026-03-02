-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database schema migration for gf_* tables (auth system).
-- Creates tables if they don't already exist. Safe to run repeatedly.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Migrate
  ( runMigrations
  ) where

import Database.PostgreSQL.Simple (Connection, execute_)
import Database.PostgreSQL.Simple.SqlQQ (sql)

-- | Run all migrations. Each CREATE TABLE uses IF NOT EXISTS,
-- so this is safe to call on every startup.
runMigrations :: Connection -> IO ()
runMigrations conn = do
  -- Users authenticated via GitHub.
  _ <- execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS gf_users (
      id              serial PRIMARY KEY,
      github_id       bigint UNIQUE NOT NULL,
      github_login    text NOT NULL,
      display_name    text,
      email           text,
      avatar_url      text,
      is_super_admin  boolean NOT NULL DEFAULT false,
      created_at      timestamptz NOT NULL DEFAULT now(),
      updated_at      timestamptz NOT NULL DEFAULT now()
    )
  |]
  -- GitHub user access tokens (encrypted at rest).
  _ <- execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS gf_github_tokens (
      user_id          integer PRIMARY KEY REFERENCES gf_users(id) ON DELETE CASCADE,
      access_token_enc bytea NOT NULL,
      obtained_at      timestamptz NOT NULL DEFAULT now()
    )
  |]
  -- Browser sessions (server-side).
  _ <- execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS gf_sessions (
      id              text PRIMARY KEY,
      user_id         integer NOT NULL REFERENCES gf_users(id) ON DELETE CASCADE,
      created_at      timestamptz NOT NULL DEFAULT now(),
      expires_at      timestamptz NOT NULL,
      last_active_at  timestamptz NOT NULL DEFAULT now()
    )
  |]
  _ <- execute_ conn [sql|
    CREATE INDEX IF NOT EXISTS idx_gf_sessions_user ON gf_sessions(user_id)
  |]
  _ <- execute_ conn [sql|
    CREATE INDEX IF NOT EXISTS idx_gf_sessions_expires ON gf_sessions(expires_at)
  |]
  -- API tokens for curl/machine access.
  _ <- execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS gf_api_tokens (
      id              serial PRIMARY KEY,
      user_id         integer NOT NULL REFERENCES gf_users(id) ON DELETE CASCADE,
      label           text NOT NULL,
      token_hash      text UNIQUE NOT NULL,
      token_prefix    text NOT NULL,
      created_at      timestamptz NOT NULL DEFAULT now(),
      last_used_at    timestamptz,
      expires_at      timestamptz
    )
  |]
  _ <- execute_ conn [sql|
    CREATE INDEX IF NOT EXISTS idx_gf_api_tokens_user ON gf_api_tokens(user_id)
  |]
  _ <- execute_ conn [sql|
    CREATE INDEX IF NOT EXISTS idx_gf_api_tokens_hash ON gf_api_tokens(token_hash)
  |]
  -- Cached GitHub repo access checks.
  _ <- execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS gf_repo_access_cache (
      user_id         integer NOT NULL REFERENCES gf_users(id) ON DELETE CASCADE,
      owner           text NOT NULL,
      repo            text NOT NULL,
      has_access      boolean NOT NULL,
      checked_at      timestamptz NOT NULL DEFAULT now(),
      PRIMARY KEY (user_id, owner, repo)
    )
  |]
  -- Project-to-GitHub repo mapping.
  _ <- execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS gf_project_repos (
      project_name    text NOT NULL,
      jobset_name     text NOT NULL,
      github_owner    text NOT NULL,
      github_repo     text NOT NULL,
      is_public       boolean,
      detected_at     timestamptz NOT NULL DEFAULT now(),
      PRIMARY KEY (project_name, jobset_name, github_owner, github_repo)
    )
  |]
  -- Repo public/private cache.
  _ <- execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS gf_repo_visibility_cache (
      owner           text NOT NULL,
      repo            text NOT NULL,
      is_public       boolean NOT NULL,
      checked_at      timestamptz NOT NULL DEFAULT now(),
      PRIMARY KEY (owner, repo)
    )
  |]
  pure ()
