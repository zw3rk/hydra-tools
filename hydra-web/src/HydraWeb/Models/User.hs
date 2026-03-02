-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | User-related data types for the authentication system.
-- Covers users (GitHub-authenticated), sessions, and API tokens.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Models.User
  ( GFUser (..)
  , GFSession (..)
  , GFAPIToken (..)
  ) where

import Data.Text (Text)

-- | A frontend user, authenticated via GitHub OAuth.
data GFUser = GFUser
  { gfuId           :: !Int
  , gfuGitHubId     :: !Int          -- ^ Unique GitHub user ID
  , gfuGitHubLogin  :: !Text         -- ^ GitHub username
  , gfuDisplayName  :: !(Maybe Text) -- ^ Optional display name
  , gfuEmail        :: !(Maybe Text) -- ^ Optional email
  , gfuAvatarURL    :: !(Maybe Text) -- ^ Optional avatar URL
  , gfuIsSuperAdmin :: !Bool
  , gfuCreatedAt    :: !Int          -- ^ Unix timestamp
  , gfuUpdatedAt    :: !Int          -- ^ Unix timestamp
  } deriving (Show)

-- | A server-side browser session.
data GFSession = GFSession
  { gfsId           :: !Text   -- ^ 256-bit hex session ID
  , gfsUserId       :: !Int    -- ^ References gf_users
  , gfsCreatedAt    :: !Int    -- ^ Unix timestamp
  , gfsExpiresAt    :: !Int    -- ^ Unix timestamp
  , gfsLastActiveAt :: !Int    -- ^ Unix timestamp
  } deriving (Show)

-- | An API token for curl/machine access.
data GFAPIToken = GFAPIToken
  { gftId          :: !Int
  , gftUserId      :: !Int
  , gftLabel       :: !Text         -- ^ User-provided name
  , gftTokenHash   :: !Text         -- ^ SHA-256 hex (not returned to API)
  , gftTokenPrefix :: !Text         -- ^ First 14 chars (for UI display)
  , gftCreatedAt   :: !Int          -- ^ Unix timestamp
  , gftLastUsedAt  :: !(Maybe Int)  -- ^ Unix timestamp
  , gftExpiresAt   :: !(Maybe Int)  -- ^ Unix timestamp (Nothing = no expiry)
  } deriving (Show)
