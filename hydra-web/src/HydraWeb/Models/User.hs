-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
{-# LANGUAGE OverloadedStrings #-}

-- | User-related data types for the authentication system.
-- Covers users (GitHub-authenticated), sessions, and API tokens.
module HydraWeb.Models.User
  ( GFUser (..),
    GFSession (..),
    GFAPIToken (..),
  )
where

import Data.Text (Text)

-- | A frontend user, authenticated via GitHub OAuth.
data GFUser = GFUser
  { gfuId :: !Int,
    -- | Unique GitHub user ID
    gfuGitHubId :: !Int,
    -- | GitHub username
    gfuGitHubLogin :: !Text,
    -- | Optional display name
    gfuDisplayName :: !(Maybe Text),
    -- | Optional email
    gfuEmail :: !(Maybe Text),
    -- | Optional avatar URL
    gfuAvatarURL :: !(Maybe Text),
    gfuIsSuperAdmin :: !Bool,
    -- | Unix timestamp
    gfuCreatedAt :: !Int,
    -- | Unix timestamp
    gfuUpdatedAt :: !Int
  }
  deriving (Show)

-- | A server-side browser session.
data GFSession = GFSession
  { -- | 256-bit hex session ID
    gfsId :: !Text,
    -- | References gf_users
    gfsUserId :: !Int,
    -- | Unix timestamp
    gfsCreatedAt :: !Int,
    -- | Unix timestamp
    gfsExpiresAt :: !Int,
    -- | Unix timestamp
    gfsLastActiveAt :: !Int
  }
  deriving (Show)

-- | An API token for curl/machine access.
data GFAPIToken = GFAPIToken
  { gftId :: !Int,
    gftUserId :: !Int,
    -- | User-provided name
    gftLabel :: !Text,
    -- | SHA-256 hex (not returned to API)
    gftTokenHash :: !Text,
    -- | First 14 chars (for UI display)
    gftTokenPrefix :: !Text,
    -- | Unix timestamp
    gftCreatedAt :: !Int,
    -- | Unix timestamp
    gftLastUsedAt :: !(Maybe Int),
    -- | Unix timestamp (Nothing = no expiry)
    gftExpiresAt :: !(Maybe Int)
  }
  deriving (Show)
