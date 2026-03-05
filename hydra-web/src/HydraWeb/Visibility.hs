-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Visibility helpers for enforcing project/jobset access control.
-- Central module for access-control decisions:
--
--   * Phase 1: @hidden@ flag enforcement (hidden=1 → super-admin only)
--   * Phase 2: Private repo detection (private GitHub repo → authenticated users only)
--
-- Returns 404 (not 403) to avoid leaking the existence of hidden entities.
module HydraWeb.Visibility
  ( isSuperAdmin
  , isProjectAccessible
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)

import HydraWeb.DB.Projects (isProjectHidden)
import HydraWeb.DB.RepoVisibility (lookupProjectRepo, getRepoVisibility)
import HydraWeb.Models.User (GFUser (..))

-- | Check if the user is a super-admin. Anonymous users are not.
isSuperAdmin :: Maybe GFUser -> Bool
isSuperAdmin (Just u) = gfuIsSuperAdmin u
isSuperAdmin Nothing  = False

-- | Full visibility check for a project by name (Phase 1 + Phase 2).
-- Checks both the hidden flag and repo privacy.
--
-- Visibility rules:
--   1. hidden=1 → super-admin only
--   2. hidden=0, repo is public (or no mapping) → everyone
--   3. hidden=0, repo is private → authenticated users only
--   4. hidden=0, no cache entry → everyone (fail-open until cache is populated)
--
-- Returns @True@ if the user can see the project, @False@ if not.
isProjectAccessible :: Connection -> Text -> Maybe GFUser -> IO Bool
isProjectAccessible conn projectName mUser = do
  -- Phase 1: check hidden flag.
  hidden <- isProjectHidden conn projectName
  if hidden
    then pure (isSuperAdmin mUser)
    else do
      -- Phase 2: check repo privacy.
      mRepo <- lookupProjectRepo conn projectName
      case mRepo of
        Nothing -> pure True  -- no org/repo mapping → treat as public
        Just (org, repo) -> do
          mPublic <- getRepoVisibility conn org repo
          case mPublic of
            Nothing    -> pure True            -- no cache entry → fail-open
            Just True  -> pure True            -- public repo
            Just False -> pure (isAuthenticated mUser)  -- private → need auth

-- | Check if the user is authenticated (any logged-in user, not just admin).
isAuthenticated :: Maybe GFUser -> Bool
isAuthenticated Nothing  = False
isAuthenticated (Just _) = True
