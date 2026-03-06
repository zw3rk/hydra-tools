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
  , isAuthenticated
  , isProjectAccessible
  , filterByProjectAccess
  , filterByRepoAccess
    -- * Pure decision logic (exported for testing)
  , accessDecision
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Database.PostgreSQL.Simple (Connection)

import HydraWeb.DB.Projects (isProjectHidden)
import HydraWeb.DB.OrgMap (lookupByOrgRepo)
import HydraWeb.DB.RepoVisibility (lookupProjectRepo, getRepoVisibility)
import HydraWeb.Models.User (GFUser (..))

-- | Check if the user is a super-admin. Anonymous users are not.
isSuperAdmin :: Maybe GFUser -> Bool
isSuperAdmin (Just u) = gfuIsSuperAdmin u
isSuperAdmin Nothing  = False

-- | Pure access-control decision given pre-fetched inputs.
--
-- @accessDecision isHidden mRepoPublic isAuthed isSuperAdmin'@
--
--   * @isHidden@     — project has hidden=1 in the DB
--   * @mRepoPublic@  — 'Nothing' if no repo mapping or no cache entry (fail-open),
--                       'Just True' if public, 'Just False' if private
--   * @isAuthed@     — user is logged in (any role)
--   * @isSuperAdmin'@ — user is a super-admin
--
-- Returns 'True' if the user should be granted access.
accessDecision :: Bool -> Maybe Bool -> Bool -> Bool -> Bool
accessDecision _isHidden _mRepoPublic _isAuthed True = True   -- super-admin sees everything
accessDecision True      _            _         _    = False  -- hidden → deny non-admin
accessDecision False     Nothing      _         _    = True   -- no mapping / no cache → fail-open
accessDecision False     (Just True)  _         _    = True   -- public repo → everyone
accessDecision False     (Just False) isAuthed  _    = isAuthed  -- private → need auth

-- | Full visibility check for a project by name (Phase 1 + Phase 2).
-- Fetches hidden flag and repo visibility from the DB, then delegates
-- to the pure 'accessDecision'.
isProjectAccessible :: Connection -> Text -> Maybe GFUser -> IO Bool
isProjectAccessible conn projectName mUser = do
  hidden <- isProjectHidden conn projectName
  mRepoPublic <- if hidden
    then pure Nothing  -- skip Phase 2 lookup for hidden projects
    else do
      mRepo <- lookupProjectRepo conn projectName
      case mRepo of
        Nothing         -> pure Nothing
        Just (org, repo) -> getRepoVisibility conn org repo
  pure $ accessDecision hidden mRepoPublic (isAuthenticated mUser) (isSuperAdmin mUser)

-- | Batch-filter a list of items by project visibility.
-- Checks each unique project name once, caching results for efficiency.
-- Use when filtering lists of builds, queue entries, etc.
filterByProjectAccess :: Connection -> Maybe GFUser -> (a -> Text) -> [a] -> IO [a]
filterByProjectAccess conn mUser getProject items = do
  -- Super-admins see everything.
  if isSuperAdmin mUser
    then pure items
    else do
      -- Collect unique project names and check each once.
      let projects = Map.fromList [(getProject item, ()) | item <- items]
      accessMap <- Map.traverseWithKey
        (\projName () -> isProjectAccessible conn projName mUser) projects
      pure [item | item <- items, Map.findWithDefault False (getProject item) accessMap]

-- | Filter items by GitHub (owner, repo) visibility.
-- Maps each unique (owner, repo) → Hydra project name via gf_org_project_map,
-- then checks project accessibility. Items with no project mapping pass
-- through (fail-open, consistent with other visibility behavior).
filterByRepoAccess :: Connection -> Maybe GFUser
                   -> (a -> Text) -> (a -> Text)  -- ^ getOwner, getRepo
                   -> [a] -> IO [a]
filterByRepoAccess conn mUser getOwner getRepo items = do
  if isSuperAdmin mUser
    then pure items
    else do
      -- Collect unique (owner, repo) pairs, check each once.
      let repos = Map.fromList [((getOwner item, getRepo item), ()) | item <- items]
      accessMap <- Map.traverseWithKey
        (\(owner, repo) () -> do
          mProj <- lookupByOrgRepo conn owner repo
          case mProj of
            Nothing       -> pure True  -- no mapping → fail-open
            Just projName -> isProjectAccessible conn projName mUser
        ) repos
      pure [item | item <- items
                 , Map.findWithDefault True (getOwner item, getRepo item) accessMap]

-- | Check if the user is authenticated (any logged-in user, not just admin).
isAuthenticated :: Maybe GFUser -> Bool
isAuthenticated Nothing  = False
isAuthenticated (Just _) = True
