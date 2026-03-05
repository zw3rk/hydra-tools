-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for the repo visibility cache.
-- Uses @gf_repo_visibility_cache@ to store whether GitHub repos are public
-- or private, and @gf_org_project_map@ to look up which repo a project maps to.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.RepoVisibility
  ( getRepoVisibility
  , upsertRepoVisibility
  , lookupProjectRepo
  , allProjectRepos
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)

-- | Check the visibility cache for a repo. Returns @Just True@ if the repo
-- is known to be public, @Just False@ if private, or @Nothing@ if not cached.
getRepoVisibility :: Connection -> Text -> Text -> IO (Maybe Bool)
getRepoVisibility conn owner repo = do
  rows <- query conn [sql|
    SELECT is_public FROM gf_repo_visibility_cache
    WHERE owner = ? AND repo = ?
  |] (owner, repo)
  case rows of
    [Only p] -> pure (Just p)
    _        -> pure Nothing

-- | Insert or update the visibility cache for a repo.
upsertRepoVisibility :: Connection -> Text -> Text -> Bool -> IO ()
upsertRepoVisibility conn owner repo isPublic = do
  _ <- execute conn [sql|
    INSERT INTO gf_repo_visibility_cache (owner, repo, is_public, checked_at)
    VALUES (?, ?, ?, now())
    ON CONFLICT (owner, repo)
    DO UPDATE SET is_public = EXCLUDED.is_public, checked_at = now()
  |] (owner, repo, isPublic)
  pure ()

-- | Look up the GitHub org/repo for a Hydra project via @gf_org_project_map@.
-- Returns @Nothing@ if the project has no mapping (treated as public).
lookupProjectRepo :: Connection -> Text -> IO (Maybe (Text, Text))
lookupProjectRepo conn projectName = do
  rows <- query conn [sql|
    SELECT org_name, repo_name FROM gf_org_project_map
    WHERE project_name = ?
    LIMIT 1
  |] (Only projectName)
  case rows of
    [(org, repo)] -> pure (Just (org, repo))
    _             -> pure Nothing

-- | Fetch all project-to-repo mappings for bulk visibility refresh.
allProjectRepos :: Connection -> IO [(Text, Text, Text)]
allProjectRepos conn =
  query_ conn [sql|
    SELECT DISTINCT org_name, repo_name, project_name
    FROM gf_org_project_map
  |]
