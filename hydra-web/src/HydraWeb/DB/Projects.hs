-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for projects and jobsets.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Projects
  ( visibleProjects
  , allProjects
  , getProject
  , jobsetOverview
  ) where

import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))

import HydraWeb.Models.Project (Project (..))

-- | Fetch all visible (non-hidden) projects.
visibleProjects :: Connection -> IO [Project]
visibleProjects conn = do
  rows <- query_ conn [sql|
    SELECT name, displayname, description, enabled, hidden, owner, homepage
    FROM projects
    WHERE hidden = 0
    ORDER BY enabled DESC, name
  |]
  pure $ map toProject rows
  where
    toProject (name, displayname, desc, enabled, hidden, owner, homepage) =
      Project name displayname desc enabled hidden owner homepage []

-- | Fetch all projects (including hidden).
allProjects :: Connection -> IO [Project]
allProjects conn = do
  rows <- query_ conn [sql|
    SELECT name, displayname, description, enabled, hidden, owner, homepage
    FROM projects
    ORDER BY enabled DESC, name
  |]
  pure $ map toProject rows
  where
    toProject (name, displayname, desc, enabled, hidden, owner, homepage) =
      Project name displayname desc enabled hidden owner homepage []

-- | Fetch a single project by name, with its jobset names.
getProject :: Connection -> String -> IO (Maybe Project)
getProject conn name = do
  rows <- query conn [sql|
    SELECT name, displayname, description, enabled, hidden, owner, homepage
    FROM projects
    WHERE name = ?
  |] (Only name)
  case rows of
    [] -> pure Nothing
    ((pname, displayname, desc, enabled, hidden, owner, homepage):_) -> do
      jrows <- query conn [sql|
        SELECT name FROM jobsets WHERE project = ? ORDER BY name
      |] (Only name)
      let jobsets = map (\(Only jn) -> jn) jrows
      pure $ Just $ Project pname displayname desc enabled hidden owner homepage jobsets

-- | Fetch jobsets for a project with build count summaries.
jobsetOverview :: Connection -> String -> IO [()]
jobsetOverview _conn _project = do
  -- TODO: implement full jobset overview query with correlated subqueries
  pure []
