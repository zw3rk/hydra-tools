-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Org/repo mapping queries for the gf_org_project_map table.
-- Maps Hydra project names to GitHub org/repo pairs, either auto-detected
-- from flake URLs or manually overridden via admin panel.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.DB.OrgMap
  ( OrgMapping (..)
  , lookupByOrgRepo
  , lookupByProject
  , listMappings
  , autoDetectMappings
  , upsertMapping
  ) where

import Control.Exception (SomeException, catch)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (Connection, query, query_, execute, Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

-- | An org/repo mapping record.
data OrgMapping = OrgMapping
  { omProjectName  :: !Text
  , omOrgName      :: !Text
  , omRepoName     :: !Text
  , omAutoDetected :: !Bool
  , omCreatedAt    :: !Int
  } deriving (Show, Eq)

-- | Lookup the project name for a given org/repo pair.
-- Returns Nothing if no mapping exists.
lookupByOrgRepo :: Connection -> Text -> Text -> IO (Maybe Text)
lookupByOrgRepo conn org repo = do
  rows <- query conn [sql|
    SELECT project_name FROM gf_org_project_map
    WHERE org_name = ? AND repo_name = ?
  |] (org, repo)
  case rows of
    []         -> pure Nothing
    (Only p:_) -> pure (Just p)

-- | Lookup the org/repo pair for a given project name.
-- Returns Nothing if no mapping exists.
lookupByProject :: Connection -> Text -> IO (Maybe (Text, Text))
lookupByProject conn projectName = do
  rows <- query conn [sql|
    SELECT org_name, repo_name FROM gf_org_project_map
    WHERE project_name = ?
  |] (Only projectName)
  case rows of
    []          -> pure Nothing
    ((o, r):_)  -> pure (Just (o, r))

-- | List all org/repo mappings ordered by org then repo.
listMappings :: Connection -> IO [OrgMapping]
listMappings conn = do
  rows <- query_ conn [sql|
    SELECT project_name, org_name, repo_name, auto_detected,
           extract(epoch from created_at)::int
    FROM gf_org_project_map
    ORDER BY org_name, repo_name
  |]
  pure $ map toMapping rows
  where
    toMapping (pn, org, repo, auto, ca) =
      OrgMapping pn org repo auto ca

-- | Auto-detect org/repo mappings by scanning jobsets.flake fields
-- and falling back to parsing the project name (e.g. "zw3rk-repo" → zw3rk/repo).
-- Only overwrites auto-detected entries (manual overrides are preserved).
autoDetectMappings :: Connection -> IO ()
autoDetectMappings conn = do
  -- Phase 1: Parse flake URIs from jobsets.
  flakeRows <- query_ conn [sql|
    SELECT DISTINCT j.project, j.flake
    FROM jobsets j
    WHERE j.flake IS NOT NULL AND j.flake LIKE 'github:%'
  |] `catch` (\(_ :: SomeException) -> pure [])
  mapM_ processFlakeRow flakeRows

  -- Phase 2: For projects without mappings, try parsing the project name
  -- using known org names from gf_github_installations.  This avoids the
  -- ambiguity of a naive first-dash split for orgs like "input-output-hk".
  knownOrgs <- map (\(Only o) -> o) <$>
    (query_ conn [sql|
      SELECT org_name FROM gf_github_installations
    |] `catch` (\(_ :: SomeException) -> pure []))
  unmapped <- query_ conn [sql|
    SELECT p.name FROM projects p
    WHERE NOT EXISTS (
      SELECT 1 FROM gf_org_project_map m WHERE m.project_name = p.name
    )
  |] `catch` (\(_ :: SomeException) -> pure [])
  mapM_ (processNameRow knownOrgs) unmapped
  where
    processFlakeRow :: (Text, Text) -> IO ()
    processFlakeRow (projectName, flake) =
      case parseGitHubFlake flake of
        Just (org, repo) ->
          upsertMapping conn projectName org repo True
        Nothing -> pure ()

    processNameRow :: [Text] -> Only Text -> IO ()
    processNameRow knownOrgs (Only projectName) =
      case parseProjectName knownOrgs projectName of
        Just (org, repo) ->
          upsertMapping conn projectName org repo True
        Nothing -> pure ()

-- | Parse a project name into (org, repo) using known org names.
-- Tries each known org as a prefix (e.g. "input-output-hk-devx" with
-- known org "input-output-hk" → ("input-output-hk", "devx")).
-- Falls back to first-dash split for single-segment orgs.
parseProjectName :: [Text] -> Text -> Maybe (Text, Text)
parseProjectName knownOrgs name =
  -- First try matching against known org prefixes (handles multi-dash orgs
  -- like "input-output-hk").
  case listToMaybe $ mapMaybe tryOrg knownOrgs of
    Just result -> Just result
    -- Fallback: split on first dash for unknown orgs.
    Nothing -> case Text.breakOn "-" name of
      (org, rest)
        | not (Text.null org) && not (Text.null rest) ->
            Just (org, Text.drop 1 rest)
        | otherwise -> Nothing
  where
    tryOrg :: Text -> Maybe (Text, Text)
    tryOrg org =
      case Text.stripPrefix (org <> "-") name of
        Just repo | not (Text.null repo) -> Just (org, repo)
        _ -> Nothing

-- | Parse a GitHub flake URI into (org, repo).
-- Handles formats: "github:org/repo", "github:org/repo/ref",
-- "github:org/repo?ref=...", "github:org/repo/ref?dir=..."
parseGitHubFlake :: Text -> Maybe (Text, Text)
parseGitHubFlake flake = do
  rest <- Text.stripPrefix "github:" flake
  -- Strip query params.
  let pathPart = fst $ Text.breakOn "?" rest
  -- Split on "/" to get org/repo[/ref].
  case Text.splitOn "/" pathPart of
    (org:repo:_) | not (Text.null org) && not (Text.null repo) ->
      Just (org, repo)
    _ -> Nothing

-- | Upsert an org/repo mapping. Manual overrides (auto_detected=false)
-- are not replaced by auto-detection.
upsertMapping :: Connection -> Text -> Text -> Text -> Bool -> IO ()
upsertMapping conn projectName org repo autoDetected = do
  _ <- execute conn [sql|
    INSERT INTO gf_org_project_map (project_name, org_name, repo_name, auto_detected)
    VALUES (?, ?, ?, ?)
    ON CONFLICT (project_name)
    DO UPDATE SET org_name = EXCLUDED.org_name,
                  repo_name = EXCLUDED.repo_name,
                  auto_detected = EXCLUDED.auto_detected
    WHERE gf_org_project_map.auto_detected = true OR EXCLUDED.auto_detected = false
  |] (projectName, org, repo, autoDetected)
  pure ()

