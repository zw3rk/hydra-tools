-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Periodic sync between GitHub PR state and Hydra jobsets.
--
-- When webhook delivery fails, open PRs may lack jobsets (missed creation)
-- or closed/merged PRs may retain enabled jobsets (missed closure). This
-- module runs a periodic reconciliation loop that:
--
--   1. Discovers org/repo mappings from the @gf_org_project_map@ table
--   2. Fetches open PRs from GitHub
--   3. Compares against enabled @pullrequest-*@ jobsets in Hydra
--   4. Queues create/disable commands via the existing command pipeline
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Bridge.Sync
  ( SyncConfig (..),
    SyncStats (..),
    PRInfo (..),
    syncLoop,
    reconcileAll,
    reconcileProject,
    fetchOpenPRs,
    fetchEnabledPRJobsets,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (Value, (.:), (.:?))
import Data.Aeson.Types (parseMaybe, withObject)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple (Connection, Only (..), query, query_)
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue (..),
    StdMethod (..),
  )
import GitHub.REST.PageLinks (PageLinks)
import Lib.GitHub.Client
  ( GitHubRestConfig,
    TokenLease,
    gitHubApiVersion,
    gitHubRestConfig,
    queryGitHubRestPage,
  )
import Lib.GitHub.Client qualified as GHClient
import Lib.Hydra qualified as Hydra

-- | Per-PR metadata fetched from GitHub.
data PRInfo = PRInfo
  { prNumber :: !Int,
    prTitle :: !Text,
    prHeadSha :: !Text,
    prIsDraft :: !Bool,
    prRepoFullName :: !Text -- e.g. "owner/repo" (from head.repo.full_name)
  }
  deriving (Show, Eq)

-- | Cumulative statistics for a single sync cycle.
data SyncStats = SyncStats
  { ssProjectsSynced :: !Int,
    ssJobsetsCreated :: !Int,
    ssJobsetsDisabled :: !Int,
    ssErrors :: !Int
  }
  deriving (Show, Eq)

instance Semigroup SyncStats where
  a <> b =
    SyncStats
      { ssProjectsSynced = ssProjectsSynced a + ssProjectsSynced b,
        ssJobsetsCreated = ssJobsetsCreated a + ssJobsetsCreated b,
        ssJobsetsDisabled = ssJobsetsDisabled a + ssJobsetsDisabled b,
        ssErrors = ssErrors a + ssErrors b
      }

instance Monoid SyncStats where
  mempty = SyncStats 0 0 0 0

-- | Configuration for the sync loop.
data SyncConfig = SyncConfig
  { scInterval :: !Int,
    -- ^ Seconds between sync cycles
    scDryRun :: !Bool,
    -- ^ Log actions without queueing commands
    scEndpoint :: !Text,
    -- ^ GitHub API endpoint URL
    scUserAgent :: !ByteString,
    -- ^ GitHub user-agent header
    scTokensRef :: !(IORef [(String, TokenLease)]),
    -- ^ Shared mutable token cache (unused directly but kept for reference)
    scGetTokens :: !(IO [(String, TokenLease)])
    -- ^ Action to refresh tokens when expired
  }

-- | Repos where draft PRs should NOT create jobsets (replicates existing
-- webhook behaviour from 'pullRequestHook' in GitHubToHydra).
draftSkipRepos :: Set Text
draftSkipRepos =
  Set.fromList
    [ "IntersectMBO/ouroboros-network",
      "IntersectMBO/cardano-cli",
      "IntersectMBO/cardano-api"
    ]

------------------------------------------------------------------------
-- Sync loop
------------------------------------------------------------------------

-- | Outer loop: sleep for the configured interval, then reconcile.
-- Runs forever; errors in individual projects are isolated.
syncLoop :: SyncConfig -> Connection -> IO ()
syncLoop cfg conn = do
  putStrLn "[sync] Periodic sync worker started"
  loop
  where
    loop = do
      threadDelay (scInterval cfg * 1_000_000)
      putStrLn "[sync] Starting reconciliation cycle"
      stats <- reconcileAll cfg conn
      putStrLn $
        "[sync] Cycle complete: "
          <> show (ssProjectsSynced stats)
          <> " project(s), "
          <> show (ssJobsetsCreated stats)
          <> " created, "
          <> show (ssJobsetsDisabled stats)
          <> " disabled, "
          <> show (ssErrors stats)
          <> " error(s)"
      loop

------------------------------------------------------------------------
-- Reconciliation
------------------------------------------------------------------------

-- | Reconcile all projects discovered in the @gf_org_project_map@ table.
-- Falls back gracefully if the table doesn't exist (e.g. hydra-web has
-- not been deployed yet).
reconcileAll :: SyncConfig -> Connection -> IO SyncStats
reconcileAll cfg conn = do
  mappings <- fetchOrgProjectMappings conn
  case mappings of
    [] -> do
      putStrLn "[sync] No org/project mappings found; skipping"
      pure mempty
    ms -> do
      putStrLn $ "[sync] Found " <> show (length ms) <> " project mapping(s)"
      mconcat <$> mapM (reconcileOne cfg conn) ms

-- | Wrapper that isolates per-project errors.
reconcileOne :: SyncConfig -> Connection -> (Text, Text, Text) -> IO SyncStats
reconcileOne cfg conn mapping@(_project, org, repo) = do
  result <- try (reconcileProject cfg conn mapping)
  case result of
    Right stats -> pure stats
    Left (e :: SomeException) -> do
      putStrLn $
        "[sync] ERROR reconciling "
          <> Text.unpack org
          <> "/"
          <> Text.unpack repo
          <> ": "
          <> show e
      pure mempty {ssErrors = 1}

-- | Reconcile a single project: compare GitHub open PRs against Hydra
-- enabled jobsets and queue create/disable commands as needed.
--
-- Arguments are @(hydraProjectName, githubOrg, githubRepo)@.
reconcileProject :: SyncConfig -> Connection -> (Text, Text, Text) -> IO SyncStats
reconcileProject cfg conn (project, org, repo) = do
  let repoFullName = org <> "/" <> repo

  -- Fetch current state from both sides.
  openPRs <- fetchOpenPRs cfg org repo
  enabledJobsets <- fetchEnabledPRJobsets conn project

  let -- Build a map of open PR numbers to their info.
      openPRMap :: Map Int PRInfo
      openPRMap = Map.fromList [(prNumber pr, pr) | pr <- openPRs]

      -- Set of PR numbers that have enabled jobsets in Hydra.
      hydraSet :: Set Int
      hydraSet = Set.fromList $ Map.keys enabledJobsets

      -- Set of open PR numbers on GitHub (after draft filtering).
      githubSet :: Set Int
      githubSet =
        Set.fromList
          [ prNumber pr
            | pr <- openPRs,
              not (shouldSkipDraft repoFullName pr)
          ]

      -- PRs open on GitHub but missing a jobset in Hydra.
      missing :: [PRInfo]
      missing =
        [ pr
          | n <- Set.toList (githubSet `Set.difference` hydraSet),
            Just pr <- [Map.lookup n openPRMap]
        ]

      -- Jobset PR numbers enabled in Hydra but not open on GitHub.
      stale :: [Int]
      stale = Set.toList (hydraSet `Set.difference` githubSet)

  putStrLn $
    "[sync] "
      <> Text.unpack repoFullName
      <> ": "
      <> show (length openPRs)
      <> " open PR(s), "
      <> show (Map.size enabledJobsets)
      <> " enabled jobset(s), "
      <> show (length missing)
      <> " missing, "
      <> show (length stale)
      <> " stale"

  -- Queue creation commands for missing jobsets.
  created <-
    if null missing
      then pure 0
      else do
        mapM_ (createMissingJobset cfg conn project repoFullName) missing
        pure (length missing)

  -- Queue disable commands for stale jobsets.
  disabled <-
    if null stale
      then pure 0
      else do
        mapM_ (disableStaleJobset cfg conn project repoFullName) stale
        pure (length stale)

  pure
    SyncStats
      { ssProjectsSynced = 1,
        ssJobsetsCreated = created,
        ssJobsetsDisabled = disabled,
        ssErrors = 0
      }

-- | Check whether a draft PR should be skipped for a given repo.
shouldSkipDraft :: Text -> PRInfo -> Bool
shouldSkipDraft repoFullName pr =
  prIsDraft pr && repoFullName `Set.member` draftSkipRepos

------------------------------------------------------------------------
-- GitHub API
------------------------------------------------------------------------

-- | Fetch all open PRs for a repo, paginating through all results.
fetchOpenPRs :: SyncConfig -> Text -> Text -> IO [PRInfo]
fetchOpenPRs cfg org repo = do
  tokens <- scGetTokens cfg
  let owner = Text.unpack org
  case find ((owner ==) . fst) tokens of
    Nothing -> do
      putStrLn $ "[sync] No GitHub token for org " <> owner <> "; skipping PR fetch"
      pure []
    Just (_, lease) -> do
      let settings =
            GitHubSettings
              { token = Just (GHClient.token lease),
                userAgent = scUserAgent cfg,
                apiVersion = gitHubApiVersion
              }
      ghCfg <- gitHubRestConfig settings (scEndpoint cfg)
      fetchAllPages ghCfg org repo 1

-- | Paginated fetch of open PRs from the GitHub REST API.
fetchAllPages :: GitHubRestConfig -> Text -> Text -> Int -> IO [PRInfo]
fetchAllPages ghCfg org repo page = do
  let ep =
        GHEndpoint
          { method = GET,
            endpoint = "/repos/:owner/:repo/pulls",
            endpointVals =
              [ "owner" := org,
                "repo" := repo
              ],
            ghData =
              [ "state" := ("open" :: String),
                "per_page" := (100 :: Int),
                "page" := page
              ]
          }
  (prs, _links) <- queryGitHubRestPage ghCfg ep :: IO ([Value], PageLinks)
  let parsed = mapMaybe parsePR prs
  -- Continue if we got a full page of results (there may be more).
  if length prs < 100
    then pure parsed
    else do
      rest <- fetchAllPages ghCfg org repo (page + 1)
      pure (parsed ++ rest)

-- | Extract PR info from a GitHub API JSON object.
-- Returns Nothing for PRs whose head repo is null (deleted fork).
parsePR :: Value -> Maybe PRInfo
parsePR = parseMaybe $ withObject "PR" $ \obj -> do
  num <- obj .: "number"
  title <- obj .: "title"
  draft <- obj .: "draft"
  headObj <- obj .: "head"
  sha <- headObj .: "sha"
  -- head.repo can be null when the source fork has been deleted.
  mRepo <- headObj .:? "repo"
  repoObj <- maybe (fail "head.repo is null") pure mRepo
  fullName <- repoObj .: "full_name"
  pure
    PRInfo
      { prNumber = num,
        prTitle = title,
        prHeadSha = sha,
        prIsDraft = draft,
        prRepoFullName = fullName
      }

------------------------------------------------------------------------
-- Hydra DB
------------------------------------------------------------------------

-- | Fetch org/project mappings from the @gf_org_project_map@ table.
-- Returns @(project_name, org_name, repo_name)@ triples.
-- Gracefully returns empty list if the table doesn't exist.
fetchOrgProjectMappings :: Connection -> IO [(Text, Text, Text)]
fetchOrgProjectMappings conn = do
  result <-
    try
      ( query_ conn
          "SELECT project_name, org_name, repo_name FROM gf_org_project_map"
      )
  case result of
    Right rows -> pure rows
    Left (e :: SomeException) -> do
      putStrLn $ "[sync] Could not query gf_org_project_map (table may not exist): " <> show e
      pure []

-- | Fetch enabled @pullrequest-*@ jobsets for a given Hydra project.
-- Returns a map from PR number to jobset name.
fetchEnabledPRJobsets :: Connection -> Text -> IO (Map Int Text)
fetchEnabledPRJobsets conn project = do
  rows <-
    query
      conn
      "SELECT name FROM jobsets WHERE project = ? AND name LIKE 'pullrequest-%' AND enabled > 0"
      (Only project)
  pure $
    Map.fromList
      [ (n, name)
        | (Only name) <- rows,
          Just n <- [parsePRNumber name]
      ]

-- | Parse a PR number from a jobset name like @pullrequest-42@.
parsePRNumber :: Text -> Maybe Int
parsePRNumber name =
  case Text.stripPrefix "pullrequest-" name of
    Just rest -> case reads (Text.unpack rest) of
      [(n, "")] -> Just n
      _ -> Nothing
    Nothing -> Nothing

------------------------------------------------------------------------
-- Command generation
------------------------------------------------------------------------

-- | Queue a CreateOrUpdateJobset command for a missing PR jobset.
createMissingJobset :: SyncConfig -> Connection -> Text -> Text -> PRInfo -> IO ()
createMissingJobset cfg conn project repoFullName pr = do
  let jobsetName = "pullrequest-" <> Text.pack (show (prNumber pr))
      jobset =
        Hydra.defHydraFlakeJobset
          { Hydra.hjName = jobsetName,
            Hydra.hjDescription = "PR " <> Text.pack (show (prNumber pr)) <> ": " <> prTitle pr,
            Hydra.hjFlake = "github:" <> prRepoFullName pr <> "/" <> prHeadSha pr
          }
  if scDryRun cfg
    then putStrLn $ "[sync] DRY-RUN: would create " <> Text.unpack project <> "/" <> Text.unpack jobsetName
    else do
      putStrLn $ "[sync] Creating jobset " <> Text.unpack project <> "/" <> Text.unpack jobsetName
      Hydra.writeCommand conn (Hydra.CreateOrUpdateJobset repoFullName project jobsetName jobset)

-- | Queue an UpdateJobset command to disable a stale PR jobset.
disableStaleJobset :: SyncConfig -> Connection -> Text -> Text -> Int -> IO ()
disableStaleJobset cfg conn project repoFullName prNum = do
  let jobsetName = "pullrequest-" <> Text.pack (show prNum)
      jobset =
        Hydra.defHydraFlakeJobset
          { Hydra.hjName = jobsetName,
            Hydra.hjDescription = "PR " <> Text.pack (show prNum) <> " (closed)",
            Hydra.hjFlake = "github:" <> repoFullName <> "/0000000000000000000000000000000000000000",
            Hydra.hjVisible = False,
            Hydra.hjEnabled = 0
          }
  if scDryRun cfg
    then putStrLn $ "[sync] DRY-RUN: would disable " <> Text.unpack project <> "/" <> Text.unpack jobsetName
    else do
      putStrLn $ "[sync] Disabling jobset " <> Text.unpack project <> "/" <> Text.unpack jobsetName
      Hydra.writeCommand conn (Hydra.UpdateJobset repoFullName project jobsetName jobset)
