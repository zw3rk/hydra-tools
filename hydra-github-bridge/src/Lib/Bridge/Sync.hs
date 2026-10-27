-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Periodic sync between GitHub PR state and Hydra jobsets.
--
-- When webhook delivery fails, open PRs may lack jobsets (missed creation)
-- or closed/merged PRs may retain enabled jobsets (missed closure). This
-- module runs a periodic reconciliation loop that:
--
--   1. Discovers org/repo mappings from the @gf_org_project_map@ table
--   2. Fetches open PRs from GitHub
--   3. Compares against enabled @pullrequest-*@ jobsets in Hydra
--   4. Refreshes stale immutable PR sources without changing jobset settings
--   5. Queues create/disable/evaluation commands via the existing pipeline
module Lib.Bridge.Sync
  ( SyncConfig (..),
    SyncStats (..),
    PRInfo (..),
    EnabledPRJobset (..),
    syncLoop,
    reconcileAll,
    reconcileProject,
    fetchOpenPRs,
    fetchEnabledPRJobsets,
    fetchOrgProjectMappings,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO)
import Data.Aeson (Value, (.:), (.:?))
import Data.Aeson.Types (parseMaybe, withObject)
import Data.ByteString (ByteString)
import Data.Char (isHexDigit)
import Data.IORef (IORef)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple (Connection, Only (..), query, query_, withTransaction)
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue (..),
    StdMethod (..),
  )
import GitHub.REST.PageLinks (PageLinks)
import Lib.Bridge.JobsetAllowlist (JobsetAllowlist, isJobsetAllowed)
import Lib.Exception (tryNonAsync)
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
    ssJobsetsRefreshed :: !Int,
    ssErrors :: !Int
  }
  deriving (Show, Eq)

instance Semigroup SyncStats where
  a <> b =
    SyncStats
      { ssProjectsSynced = ssProjectsSynced a + ssProjectsSynced b,
        ssJobsetsCreated = ssJobsetsCreated a + ssJobsetsCreated b,
        ssJobsetsDisabled = ssJobsetsDisabled a + ssJobsetsDisabled b,
        ssJobsetsRefreshed = ssJobsetsRefreshed a + ssJobsetsRefreshed b,
        ssErrors = ssErrors a + ssErrors b
      }

instance Monoid SyncStats where
  mempty = SyncStats 0 0 0 0 0

-- | The fields needed to recognize an existing bridge-managed PR source.
-- All other jobset settings remain in Hydra, rather than being reconstructed
-- from defaults when a missed synchronize event is repaired.
data EnabledPRJobset = EnabledPRJobset
  { ejName :: !Text,
    ejType :: !Int,
    ejFlake :: !(Maybe Text)
  }
  deriving (Show, Eq)

-- | Configuration for the sync loop.
data SyncConfig = SyncConfig
  { -- | Seconds between sync cycles
    scInterval :: !Int,
    -- | Log actions without queueing commands
    scDryRun :: !Bool,
    -- | GitHub API endpoint URL
    scEndpoint :: !Text,
    -- | GitHub user-agent header
    scUserAgent :: !ByteString,
    -- | Shared mutable token cache (unused directly but kept for reference)
    scTokensRef :: !(IORef [(String, TokenLease)]),
    -- | Action to refresh tokens when expired
    scGetTokens :: !(IO [(String, TokenLease)]),
    -- | Jobsets the sync may create or disable, per repo
    scJobsetAllowlist :: !JobsetAllowlist
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
          <> show (ssJobsetsRefreshed stats)
          <> " source(s) refreshed, "
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
  result <- tryNonAsync (reconcileProject cfg conn mapping)
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

  -- Snapshot Hydra BEFORE asking GitHub. A webhook arriving while GitHub is
  -- being read must win over this snapshot's conditional source update.
  enabledJobsets <- fetchEnabledPRJobsets conn project
  openPRs <- fetchOpenPRs cfg org repo

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

      -- Whether the jobset allowlist lets the sync touch this PR's jobset.
      allowed :: Int -> Bool
      allowed n = isJobsetAllowed (scJobsetAllowlist cfg) repoFullName ("pullrequest-" <> Text.pack (show n))

      -- PRs open on GitHub but missing a jobset in Hydra.
      missing :: [PRInfo]
      missing =
        [ pr
        | n <- Set.toList (githubSet `Set.difference` hydraSet),
          allowed n,
          Just pr <- [Map.lookup n openPRMap]
        ]

      -- Jobset PR numbers enabled in Hydra but not open on GitHub.
      stale :: [Int]
      stale = filter allowed $ Set.toList (hydraSet `Set.difference` githubSet)

      -- Enabled jobsets of open PRs whose source no longer matches the
      -- PR head.
      refreshes =
        [ (jobset, newFlake)
        | n <- Set.toList (githubSet `Set.intersection` hydraSet),
          allowed n,
          Just pr <- [Map.lookup n openPRMap],
          Just jobset <- [Map.lookup n enabledJobsets],
          Just newFlake <- [sourceRefreshTarget repoFullName pr jobset]
        ]

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
      <> " stale, "
      <> show (length refreshes)
      <> " source(s) outdated"

  refreshed <- sum <$> mapM (refreshJobsetSource cfg conn project) refreshes

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
        ssJobsetsRefreshed = refreshed,
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
    Nothing ->
      -- An unavailable PR listing is not an empty PR listing: returning []
      -- would disable every existing PR jobset and report a successful cycle.
      throwIO $ userError $ "No GitHub token for org " <> owner <> "; reconciliation skipped"
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
  -- A deleted fork can have a null head.repo. Treat that, or any malformed
  -- entry, as an incomplete listing: silently omitting it would disable a
  -- still-open PR. All pages are read before reconciliation writes anything.
  parsed <- maybe (throwIO $ userError "Incomplete GitHub pull request listing") pure (traverse parsePR prs)
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
--
-- Mappings whose Hydra project is disabled are skipped: reconciling one
-- would re-enable (and re-trigger) its PR jobsets.  That is how a repo
-- transferred to a new owner ends up building every PR twice, once in the
-- old project and once in the new one, since hydra-web re-detects the old
-- mapping from the old jobsets' flake URLs.  A mapping whose project does
-- not exist yet is kept, so new repos still get their project created.
fetchOrgProjectMappings :: Connection -> IO [(Text, Text, Text)]
fetchOrgProjectMappings conn = do
  result <-
    tryNonAsync
      ( query_
          conn
          "SELECT m.project_name, m.org_name, m.repo_name FROM gf_org_project_map m \
          \LEFT JOIN projects p ON p.name = m.project_name \
          \WHERE p.name IS NULL OR p.enabled <> 0"
      )
  case result of
    Right rows -> pure rows
    Left (e :: SomeException) -> do
      putStrLn $ "[sync] Could not query gf_org_project_map (table may not exist): " <> show e
      pure []

-- | Fetch enabled @pullrequest-*@ jobsets for a given Hydra project.
-- Returns a map from PR number to its current source snapshot.
fetchEnabledPRJobsets :: Connection -> Text -> IO (Map Int EnabledPRJobset)
fetchEnabledPRJobsets conn project = do
  rows <-
    query
      conn
      "SELECT name, type, flake FROM jobsets WHERE project = ? AND name LIKE 'pullrequest-%' AND enabled > 0"
      (Only project)
  pure $
    Map.fromList
      [ (n, EnabledPRJobset name typ flake)
      | (name, typ, flake) <- rows,
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

-- | Only rewrite the immutable, root-directory GitHub sources emitted by
-- the bridge. Custom sources (including URL parameters) and legacy jobsets
-- are deliberately left alone. Preserve whether creation used the base
-- repository or the PR's head repository.
sourceRefreshTarget :: Text -> PRInfo -> EnabledPRJobset -> Maybe Text
sourceRefreshTarget repoFullName pr jobset
  | ejType jobset /= 1 = Nothing
  | ejName jobset /= "pullrequest-" <> Text.pack (show $ prNumber pr) = Nothing
  | otherwise = do
      oldFlake <- ejFlake jobset
      let prefixes = ["github:" <> repo <> "/" | repo <- [repoFullName, prRepoFullName pr]]
      prefix <- find (\p -> maybe False immutableSha (Text.stripPrefix p oldFlake)) prefixes
      let newFlake = prefix <> prHeadSha pr
      if immutableSha (prHeadSha pr) && newFlake /= oldFlake then Just newFlake else Nothing
  where
    immutableSha sha = Text.length sha == 40 && Text.all isHexDigit sha

-- | Compare-and-swap only the source, then queue its ordinary evaluation in
-- the SAME transaction. A newer webhook/config edit or a disable/delete wins
-- over the old snapshot, and a failed enqueue rolls the source update back.
-- This does not impose global ordering on already-queued webhook commands.
refreshJobsetSource :: SyncConfig -> Connection -> Text -> (EnabledPRJobset, Text) -> IO Int
refreshJobsetSource cfg conn project (jobset, newFlake)
  | scDryRun cfg = do
      putStrLn $ "[sync] DRY-RUN: would refresh source " <> Text.unpack project <> "/" <> Text.unpack (ejName jobset)
      pure 1
  | otherwise = do
      refreshed <- withTransaction conn $ do
        changed <-
          query
            conn
            "UPDATE jobsets SET flake = ? WHERE project = ? AND name = ? AND enabled > 0 AND type = 1 AND flake = ? RETURNING name"
            (newFlake, project, ejName jobset, ejFlake jobset) ::
            IO [Only Text]
        case changed of
          [] -> pure 0
          [_] -> do
            Hydra.writeCommand conn (Hydra.EvaluateJobset project (ejName jobset) False)
            pure 1
          _ -> throwIO $ userError "Multiple jobsets matched a source refresh"
      putStrLn $
        (if refreshed == 1 then "[sync] Refreshed source " else "[sync] Source changed concurrently; skipped ")
          <> Text.unpack project
          <> "/"
          <> Text.unpack (ejName jobset)
      pure refreshed

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
