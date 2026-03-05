-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Background worker that checks GitHub repo visibility (public/private)
-- and caches the result in @gf_repo_visibility_cache@.
--
-- Uses GitHub App installation tokens to query @GET /repos/{owner}/{repo}@.
-- Runs periodically to keep the cache fresh.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraWeb.GitHub.RepoCheck
  ( repoVisibilityLoop
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecode, (.:))
import Data.Aeson.Types (parseMaybe, withObject)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Pool (Pool, withResource)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import GitHub.REST.Auth (fromToken)
import Network.HTTP.Client (Manager, Request (..), httpLbs, parseRequest_,
                            responseBody, responseStatus)
import Network.HTTP.Types (statusCode)

import HydraLib.GitHub.Client (fetchAppInstallationToken, gitHubApiVersion)
import HydraLib.GitHub.Types (TokenLease (..))

import HydraWeb.DB.Installations (Installation (..), listInstallations)
import HydraWeb.DB.RepoVisibility (allProjectRepos, upsertRepoVisibility)

-- | Background loop that refreshes the repo visibility cache.
-- Checks all repos in @gf_org_project_map@ against the GitHub API.
repoVisibilityLoop :: Pool Connection -> Manager -> Int -> FilePath -> ByteString -> Int -> IO ()
repoVisibilityLoop pool mgr interval ghAppKeyFile ghUserAgent ghAppId = do
  putStrLn "[repo-visibility] Background visibility checker started"
  loop
  where
    loop = do
      -- Sleep first, then check.
      threadDelay (interval * 1000000)
      result <- try (refreshAll pool mgr ghAppKeyFile ghUserAgent ghAppId)
      case result of
        Right (checked, errors) ->
          putStrLn $ "[repo-visibility] Refresh complete: "
            <> show checked <> " repo(s) checked, "
            <> show errors <> " error(s)"
        Left (e :: SomeException) ->
          putStrLn $ "[repo-visibility] ERROR: " <> show e
      loop

-- | Refresh visibility for all repos in @gf_org_project_map@.
refreshAll :: Pool Connection -> Manager -> FilePath -> ByteString -> Int -> IO (Int, Int)
refreshAll pool mgr ghAppKeyFile ghUserAgent ghAppId = withResource pool $ \conn -> do
  -- Get all org/repo pairs that need checking.
  mappings <- allProjectRepos conn
  let uniqueRepos = Set.toList $ Set.fromList [(org, repo) | (org, repo, _proj) <- mappings]

  -- Get enabled installations from DB for token generation.
  installations <- listInstallations conn
  let enabledInstalls = [(instOrgName i, instInstallationId i)
                        | i <- installations, instEnabled i]

  -- Fetch installation tokens for each org (one API call per org).
  orgTokens <- fetchOrgTokens ghAppKeyFile ghUserAgent ghAppId enabledInstalls

  -- Check each repo.
  results <- mapM (checkAndStore conn mgr ghUserAgent orgTokens) uniqueRepos
  let checked = length (filter id results)
      errors = length (filter not results)
  pure (checked, errors)

-- | Fetch installation tokens for each enabled org.
-- Returns (org, auth-header-value) pairs.
fetchOrgTokens :: FilePath -> ByteString -> Int -> [(Text, Int)] -> IO [(Text, ByteString)]
fetchOrgTokens ghAppKeyFile ghUserAgent ghAppId installs = do
  let ghEndpoint = "https://api.github.com" :: Text
  results <- mapM (\(org, installId) -> do
    result <- try (fetchAppInstallationToken ghEndpoint ghAppId ghAppKeyFile ghUserAgent installId)
    case result of
      Right lease -> pure (Just (org, fromToken (HydraLib.GitHub.Types.token lease)))
      Left (e :: SomeException) -> do
        putStrLn $ "[repo-visibility] Failed to get token for " <> Text.unpack org <> ": " <> show e
        pure Nothing
    ) installs
  pure [x | Just x <- results]

-- | Check a single repo's visibility via GitHub API and store in cache.
checkAndStore :: Connection -> Manager -> ByteString -> [(Text, ByteString)]
             -> (Text, Text) -> IO Bool
checkAndStore conn mgr ghUserAgent orgTokens (org, repo) =
  case find (\(o, _) -> o == org) orgTokens of
    Nothing -> do
      -- No token for this org → assume public (can't verify).
      upsertRepoVisibility conn org repo True
      pure True
    Just (_, authHeader) -> do
      result <- try (checkRepo mgr ghUserAgent authHeader org repo)
      case result of
        Right (Just isPublic) -> do
          upsertRepoVisibility conn org repo isPublic
          pure True
        Right Nothing -> do
          putStrLn $ "[repo-visibility] Could not parse visibility for "
            <> Text.unpack org <> "/" <> Text.unpack repo
          pure False
        Left (e :: SomeException) -> do
          putStrLn $ "[repo-visibility] Error checking "
            <> Text.unpack org <> "/" <> Text.unpack repo <> ": " <> show e
          pure False

-- | Call @GET /repos/{owner}/{repo}@ and parse the @private@ field.
-- Returns @Just True@ if public, @Just False@ if private.
checkRepo :: Manager -> ByteString -> ByteString -> Text -> Text -> IO (Maybe Bool)
checkRepo mgr ghUserAgent authHeader owner repo = do
  let url = "https://api.github.com/repos/" <> Text.unpack owner <> "/" <> Text.unpack repo
      req = (parseRequest_ url)
        { requestHeaders =
            [ ("Authorization", authHeader)
            , ("Accept", "application/vnd.github+json")
            , ("User-Agent", ghUserAgent)
            , ("X-GitHub-Api-Version", TE.encodeUtf8 $ TE.decodeUtf8 gitHubApiVersion)
            ]
        }
  resp <- httpLbs req mgr
  let status = statusCode (responseStatus resp)
  if status == 200
    then case eitherDecode (responseBody resp) of
      Right val -> pure $ parseMaybe (withObject "repo" $ \o -> do
        private <- o .: "private"
        pure (not (private :: Bool))
        ) val
      Left _ -> pure Nothing
    else if status == 404
      -- 404 means we can't see the repo → treat as private.
      then pure (Just False)
      else pure Nothing
