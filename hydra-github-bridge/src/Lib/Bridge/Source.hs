-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Bridge.Source
  ( RepoFlakeDirs,
    parseRepoFlakeDirs,
    validRepoPath,
    flakeSourceUri,
  )
where

import Data.Char (isControl)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.URI (urlEncode)
import Text.Read (readMaybe)

-- | Optional repository-specific Flake directories, keyed by @owner/repo@.
type RepoFlakeDirs = Map Text Text

-- | Read the Haskell association-list syntax used by the other bridge settings.
-- An empty value preserves the bridge's historical root-flake behavior.
parseRepoFlakeDirs :: String -> Either String RepoFlakeDirs
parseRepoFlakeDirs raw = do
  entries <- maybe (Left "expected a Haskell list of (owner/repo, directory) pairs") Right (readMaybe raw :: Maybe [(String, String)])
  if any (not . validRepo . fst) entries
    then Left "repository keys must have the form owner/repo"
    else
      if any (not . validRepoPath . snd) entries
        then Left "flake directories must be safe relative repository paths"
        else
          if Map.size values /= length entries
            then Left "repository flake directory keys must be unique"
            else Right values
  where
    values = Map.fromList [(Text.pack repo, Text.pack dir) | (repo, dir) <- entries]
    validRepo repo = case Text.splitOn "/" (Text.pack repo) of
      [owner, name] -> not (Text.null owner || Text.null name)
      _ -> False

-- | A directory must be a nonempty relative repository path with no empty,
-- dot, or parent segments, backslashes, or control characters.
validRepoPath :: String -> Bool
validRepoPath raw =
  let path = Text.pack raw
      segments = Text.splitOn "/" path
   in not (Text.null path)
        && not (Text.isPrefixOf "/" path)
        && not (Text.any (\c -> c == '\\' || isControl c) path)
        && all (\part -> not (Text.null part) && part /= "." && part /= "..") segments

-- | Emit the canonical immutable GitHub Flake URI for a repository and SHA.
-- Query values use percent encoding, including spaces and reserved
-- characters, while the default empty map keeps root URIs unchanged.
flakeSourceUri :: RepoFlakeDirs -> Text -> Text -> Text
flakeSourceUri dirs repo sha =
  "github:" <> repo <> "/" <> sha <> maybe "" ("?dir=" <>) encodedDir
  where
    encodedDir = decodeUtf8 . urlEncode False . encodeUtf8 <$> Map.lookup repo dirs
