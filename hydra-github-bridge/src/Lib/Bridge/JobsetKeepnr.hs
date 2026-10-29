{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | How many evaluations Hydra keeps the outputs of for pull request
-- jobsets, per repository.
--
-- The bridge writes the whole jobset on every create or update, so a
-- @keepnr@ changed in Hydra's UI or database is put back to the bridge's
-- value on the PR's next push.  Repositories whose PRs build a lot (each
-- haskell.nix evaluation roots hundreds of GiB of compilers) can keep fewer.
module Lib.Bridge.JobsetKeepnr
  ( PrJobsetKeepnr,
    noPrJobsetKeepnr,
    parsePrJobsetKeepnr,
    prJobsetKeepnr,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

-- | @keepnr@ per lower-cased @owner/repo@.
newtype PrJobsetKeepnr = PrJobsetKeepnr (Map Text Int)
  deriving (Show)

noPrJobsetKeepnr :: PrJobsetKeepnr
noPrJobsetKeepnr = PrJobsetKeepnr Map.empty

-- | Parse @PR_JOBSET_KEEPNR@, e.g. @[(\"ican-group/haskell.nix\", 1)]@.
parsePrJobsetKeepnr :: String -> Either String PrJobsetKeepnr
parsePrJobsetKeepnr raw =
  case readMaybe raw :: Maybe [(String, Int)] of
    Nothing -> Left "PR_JOBSET_KEEPNR: expected a Haskell list of (\"owner/repo\", keepnr) pairs"
    Just entries
      | any ((< 0) . snd) entries -> Left "PR_JOBSET_KEEPNR: keepnr must not be negative"
      | otherwise -> Right . PrJobsetKeepnr $ Map.fromList [(Text.toLower (Text.pack repo), n) | (repo, n) <- entries]

-- | The @keepnr@ for a jobset of @owner/repo@, if it is a pull request
-- jobset of a configured repository.
prJobsetKeepnr :: PrJobsetKeepnr -> Text -> Text -> Maybe Int
prJobsetKeepnr (PrJobsetKeepnr m) repoName jobsetName
  | "pullrequest-" `Text.isPrefixOf` jobsetName = Map.lookup (Text.toLower repoName) m
  | otherwise = Nothing
