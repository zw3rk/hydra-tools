{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Optional per-repository allowlist of the jobsets the bridge may touch.
--
-- A repository listed here only has the named jobsets created, updated,
-- disabled or evaluated by the webhooks and the sync loop; every other
-- jobset of that repository is left alone.  Repositories that are not
-- listed behave as before.  Useful when most of a repository's jobsets
-- cannot currently evaluate and only one PR should be built.
module Lib.Bridge.JobsetAllowlist
  ( JobsetAllowlist,
    noJobsetAllowlist,
    parseJobsetAllowlist,
    isJobsetAllowed,
    whenJobsetAllowed,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

-- | Map from lower-cased @owner/repo@ to the jobset names allowed for it.
newtype JobsetAllowlist = JobsetAllowlist (Map Text (Set Text))
  deriving (Show, Eq)

noJobsetAllowlist :: JobsetAllowlist
noJobsetAllowlist = JobsetAllowlist Map.empty

-- | Parse a JSON object such as
-- @{"simplex-chat/simplex-chat": ["pullrequest-7729"]}@.
-- Repository names are matched case-insensitively.
parseJobsetAllowlist :: String -> Either String JobsetAllowlist
parseJobsetAllowlist s = do
  m <- Aeson.eitherDecode (LC8.pack s) :: Either String (Map Text [Text])
  pure . JobsetAllowlist $ Map.fromList [(Text.toLower repo, Set.fromList js) | (repo, js) <- Map.toList m]

-- | Whether the bridge may touch @jobsetName@ of @repoName@ (@owner/repo@).
isJobsetAllowed :: JobsetAllowlist -> Text -> Text -> Bool
isJobsetAllowed (JobsetAllowlist m) repoName jobsetName =
  maybe True (Set.member jobsetName) (Map.lookup (Text.toLower repoName) m)

-- | Run the action only if the jobset is allowed; log and skip otherwise.
whenJobsetAllowed :: JobsetAllowlist -> Text -> Text -> IO () -> IO ()
whenJobsetAllowed allowlist repoName jobsetName act
  | isJobsetAllowed allowlist repoName jobsetName = act
  | otherwise =
      putStrLn $
        "Ignoring " <> Text.unpack repoName <> " jobset " <> Text.unpack jobsetName <> ": not in the jobset allowlist"
