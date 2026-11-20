{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Report the jobs of very large jobsets to GitHub as one check-run per
-- group of jobs instead of one per job.
--
-- GitHub copes badly with thousands of check-runs on one commit (haskell.nix
-- has 9000+ jobs).  For a repository configured with a depth @n@, a job whose
-- name has more than @n@ dot-separated components is reported under the
-- check-run named after its first @n@ components, e.g.
-- @aarch64-darwin.unstable.ghc914-sh.ghcjs@.  Jobs with shorter names (such
-- as @required@) are still reported individually, as are failures (see
-- 'Lib.Bridge.HydraToGitHub.handleHydraNotification').
--
-- Recomputing a group on every build notification would scan the whole
-- evaluation each time, so notifications only mark the group dirty and
-- 'groupFlusher' recomputes the dirty groups of an evaluation in one query.
-- The same query also feeds the aggregate @hydra@ check-run of the
-- evaluation (see "Lib.Bridge.HydraCheck"), which is tracked for every
-- repository, grouped or not.
module Lib.Bridge.CheckRunGroups
  ( CheckRunGroups,
    noCheckRunGroups,
    parseCheckRunGroups,
    jobGroup,
    GroupKey (..),
    GroupTracker,
    newGroupTracker,
    markGroupDirty,
    markAggregateDirty,
    groupFlusher,
    aggregateReconciler,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (forM_, forever)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Lib.Bridge.HydraCheck (aggregateCheckRuns, unfinishedHydraEvals)
import Lib.Bridge.PublicUrl (hydraDetailsUrl)
import Lib.GitHub qualified as GitHub
import Network.HTTP.Types.URI (urlEncode)
import Text.Read (readMaybe)

-- | Group depth per lower-cased @owner/repo@.
newtype CheckRunGroups = CheckRunGroups (Map Text Int)

noCheckRunGroups :: CheckRunGroups
noCheckRunGroups = CheckRunGroups Map.empty

-- | Parse @CHECK_RUN_GROUPS@, e.g. @[(\"ican-group/haskell.nix\", 4)]@.
parseCheckRunGroups :: String -> Either String CheckRunGroups
parseCheckRunGroups raw =
  case readMaybe raw :: Maybe [(String, Int)] of
    Nothing -> Left "CHECK_RUN_GROUPS: expected a Haskell list of (\"owner/repo\", depth) pairs"
    Just entries
      | any ((< 1) . snd) entries -> Left "CHECK_RUN_GROUPS: depths must be positive"
      | otherwise -> Right . CheckRunGroups $ Map.fromList [(Text.toLower (Text.pack repo), depth) | (repo, depth) <- entries]

-- | The group a job is reported under, if its repository is grouped and its
-- name is longer than the group depth.
jobGroup :: CheckRunGroups -> Text -> Text -> Text -> Maybe Text
jobGroup (CheckRunGroups depths) owner repo job = do
  depth <- Map.lookup (Text.toLower (owner <> "/" <> repo)) depths
  let parts = Text.splitOn "." job
  if length parts > depth
    then Just (Text.intercalate "." (take depth parts))
    else Nothing

-- | The evaluation whose builds make up the groups, and the commit their
-- check-runs are reported on.  The two differ when an evaluation is
-- identical to an earlier one, whose builds are then reported for the new
-- commit.
data GroupKey = GroupKey
  { gkEval :: Int,
    gkOwner :: Text,
    gkRepo :: Text,
    gkHeadSha :: Text
  }
  deriving (Eq, Ord, Show)

-- | What is out of date for an evaluation: some of its groups, and/or its
-- aggregate @hydra@ check-run.
data Dirty = Dirty (Set Text) Bool

instance Semigroup Dirty where
  Dirty g a <> Dirty g' a' = Dirty (g <> g') (a || a')

-- | Groups whose check-runs are out of date.  This is only held in memory;
-- after a restart a group is brought up to date by its next notification,
-- and an unfinished aggregate by 'aggregateReconciler'.
newtype GroupTracker = GroupTracker (IORef (Map GroupKey Dirty))

newGroupTracker :: IO GroupTracker
newGroupTracker = GroupTracker <$> newIORef Map.empty

markGroupDirty :: GroupTracker -> GroupKey -> Text -> IO ()
markGroupDirty (GroupTracker ref) key grp =
  atomicModifyIORef' ref $ \dirty -> (Map.insertWith (<>) key (Dirty (Set.singleton grp) False) dirty, ())

-- | Mark the aggregate @hydra@ check-run of an evaluation out of date.
markAggregateDirty :: GroupTracker -> GroupKey -> IO ()
markAggregateDirty (GroupTracker ref) key =
  atomicModifyIORef' ref $ \dirty -> (Map.insertWith (<>) key (Dirty Set.empty True) dirty, ())

-- | Every @interval@ seconds, mark the aggregate of every evaluation that is
-- still unfinished according to @github_hydra_state@ out of date, for the
-- flusher to recompute.  Notifications only reach the latest evaluation a
-- build belongs to, and the tracker is lost on restart; this keeps a
-- pending @hydra@ check-run (and the jobs gated on it) from being stranded.
aggregateReconciler :: Int -> GroupTracker -> Connection -> IO ()
aggregateReconciler interval tracker conn = forever $ do
  threadDelay (interval * 1000000)
  ( do
      evals <- unfinishedHydraEvals conn
      forM_ evals $ \(evalId, owner, repo, sha) -> markAggregateDirty tracker (GroupKey evalId owner repo sha)
    )
    `catch` \e -> case fromException e of
      Just (async :: SomeAsyncException) -> throwIO async
      Nothing -> putStrLn $ "ERROR: hydra aggregate reconcile: " ++ displayException (e :: SomeException)

-- | Every @interval@ seconds, recompute the check-runs of the groups marked
-- dirty since the last flush and pass them to @report@.
groupFlusher :: Int -> Text -> Text -> CheckRunGroups -> GroupTracker -> ([GitHub.CheckRun] -> IO ()) -> Connection -> IO ()
groupFlusher interval publicBase checkRunPrefix groups (GroupTracker ref) report conn = forever $ do
  threadDelay (interval * 1000000)
  dirty <- atomicModifyIORef' ref (Map.empty,)
  forM_ (Map.toList dirty) $ \(key, Dirty grps aggregate) ->
    (groupCheckRuns publicBase checkRunPrefix groups key grps aggregate conn >>= report)
      `catch` \e -> case fromException e of
        Just (async :: SomeAsyncException) -> throwIO async
        Nothing -> putStrLn $ "ERROR: check-run groups " ++ show key ++ ": " ++ displayException (e :: SomeException)

groupCheckRuns :: Text -> Text -> CheckRunGroups -> GroupKey -> Set Text -> Bool -> Connection -> IO [GitHub.CheckRun]
groupCheckRuns publicBase checkRunPrefix groups key grps aggregate conn = do
  rows <-
    query
      conn
      "SELECT b.job, b.finished, b.buildstatus, b.starttime, b.stoptime \
      \FROM jobsetevalmembers m JOIN builds b ON b.id = m.build \
      \WHERE m.eval = ?"
      (Only key.gkEval) ::
      IO [(Text, Int, Maybe Int, Maybe Int, Maybe Int)]
  let members =
        Map.fromListWith
          (++)
          [ (grp, [row])
          | row@(job, _, _, _, _) <- rows,
            Just grp <- [jobGroup groups key.gkOwner key.gkRepo job],
            grp `Set.member` grps
          ]
  aggregateRuns <-
    if aggregate
      then aggregateCheckRuns publicBase checkRunPrefix conn key.gkEval key.gkOwner key.gkRepo key.gkHeadSha rows
      else pure []
  pure $ [GitHub.CheckRun key.gkOwner key.gkRepo (groupPayload grp builds) | (grp, builds) <- Map.toList members] ++ aggregateRuns
  where
    groupPayload grp builds =
      let finished = [b | b@(_, 1, _, _, _) <- builds]
          failed = sort [job | (job, 1, buildStatus, _, _) <- builds, buildStatus /= Just 0]
          total = length builds
          pending = total - length finished
          done = pending == 0
          starts = catMaybes [start | (_, _, _, start, _) <- builds]
          stops = catMaybes [stop | (_, _, _, _, stop) <- finished]
          toTime = posixSecondsToUTCTime . fromIntegral
          shownFailed = 500
       in GitHub.CheckRunPayload
            { name = checkRunPrefix <> grp,
              headSha = key.gkHeadSha,
              detailsUrl = Just $ hydraDetailsUrl publicBase ("/eval/" <> tshow key.gkEval <> "?full=1&filter=" <> urlEncodeText grp),
              externalId = Just $ tshow key.gkEval,
              status = if done then GitHub.Completed else GitHub.InProgress,
              conclusion = if done then Just (if null failed then GitHub.Success else GitHub.Failure) else Nothing,
              startedAt = if null starts then Nothing else Just (toTime (minimum starts)),
              completedAt = if done && not (null stops) then Just (toTime (maximum stops)) else Nothing,
              output =
                Just $
                  GitHub.CheckRunOutput
                    { title =
                        if
                          | not done -> tshow (length finished) <> " of " <> tshow total <> " jobs finished"
                          | null failed -> tshow total <> " jobs succeeded"
                          | otherwise -> tshow (length failed) <> " of " <> tshow total <> " jobs failed",
                      summary =
                        tshow (length finished - length failed)
                          <> " succeeded, "
                          <> tshow (length failed)
                          <> " failed, "
                          <> tshow pending
                          <> " pending.",
                      text =
                        if null failed
                          then Nothing
                          else
                            Just $
                              "Each failed job also has a check-run of its own.\n\n"
                                <> Text.unlines ["- `" <> job <> "`" | job <- take shownFailed failed]
                                <> (if length failed > shownFailed then "- ...\n" else "")
                    }
            }
    tshow :: (Show a) => a -> Text
    tshow = Text.pack . show
    urlEncodeText = Text.decodeUtf8 . urlEncode True . Text.encodeUtf8
