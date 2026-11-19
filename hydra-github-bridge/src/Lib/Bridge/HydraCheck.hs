-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The aggregate @hydra@ check-run: one check-run per commit that says
-- whether Hydra is done with it.
--
-- It is @in_progress@ from the start of the evaluation until the evaluation
-- has finished and no build of it is unfinished, then @success@ when the
-- evaluation had no errors and every build succeeded, and @failure@
-- otherwise.  A restarted build makes the evaluation unfinished again, and
-- the check-run goes back to @in_progress@.
--
-- Besides the check-run, the state is kept in @github_hydra_state@ (one row
-- per commit), which is what "Lib.Bridge.Gates" reads to approve or reject
-- the GitHub Actions jobs waiting for Hydra.  Reading the bridge's own small
-- table keeps that independent of how far the check-run queue has got, and
-- avoids searching @jobsetevals@ for a commit.
--
-- The same state is also posted as a commit status named like the
-- check-run, but only when the outcome changes (see 'hydraCommitState'),
-- for workflows triggered @on: status@ ("Lib.Bridge.CommitStatus").
module Lib.Bridge.HydraCheck
  ( -- * Names
    hydraCheckName,
    isHydraCheckName,

    -- * Pure summary
    HydraOutcome (..),
    outcomeText,
    parseOutcome,
    BuildRow,
    EvalSummary (..),
    summariseEval,
    summaryCheckRunPayload,

    -- * State
    HydraState (..),
    Transition (..),
    hydraCommitState,
    queueHydraStatus,
    hydraStateSchema,
    recordEvalStarted,
    recordEvalFailed,
    recordEvalSummary,
    lookupHydraState,
    unfinishedHydraEvals,
    aggregateCheckRuns,
  )
where

import Data.List (sort)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple (Connection, Only (..), Query, ToRow, execute_, query, query_, (:.) (..))
import Lib.Bridge.CommitStatus (CommitState (..), enqueueCommitStatus)
import Lib.Bridge.PublicUrl (hydraDetailsUrl)
import Lib.GitHub qualified as GitHub

-- | The name of the aggregate check-run.  It takes the bridge's
-- @CHECK_RUN_PREFIX@ like every other build check-run.
hydraCheckName :: Text -> Text
hydraCheckName checkRunPrefix = checkRunPrefix <> "hydra"

-- | Whether a check-run name is the aggregate one, whatever the prefix.
-- Used to give it the same queue priority as @*required@ check-runs.
isHydraCheckName :: Text -> Bool
isHydraCheckName name =
  name == "hydra" || any (`Text.isSuffixOf` name) [":hydra", "/hydra"]

data HydraOutcome = HydraPending | HydraSucceeded | HydraFailed
  deriving (Eq, Show)

outcomeText :: HydraOutcome -> Text
outcomeText HydraPending = "pending"
outcomeText HydraSucceeded = "success"
outcomeText HydraFailed = "failure"

parseOutcome :: Text -> Maybe HydraOutcome
parseOutcome t = case t of
  "pending" -> Just HydraPending
  "success" -> Just HydraSucceeded
  "failure" -> Just HydraFailed
  _ -> Nothing

-- | One build of an evaluation: job, finished (0/1), buildstatus,
-- starttime, stoptime.  The same shape "Lib.Bridge.CheckRunGroups" reads.
type BuildRow = (Text, Int, Maybe Int, Maybe Int, Maybe Int)

data EvalSummary = EvalSummary
  { outcome :: HydraOutcome,
    title :: Text,
    summary :: Text,
    failedJobs :: [Text],
    startedAt :: Maybe UTCTime,
    completedAt :: Maybe UTCTime
  }
  deriving (Eq, Show)

-- | Summarise an evaluation from its error message (empty or absent when
-- it had none) and its builds.
--
-- An evaluation error is final for the commit, so it fails the check-run
-- straight away rather than after the builds that did evaluate.  An
-- evaluation without builds is done as soon as it exists.
summariseEval :: Maybe Text -> [BuildRow] -> EvalSummary
summariseEval evalError builds =
  EvalSummary
    { outcome =
        if
          | hasEvalError -> HydraFailed
          | not done -> HydraPending
          | null failed -> HydraSucceeded
          | otherwise -> HydraFailed,
      title =
        if
          | hasEvalError -> "Evaluation has errors"
          | total == 0 -> "No jobs to build"
          | not done ->
              tshow (length finished)
                <> " of "
                <> tshow total
                <> " jobs finished"
                <> (if null failed then "" else " (" <> tshow (length failed) <> " failed)")
          | null failed -> tshow total <> " jobs succeeded"
          | otherwise -> tshow (length failed) <> " of " <> tshow total <> " jobs failed",
      summary =
        tshow (length finished - length failed)
          <> " succeeded, "
          <> tshow (length failed)
          <> " failed, "
          <> tshow pending
          <> " pending.",
      failedJobs = failed,
      startedAt = if null starts then Nothing else Just (toTime (minimum starts)),
      completedAt = if done && not (null stops) then Just (toTime (maximum stops)) else Nothing
    }
  where
    hasEvalError = maybe False (not . Text.null . Text.strip) evalError
    finished = [b | b@(_, 1, _, _, _) <- builds]
    failed = sort [job | (job, 1, buildStatus, _, _) <- builds, buildStatus /= Just 0]
    total = length builds
    pending = total - length finished
    done = pending == 0
    starts = catMaybes [start | (_, _, _, start, _) <- builds]
    stops = catMaybes [stop | (_, _, _, _, stop) <- finished]
    toTime = posixSecondsToUTCTime . fromIntegral

-- | The check-run for an evaluation summary.
summaryCheckRunPayload :: Text -> Text -> Int -> Text -> EvalSummary -> GitHub.CheckRunPayload
summaryCheckRunPayload publicBase checkRunPrefix evalId headSha s =
  GitHub.CheckRunPayload
    { name = hydraCheckName checkRunPrefix,
      headSha = headSha,
      detailsUrl = Just $ hydraDetailsUrl publicBase ("/eval/" <> tshow evalId),
      externalId = Just $ tshow evalId,
      status = if s.outcome == HydraPending then GitHub.InProgress else GitHub.Completed,
      conclusion = case s.outcome of
        HydraPending -> Nothing
        HydraSucceeded -> Just GitHub.Success
        HydraFailed -> Just GitHub.Failure,
      startedAt = s.startedAt,
      completedAt = if s.outcome == HydraPending then Nothing else s.completedAt,
      output =
        Just $
          GitHub.CheckRunOutput
            { title = s.title,
              summary = s.summary,
              text =
                if null s.failedJobs
                  then Nothing
                  else
                    Just $
                      Text.unlines ["- `" <> job <> "`" | job <- take shownFailed s.failedJobs]
                        <> (if length s.failedJobs > shownFailed then "- ...\n" else "")
            }
    }
  where
    shownFailed = 500

-- | What the bridge last worked out about a commit.
data HydraState = HydraState
  { evalId :: Maybe Int,
    outcome :: HydraOutcome,
    title :: Text
  }
  deriving (Eq, Show)

-- | A change of a commit's recorded state: the outcome before (if the
-- commit had a state) and after.
data Transition = Transition
  { previous :: Maybe HydraOutcome,
    current :: HydraOutcome
  }
  deriving (Eq, Show)

-- | The commit status to post for a change, if any.  Only a change of
-- outcome posts one (pending -> success/failure, or back to pending when a
-- build is restarted); progress within pending does not, so @status@
-- events stay rare.
hydraCommitState :: Transition -> Maybe CommitState
hydraCommitState t
  | t.previous == Just t.current = Nothing
  | otherwise = Just $ case t.current of
      HydraPending -> StatusPending
      HydraSucceeded -> StatusSuccess
      HydraFailed -> StatusFailure

-- | Queue the commit status for a recorded change, if it changed the
-- outcome.  The context is the check-run's name.
queueHydraStatus :: Connection -> Text -> Text -> Text -> Text -> Maybe Text -> Text -> Maybe Transition -> IO ()
queueHydraStatus conn checkRunPrefix owner repo sha targetUrl description transition =
  case transition >>= hydraCommitState of
    Nothing -> pure ()
    Just state -> enqueueCommitStatus conn owner repo sha (hydraCheckName checkRunPrefix) state targetUrl description

-- | Run a state upsert (an @INSERT ... RETURNING outcome@ taking
-- @params@) and report the transition it made, if it changed the row.  The
-- previous outcome is read in the same statement, from the snapshot the
-- upsert started from.
recordTransition :: (ToRow q) => Connection -> Text -> Text -> Text -> Query -> q -> IO (Maybe Transition)
recordTransition conn owner repo sha upsert params = do
  rows <-
    query
      conn
      ( "WITH prev AS (SELECT outcome FROM github_hydra_state WHERE owner = ? AND repo = ? AND head_sha = ?), \
        \     up AS ("
          <> upsert
          <> " RETURNING outcome) \
             \SELECT (SELECT outcome FROM prev), outcome FROM up"
      )
      ((owner, repo, sha) :. params) ::
      IO [(Maybe Text, Text)]
  pure $ case rows of
    [(prev, cur)] | Just c <- parseOutcome cur -> Just (Transition (prev >>= parseOutcome) c)
    _ -> Nothing

-- | @eval_id@ is NULL until an evaluation exists (after @eval_started@, or
-- after @eval_failed@ when fetching or evaluating failed outright).
-- @updated@ only moves when the outcome or title changes.
hydraStateSchema :: [String]
hydraStateSchema =
  [ "CREATE TABLE IF NOT EXISTS github_hydra_state ( \
    \  owner TEXT NOT NULL, \
    \  repo TEXT NOT NULL, \
    \  head_sha TEXT NOT NULL, \
    \  eval_id INTEGER, \
    \  outcome TEXT NOT NULL, \
    \  title TEXT NOT NULL, \
    \  updated TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
    \  PRIMARY KEY (owner, repo, head_sha) \
    \)",
    "CREATE INDEX IF NOT EXISTS idx_github_hydra_state_pending \
    \ON github_hydra_state (updated) WHERE outcome = 'pending'"
  ]

-- | An evaluation of the commit started.  Only creates the row, or resets
-- one that has no evaluation yet: Hydra re-checks unchanged jobsets, and
-- each check must not send a finished commit back to pending.
recordEvalStarted :: Connection -> Text -> Text -> Text -> IO (Maybe Transition)
recordEvalStarted conn owner repo sha =
  recordTransition
    conn
    owner
    repo
    sha
    "INSERT INTO github_hydra_state (owner, repo, head_sha, eval_id, outcome, title) \
    \VALUES (?, ?, ?, NULL, 'pending', 'Evaluating') \
    \ON CONFLICT (owner, repo, head_sha) DO UPDATE \
    \SET outcome = excluded.outcome, title = excluded.title, updated = NOW() \
    \WHERE github_hydra_state.eval_id IS NULL \
    \  AND (github_hydra_state.outcome, github_hydra_state.title) <> (excluded.outcome, excluded.title)"
    (owner, repo, sha)

-- | Fetching or evaluating the commit failed before an evaluation existed.
-- Does not override the outcome of an evaluation that did happen.
recordEvalFailed :: Connection -> Text -> Text -> Text -> IO (Maybe Transition)
recordEvalFailed conn owner repo sha =
  recordTransition
    conn
    owner
    repo
    sha
    "INSERT INTO github_hydra_state (owner, repo, head_sha, eval_id, outcome, title) \
    \VALUES (?, ?, ?, NULL, 'failure', 'Evaluation failed') \
    \ON CONFLICT (owner, repo, head_sha) DO UPDATE \
    \SET outcome = excluded.outcome, title = excluded.title, updated = NOW() \
    \WHERE github_hydra_state.eval_id IS NULL \
    \  AND (github_hydra_state.outcome, github_hydra_state.title) <> (excluded.outcome, excluded.title)"
    (owner, repo, sha)

-- | Record the summary of evaluation @evalId@.  An older evaluation of the
-- same commit (e.g. the reconciler recomputing it late) never overrides a
-- newer one.
recordEvalSummary :: Connection -> Text -> Text -> Text -> Int -> EvalSummary -> IO (Maybe Transition)
recordEvalSummary conn owner repo sha evalId s =
  recordTransition
    conn
    owner
    repo
    sha
    "INSERT INTO github_hydra_state (owner, repo, head_sha, eval_id, outcome, title) \
    \VALUES (?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (owner, repo, head_sha) DO UPDATE \
    \SET eval_id = excluded.eval_id, outcome = excluded.outcome, title = excluded.title, updated = NOW() \
    \WHERE (github_hydra_state.eval_id IS NULL OR github_hydra_state.eval_id <= excluded.eval_id) \
    \  AND (github_hydra_state.eval_id, github_hydra_state.outcome, github_hydra_state.title) \
    \      IS DISTINCT FROM (excluded.eval_id, excluded.outcome, excluded.title)"
    (owner, repo, sha, evalId, outcomeText s.outcome, s.title)

lookupHydraState :: Connection -> Text -> Text -> Text -> IO (Maybe HydraState)
lookupHydraState conn owner repo sha = do
  rows <-
    query
      conn
      "SELECT eval_id, outcome, title FROM github_hydra_state WHERE owner = ? AND repo = ? AND head_sha = ?"
      (owner, repo, sha)
  pure $ case rows of
    [(evalId, outcome, title)] | Just o <- parseOutcome outcome -> Just (HydraState evalId o title)
    _ -> Nothing

-- | Commits whose evaluation exists but is not done yet, so the reconciler
-- can recompute them: the group tracker is only held in memory, and a
-- build shared with a later evaluation only notifies about that later one.
unfinishedHydraEvals :: Connection -> IO [(Int, Text, Text, Text)]
unfinishedHydraEvals conn =
  query_
    conn
    "SELECT eval_id, owner, repo, head_sha FROM github_hydra_state \
    \WHERE outcome = 'pending' AND eval_id IS NOT NULL AND updated > NOW() - interval '14 days'"

-- | Summarise evaluation @evalId@ (whose builds are @rows@) for commit
-- @sha@, record it, and return the check-run when the state changed.  A
-- change to a finished state wakes the gate worker (see "Lib.Bridge.Gates").
aggregateCheckRuns :: Text -> Text -> Connection -> Int -> Text -> Text -> Text -> [BuildRow] -> IO [GitHub.CheckRun]
aggregateCheckRuns publicBase checkRunPrefix conn evalId owner repo sha rows = do
  -- The same lookup as the ci/eval check-run: every evaluation links an
  -- evaluationerrors row (empty when it succeeded); fall back to the
  -- jobset only for evaluations without one.
  errs <-
    query
      conn
      "SELECT coalesce(e.errormsg, j.errormsg) FROM jobsetevals je \
      \JOIN jobsets j ON j.id = je.jobset_id \
      \LEFT JOIN evaluationerrors e ON e.id = je.evaluationerror_id \
      \WHERE je.id = ?"
      (Only evalId)
  let evalError = case errs of
        [Only e] -> e
        _ -> Nothing
      s = summariseEval evalError rows
  transition <- recordEvalSummary conn owner repo sha evalId s
  queueHydraStatus conn checkRunPrefix owner repo sha (Just $ hydraDetailsUrl publicBase ("/eval/" <> tshow evalId)) s.title transition
  if isNothing transition
    then pure []
    else do
      _ <- if s.outcome /= HydraPending then execute_ conn "NOTIFY github_gate" else pure 0
      pure [GitHub.CheckRun owner repo (summaryCheckRunPayload publicBase checkRunPrefix evalId sha s)]

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
