-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for evaluations and eval inputs.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module HydraWeb.DB.Evals
  ( getEval
  , getEvalError
  , getEvalInputs
  , previousEval
  , jobsetEvals
  , allJobsetEvalsCount
  , latestEvals
  , latestEvalsCount
  , runningEvaluations
  , runningEvalsCount
  , queuedEvaluations
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.Types ((:.)((:.)), In(..))

import HydraWeb.Models.Eval
  ( JobsetEval (..), JobsetEvalInput (..), EvalInfo (..), EvaluationError (..)
  , RunningEval (..), QueuedEval (..)
  )

-- | Fetch a single evaluation by ID with denormalized project/jobset.
getEval :: Connection -> Int -> IO (Maybe JobsetEval)
getEval conn evalId' = do
  rows <- query conn [sql|
    SELECT e.id, e.jobset_id, e.evaluationerror_id,
           e.timestamp, e.checkouttime, e.evaltime,
           e.hasnewbuilds, e.hash, e.nrbuilds, e.nrsucceeded,
           e.flake, e.nixexprinput, e.nixexprpath,
           j.project, j.name
    FROM jobsetevals e
    JOIN jobsets j ON j.id = e.jobset_id
    WHERE e.id = ?
  |] (Only evalId')
  case rows of
    []    -> pure Nothing
    (r:_) -> pure $ Just (scanEvalRow r)

-- | Fetch the evaluation error message for an eval (Nothing if no error).
getEvalError :: Connection -> Int -> IO (Maybe EvaluationError)
getEvalError conn evalId' = do
  rows <- query conn [sql|
    SELECT er.id, er.errormsg, er.errortime
    FROM evaluationerrors er
    JOIN jobsetevals e ON e.evaluationerror_id = er.id
    WHERE e.id = ?
  |] (Only evalId')
  case rows of
    []             -> pure Nothing
    ((i, m, t):_) -> pure $ Just (EvaluationError i m t)

-- | Fetch inputs for an evaluation (altnr=0, with URI/revision or dependency).
getEvalInputs :: Connection -> Int -> IO [JobsetEvalInput]
getEvalInputs conn evalId' = do
  rows <- query conn [sql|
    SELECT name, altnr, type, uri, revision, value, dependency
    FROM jobsetevalinputs
    WHERE eval = ?
      AND altnr = 0
      AND ((uri IS NOT NULL AND revision IS NOT NULL) OR dependency IS NOT NULL)
    ORDER BY name
  |] (Only evalId')
  pure $ map (\(n, a, t, u, r, v, d) -> JobsetEvalInput n a t u r v d) rows

-- | Fetch the most recent eval before the given one for the same jobset.
previousEval :: Connection -> JobsetEval -> IO (Maybe JobsetEval)
previousEval conn eval = do
  rows <- query conn [sql|
    SELECT e.id, e.jobset_id, e.evaluationerror_id,
           e.timestamp, e.checkouttime, e.evaltime,
           e.hasnewbuilds, e.hash, e.nrbuilds, e.nrsucceeded,
           e.flake, e.nixexprinput, e.nixexprpath,
           j.project, j.name
    FROM jobsetevals e
    JOIN jobsets j ON j.id = e.jobset_id
    WHERE e.jobset_id = ? AND e.hasnewbuilds = 1 AND e.id < ?
    ORDER BY e.id DESC LIMIT 1
  |] (evalJobsetId eval, evalId eval)
  case rows of
    []    -> pure Nothing
    (r:_) -> pure $ Just (scanEvalRow r)

-- | Fetch paginated evaluations for a jobset with computed stats and diffs.
-- Uses batched queries to avoid N+1: fetches all build-status counts and
-- inputs in bulk rather than per-eval.
jobsetEvals :: Connection -> Int -> Int -> Int -> IO [EvalInfo]
jobsetEvals conn jobsetId' offset limit = do
  -- Fetch one extra eval to serve as the "previous" for the last page entry,
  -- since our list is already ordered by id DESC with hasnewbuilds=1.
  rows <- query conn [sql|
    SELECT e.id, e.jobset_id, e.evaluationerror_id,
           e.timestamp, e.checkouttime, e.evaltime,
           e.hasnewbuilds, e.hash, e.nrbuilds, e.nrsucceeded,
           e.flake, e.nixexprinput, e.nixexprpath,
           j.project, j.name
    FROM jobsetevals e
    JOIN jobsets j ON j.id = e.jobset_id
    WHERE e.jobset_id = ? AND e.hasnewbuilds = 1
    ORDER BY e.id DESC
    LIMIT ? OFFSET ?
  |] (jobsetId', limit + 1, offset)
  let allEvals  = map scanEvalRow rows
      pageEvals = take limit allEvals
  if null pageEvals then pure [] else do
    let allIds     = map evalId allEvals
        needCounts = [evalId e | e <- allEvals, evalNrSucceeded e == Nothing]

    -- Batch: scheduled/succeeded counts for evals without precomputed values.
    (schedMap, succMap) <- if null needCounts
      then pure (Map.empty, Map.empty)
      else do
        schedRows <- query conn [sql|
          SELECT m.eval, count(*)
          FROM builds b
          JOIN jobsetevalmembers m ON m.build = b.id
          WHERE m.eval IN ? AND b.finished = 0
          GROUP BY m.eval
        |] (Only (In needCounts))
        succRows <- query conn [sql|
          SELECT m.eval, count(*)
          FROM builds b
          JOIN jobsetevalmembers m ON m.build = b.id
          WHERE m.eval IN ? AND b.finished = 1 AND b.buildstatus = 0
          GROUP BY m.eval
        |] (Only (In needCounts))
        pure (Map.fromList (schedRows :: [(Int, Int)])
             , Map.fromList (succRows :: [(Int, Int)]))

    -- Batch: inputs for all eval IDs (page evals + the extra previous).
    inputRows <- query conn [sql|
      SELECT eval, name, altnr, type, uri, revision, value, dependency
      FROM jobsetevalinputs
      WHERE eval IN ?
        AND altnr = 0
        AND ((uri IS NOT NULL AND revision IS NOT NULL) OR dependency IS NOT NULL)
      ORDER BY name
    |] (Only (In allIds))
    let inputMap :: Map Int [JobsetEvalInput]
        inputMap = Map.fromListWith (++) $ flip map inputRows $
          \((eid, n, a, t) :. (u, r, v, d)) ->
            (eid :: Int, [JobsetEvalInput n a t u r v d])

    -- Build a map from each page eval's ID to its "previous" eval.
    -- Since allEvals is ordered by id DESC from the same jobset with
    -- hasnewbuilds=1, eval[i]'s previous is eval[i+1].
    let prevMap = Map.fromList (zip (map evalId pageEvals) (drop 1 allEvals))

    pure $ map (buildEvalInfo schedMap succMap inputMap prevMap) pageEvals
  where
    buildEvalInfo :: Map Int Int -> Map Int Int -> Map Int [JobsetEvalInput]
                  -> Map Int JobsetEval -> JobsetEval -> EvalInfo
    buildEvalInfo schedMap succMap inputMap prevMap eval =
      let eid = evalId eval
          (nrScheduled, nrSucceeded) = case evalNrSucceeded eval of
            Just s  -> (0, s)
            Nothing -> ( Map.findWithDefault 0 eid schedMap
                       , Map.findWithDefault 0 eid succMap )
          nrBuilds = fromMaybe 0 (evalNrBuilds eval)
          nrFailed = nrBuilds - nrSucceeded - nrScheduled
          inputs   = Map.findWithDefault [] eid inputMap
          (diff, changedInputs) = case Map.lookup eid prevMap of
            Nothing -> (0, [])
            Just prev ->
              let prevSucceeded = case evalNrSucceeded prev of
                    Just s  -> s
                    Nothing -> Map.findWithDefault 0 (evalId prev) succMap
                  prevInputs = Map.findWithDefault [] (evalId prev) inputMap
                  changed    = computeChangedInputs inputs prevInputs
              in (nrSucceeded - prevSucceeded, changed)
      in EvalInfo eval nrScheduled nrSucceeded nrFailed diff changedInputs

-- | Count of evals with new builds for a jobset (for pagination).
allJobsetEvalsCount :: Connection -> Int -> IO Int
allJobsetEvalsCount conn jobsetId' = do
  [Only n] <- query conn [sql|
    SELECT count(*) FROM jobsetevals WHERE jobset_id = ? AND hasnewbuilds = 1
  |] (Only jobsetId')
  pure n

-- | Fetch the most recent evaluations across all jobsets (for /evals page).
-- Excludes evals from hidden projects/jobsets.
latestEvals :: Connection -> Int -> Int -> IO [EvalInfo]
latestEvals conn offset limit = do
  rows <- query conn [sql|
    SELECT e.id, e.jobset_id, e.evaluationerror_id,
           e.timestamp, e.checkouttime, e.evaltime,
           e.hasnewbuilds, e.hash, e.nrbuilds, e.nrsucceeded,
           e.flake, e.nixexprinput, e.nixexprpath,
           j.project, j.name
    FROM jobsetevals e
    JOIN jobsets j ON j.id = e.jobset_id
    JOIN projects p ON p.name = j.project
    WHERE e.hasnewbuilds = 1
      AND j.hidden = 0 AND p.hidden = 0
    ORDER BY e.id DESC
    LIMIT ? OFFSET ?
  |] (limit, offset)
  let evals = map scanEvalRow rows
  -- For the latest-evals list, we do a simpler computation (no diff).
  pure $ map simpleEvalInfo evals

-- | Total count of evals with new builds (for pagination on /evals).
-- Excludes evals from hidden projects/jobsets.
latestEvalsCount :: Connection -> IO Int
latestEvalsCount conn = do
  [Only n] <- query_ conn [sql|
    SELECT count(*)
    FROM jobsetevals e
    JOIN jobsets j ON j.id = e.jobset_id
    JOIN projects p ON p.name = j.project
    WHERE e.hasnewbuilds = 1
      AND j.hidden = 0 AND p.hidden = 0
  |]
  pure n

-- | Simple eval info without diff computation (for /evals page).
simpleEvalInfo :: JobsetEval -> EvalInfo
simpleEvalInfo eval =
  let nrSucceeded = fromMaybe 0 (evalNrSucceeded eval)
      nrBuilds    = fromMaybe 0 (evalNrBuilds eval)
      nrFailed    = nrBuilds - nrSucceeded
  in  EvalInfo eval 0 nrSucceeded nrFailed 0 []

-- | Determine which inputs changed between two eval input lists.
computeChangedInputs :: [JobsetEvalInput] -> [JobsetEvalInput] -> [JobsetEvalInput]
computeChangedInputs current prev =
  let prevMap = [(jeiName p, p) | p <- prev]
      findPrev n = lookup n prevMap
  in  filter (isChanged findPrev) current
  where
    isChanged findPrev input = case findPrev (jeiName input) of
      Nothing -> True  -- New input, not in previous eval
      Just p  -> jeiRevision input /= jeiRevision p
              || jeiType input /= jeiType p
              || jeiUri input /= jeiUri p
              || jeiDependency input /= jeiDependency p

-- | Fetch currently-running evaluations (jobsets with starttime set).
-- Excludes evals from hidden projects/jobsets.
runningEvaluations :: Connection -> IO [RunningEval]
runningEvaluations conn = do
  rows <- query_ conn [sql|
    SELECT j.name, j.project, j.starttime,
           extract(epoch from now())::int - j.starttime,
           j.flake, j.id
    FROM jobsets j
    JOIN projects p ON p.name = j.project
    WHERE j.starttime IS NOT NULL
      AND j.hidden = 0 AND p.hidden = 0
    ORDER BY j.starttime ASC
  |]
  pure $ map (\(name, proj, start, dur, flake, jid) ->
    RunningEval name proj start dur flake jid) rows

-- | Count of currently-running evaluations (for nav badge).
-- Excludes hidden projects/jobsets.
runningEvalsCount :: Connection -> IO Int
runningEvalsCount conn = do
  [Only n] <- query_ conn [sql|
    SELECT count(*)
    FROM jobsets j
    JOIN projects p ON p.name = j.project
    WHERE j.starttime IS NOT NULL
      AND j.hidden = 0 AND p.hidden = 0
  |]
  pure n

-- | Fetch queued evaluations (jobsets with triggertime set, not yet started).
-- Excludes hidden projects/jobsets. Ordered by triggertime (queue priority).
queuedEvaluations :: Connection -> IO [QueuedEval]
queuedEvaluations conn = do
  rows <- query_ conn [sql|
    SELECT j.name, j.project, j.triggertime,
           j.flake, j.id, j.errormsg
    FROM jobsets j
    JOIN projects p ON p.name = j.project
    WHERE j.triggertime > 0
      AND j.starttime IS NULL
      AND j.hidden = 0 AND p.hidden = 0
    ORDER BY j.triggertime ASC
  |]
  pure $ map (\(name, proj, tt, flake, jid, emsg) ->
    QueuedEval name proj tt flake jid emsg) rows

-- | Scan a jobset eval row (15 columns) using nested tuples via (:.).
scanEvalRow :: ( (Int, Int, Maybe Int, Int, Int, Int, Int, Text)
               :. (Maybe Int, Maybe Int, Maybe Text, Maybe Text, Maybe Text, Text, Text) )
            -> JobsetEval
scanEvalRow ( (eid, jsId, errId, ts, checkout, evalTime, hasNew, hash)
            :. (nrBuilds, nrSucc, flake, nixInput, nixPath, proj, js) ) =
  JobsetEval eid jsId errId ts checkout evalTime
             hasNew hash nrBuilds nrSucc
             flake nixInput nixPath proj js
