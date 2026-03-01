-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for evaluations and eval inputs.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Evals
  ( getEval
  , getEvalError
  , getEvalInputs
  , previousEval
  , jobsetEvals
  , allJobsetEvalsCount
  , latestEvals
  , latestEvalsCount
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))

import HydraWeb.Models.Eval

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
jobsetEvals :: Connection -> Int -> Int -> Int -> IO [EvalInfo]
jobsetEvals conn jobsetId' offset limit = do
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
  |] (jobsetId', limit, offset)
  let evals = map scanEvalRow rows
  mapM (computeEvalInfo conn) evals

-- | Count of evals with new builds for a jobset (for pagination).
allJobsetEvalsCount :: Connection -> Int -> IO Int
allJobsetEvalsCount conn jobsetId' = do
  [Only n] <- query conn [sql|
    SELECT count(*) FROM jobsetevals WHERE jobset_id = ? AND hasnewbuilds = 1
  |] (Only jobsetId')
  pure n

-- | Fetch the most recent evaluations across all jobsets (for /evals page).
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
    WHERE e.hasnewbuilds = 1
    ORDER BY e.id DESC
    LIMIT ? OFFSET ?
  |] (limit, offset)
  let evals = map scanEvalRow rows
  -- For the latest-evals list, we do a simpler computation (no diff).
  pure $ map simpleEvalInfo evals

-- | Total count of evals with new builds (for pagination on /evals).
latestEvalsCount :: Connection -> IO Int
latestEvalsCount conn = do
  [Only n] <- query conn [sql|
    SELECT count(*) FROM jobsetevals WHERE hasnewbuilds = 1
  |] ()
  pure n

-- | Compute stats and diff for a single eval (used in jobsetEvals).
computeEvalInfo :: Connection -> JobsetEval -> IO EvalInfo
computeEvalInfo conn eval = do
  -- Compute scheduled/succeeded counts.
  (nrScheduled, nrSucceeded) <- case evalNrSucceeded eval of
    Just s  -> pure (0, s)
    Nothing -> do
      [Only sched] <- query conn [sql|
        SELECT count(*) FROM builds b
        JOIN jobsetevalmembers m ON m.build = b.id
        WHERE m.eval = ? AND b.finished = 0
      |] (Only (evalId eval))
      [Only succ'] <- query conn [sql|
        SELECT count(*) FROM builds b
        JOIN jobsetevalmembers m ON m.build = b.id
        WHERE m.eval = ? AND b.finished = 1 AND b.buildstatus = 0
      |] (Only (evalId eval))
      pure (sched :: Int, succ' :: Int)

  let nrBuilds = fromMaybe 0 (evalNrBuilds eval)
      nrFailed = nrBuilds - nrSucceeded - nrScheduled

  -- Load inputs for this eval.
  inputs <- getEvalInputs conn (evalId eval)

  -- Find previous eval for diff.
  mPrev <- previousEval conn eval

  (diff, changedInputs) <- case mPrev of
    Nothing -> pure (0, [])
    Just prev -> do
      prevSucceeded <- case evalNrSucceeded prev of
        Just s  -> pure s
        Nothing -> do
          [Only s] <- query conn [sql|
            SELECT count(*) FROM builds b
            JOIN jobsetevalmembers m ON m.build = b.id
            WHERE m.eval = ? AND b.finished = 1 AND b.buildstatus = 0
          |] (Only (evalId prev))
          pure (s :: Int)
      prevInputs <- getEvalInputs conn (evalId prev)
      let changed = computeChangedInputs inputs prevInputs
      pure (nrSucceeded - prevSucceeded, changed)

  pure EvalInfo
    { eiEval          = eval
    , eiNrScheduled   = nrScheduled
    , eiNrSucceeded   = nrSucceeded
    , eiNrFailed      = nrFailed
    , eiDiff          = diff
    , eiChangedInputs = changedInputs
    }

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

-- | Scan a jobset eval row (15 columns).
scanEvalRow :: ( Int, Int, Maybe Int,
                 Int, Int, Int,
                 Int, Text, Maybe Int, Maybe Int,
                 Maybe Text, Maybe Text, Maybe Text,
                 Text, Text )
            -> JobsetEval
scanEvalRow ( eid, jsId, errId,
              ts, checkout, evalTime,
              hasNew, hash, nrBuilds, nrSucc,
              flake, nixInput, nixPath,
              proj, js ) =
  JobsetEval eid jsId errId ts checkout evalTime
             hasNew hash nrBuilds nrSucc
             flake nixInput nixPath proj js
