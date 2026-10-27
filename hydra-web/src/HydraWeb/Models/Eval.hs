-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
{-# LANGUAGE DeriveGeneric #-}

-- | Evaluation-related types matching Hydra's PostgreSQL schema.
module HydraWeb.Models.Eval
  ( JobsetEval (..),
    JobsetEvalInput (..),
    EvalInfo (..),
    EvaluationError (..),
    RunningEval (..),
    QueuedEval (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | A jobset evaluation. Matches "jobsetevals" table plus denormalized
-- project/jobset from the "jobsets" table via JOIN.
data JobsetEval = JobsetEval
  { evalId :: !Int,
    evalJobsetId :: !Int,
    evalEvaluationErrorId :: !(Maybe Int),
    evalTimestamp :: !Int,
    evalCheckoutTime :: !Int,
    evalEvalTime :: !Int,
    evalHasNewBuilds :: !Int,
    evalHash :: !Text,
    evalNrBuilds :: !(Maybe Int),
    evalNrSucceeded :: !(Maybe Int),
    evalFlake :: !(Maybe Text),
    evalNixExprInput :: !(Maybe Text),
    evalNixExprPath :: !(Maybe Text),
    -- | Denormalized from jobsets JOIN
    evalProject :: !Text,
    -- | Denormalized from jobsets JOIN
    evalJobset :: !Text
  }
  deriving (Show, Eq, Generic)

-- | An input to a jobset evaluation. Matches "jobsetevalinputs" table.
data JobsetEvalInput = JobsetEvalInput
  { jeiName :: !Text,
    jeiAltNr :: !Int,
    jeiType :: !Text,
    jeiUri :: !(Maybe Text),
    jeiRevision :: !(Maybe Text),
    jeiValue :: !(Maybe Text),
    jeiDependency :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)

-- | Computed per-evaluation statistics for the eval list view.
-- Wraps a JobsetEval with aggregate counts and diff info.
data EvalInfo = EvalInfo
  { eiEval :: !JobsetEval,
    eiNrScheduled :: !Int,
    eiNrSucceeded :: !Int,
    eiNrFailed :: !Int,
    -- | Change in nrSucceeded vs previous eval
    eiDiff :: !Int,
    -- | Inputs that differ from previous eval
    eiChangedInputs :: ![JobsetEvalInput]
  }
  deriving (Show, Eq, Generic)

-- | An evaluation error. Matches "evaluationerrors" table.
data EvaluationError = EvaluationError
  { eeId :: !Int,
    eeErrorMsg :: !(Maybe Text),
    eeErrorTime :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)

-- | A currently-running evaluation (jobset with starttime set, still evaluating).
data RunningEval = RunningEval
  { -- | Jobset name
    reJobset :: !Text,
    -- | Project name
    reProject :: !Text,
    -- | When evaluation started (epoch)
    reStartTime :: !Int,
    -- | Seconds elapsed since start
    reDuration :: !Int,
    -- | Flake URI (if flake-based)
    reFlake :: !(Maybe Text),
    -- | Jobset ID (for linking)
    reJobsetId :: !Int
  }
  deriving (Show, Eq, Generic)

-- | A queued evaluation (jobset with triggertime set, waiting to start).
data QueuedEval = QueuedEval
  { -- | Jobset name
    qeJobset :: !Text,
    -- | Project name
    qeProject :: !Text,
    -- | When evaluation was triggered (epoch)
    qeTriggerTime :: !Int,
    -- | Flake URI (if flake-based)
    qeFlake :: !(Maybe Text),
    -- | Jobset ID
    qeJobsetId :: !Int,
    -- | Last error message (if any)
    qeErrorMsg :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)
