-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Evaluation-related types matching Hydra's PostgreSQL schema.
{-# LANGUAGE DeriveGeneric #-}

module HydraWeb.Models.Eval
  ( JobsetEval (..)
  , JobsetEvalInput (..)
  , EvalInfo (..)
  , EvaluationError (..)
  , RunningEval (..)
  , QueuedEval (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | A jobset evaluation. Matches "jobsetevals" table plus denormalized
-- project/jobset from the "jobsets" table via JOIN.
data JobsetEval = JobsetEval
  { evalId               :: !Int
  , evalJobsetId         :: !Int
  , evalEvaluationErrorId :: !(Maybe Int)
  , evalTimestamp        :: !Int
  , evalCheckoutTime     :: !Int
  , evalEvalTime         :: !Int
  , evalHasNewBuilds     :: !Int
  , evalHash             :: !Text
  , evalNrBuilds         :: !(Maybe Int)
  , evalNrSucceeded      :: !(Maybe Int)
  , evalFlake            :: !(Maybe Text)
  , evalNixExprInput     :: !(Maybe Text)
  , evalNixExprPath      :: !(Maybe Text)
  , evalProject          :: !Text  -- ^ Denormalized from jobsets JOIN
  , evalJobset           :: !Text  -- ^ Denormalized from jobsets JOIN
  } deriving (Show, Eq, Generic)

-- | An input to a jobset evaluation. Matches "jobsetevalinputs" table.
data JobsetEvalInput = JobsetEvalInput
  { jeiName       :: !Text
  , jeiAltNr      :: !Int
  , jeiType       :: !Text
  , jeiUri        :: !(Maybe Text)
  , jeiRevision   :: !(Maybe Text)
  , jeiValue      :: !(Maybe Text)
  , jeiDependency :: !(Maybe Int)
  } deriving (Show, Eq, Generic)

-- | Computed per-evaluation statistics for the eval list view.
-- Wraps a JobsetEval with aggregate counts and diff info.
data EvalInfo = EvalInfo
  { eiEval          :: !JobsetEval
  , eiNrScheduled   :: !Int
  , eiNrSucceeded   :: !Int
  , eiNrFailed      :: !Int
  , eiDiff          :: !Int                -- ^ Change in nrSucceeded vs previous eval
  , eiChangedInputs :: ![JobsetEvalInput]  -- ^ Inputs that differ from previous eval
  } deriving (Show, Eq, Generic)

-- | An evaluation error. Matches "evaluationerrors" table.
data EvaluationError = EvaluationError
  { eeId        :: !Int
  , eeErrorMsg  :: !(Maybe Text)
  , eeErrorTime :: !(Maybe Int)
  } deriving (Show, Eq, Generic)

-- | A currently-running evaluation (jobset with starttime set, still evaluating).
data RunningEval = RunningEval
  { reJobset    :: !Text         -- ^ Jobset name
  , reProject   :: !Text         -- ^ Project name
  , reStartTime :: !Int          -- ^ When evaluation started (epoch)
  , reDuration  :: !Int          -- ^ Seconds elapsed since start
  , reFlake     :: !(Maybe Text) -- ^ Flake URI (if flake-based)
  , reJobsetId  :: !Int          -- ^ Jobset ID (for linking)
  } deriving (Show, Eq, Generic)

-- | A queued evaluation (jobset with triggertime set, waiting to start).
data QueuedEval = QueuedEval
  { qeJobset      :: !Text         -- ^ Jobset name
  , qeProject     :: !Text         -- ^ Project name
  , qeTriggerTime :: !Int          -- ^ When evaluation was triggered (epoch)
  , qeFlake       :: !(Maybe Text) -- ^ Flake URI (if flake-based)
  , qeJobsetId    :: !Int          -- ^ Jobset ID
  , qeErrorMsg    :: !(Maybe Text) -- ^ Last error message (if any)
  } deriving (Show, Eq, Generic)
