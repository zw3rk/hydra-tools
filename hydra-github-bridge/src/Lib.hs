{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Lib where

import qualified Lib.GitHub as GitHub
import qualified Lib.Hydra  as Hydra

toCheckRunConclusion :: Hydra.BuildStatus -> GitHub.CheckRunConclusion
toCheckRunConclusion = \case
    (Hydra.Succeeded)               -> GitHub.Success
    (Hydra.Failed)                  -> GitHub.Failure
    (Hydra.DependencyFailed)        -> GitHub.Failure
    (Hydra.Aborted)                 -> GitHub.Cancelled
    (Hydra.Cancelled)               -> GitHub.Cancelled
    (Hydra.FailedWithOutput)        -> GitHub.Failure
    (Hydra.TimedOut)                -> GitHub.TimedOut
    (Hydra.LogLimitExceeded)        -> GitHub.Failure
    (Hydra.OutputSizeLimitExceeded) -> GitHub.Failure
    (Hydra.NonDeterministicBuild)   -> GitHub.Failure
    (Hydra.Other)                   -> GitHub.Failure
