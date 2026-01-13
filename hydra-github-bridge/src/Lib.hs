{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib where

import Data.Void (absurd)
import qualified Lib.GitHub as GitHub
import qualified Lib.Hydra as Hydra

toCheckRunConclusion :: Hydra.BuildStatus -> GitHub.CheckRunConclusion
toCheckRunConclusion = \case
  Hydra.Succeeded -> GitHub.Success
  Hydra.Failed -> GitHub.Failure
  Hydra.DependencyFailed -> GitHub.Failure
  Hydra.Aborted -> GitHub.Cancelled
  Hydra.Cancelled -> GitHub.Cancelled
  Hydra.FailedWithOutput -> GitHub.Failure
  Hydra.TimedOut -> GitHub.TimedOut
  Hydra.LogLimitExceeded -> GitHub.Failure
  Hydra.OutputSizeLimitExceeded -> GitHub.Failure
  Hydra.NonDeterministicBuild -> GitHub.Failure
  Hydra.Other -> GitHub.Failure

binarySearch :: Int -> Int -> (Int -> (Bool, a)) -> a
binarySearch low high find =
  either absurd id $ binarySearchM low high (Right . find)

binarySearchM :: (Monad m) => Int -> Int -> (Int -> m (Bool, a)) -> m a
binarySearchM low high find = do
  let mid = (low + high) `div` 2
  (higher, found) <- find mid
  if low == high
    then return found
    else
      if higher
        then binarySearchM (mid + 1) high find
        else binarySearchM low mid find
