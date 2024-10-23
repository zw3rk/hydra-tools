{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Lib.Hydra where

type JobSetId = Int
type EvalId   = Int
type BuildId  = Int

data Notification
    = EvalStarted JobSetId
    | EvalAdded JobSetId EvalId
    | EvalCached JobSetId EvalId
    | EvalFailed JobSetId
    | BuildQueued BuildId
    | BuildStarted BuildId
    | BuildFinished BuildId [BuildId]
    deriving (Show, Eq)

data BuildStatus
    = Succeeded
    | Failed
    | DependencyFailed
    | Aborted
    | Cancelled
    | FailedWithOutput
    | TimedOut
    | LogLimitExceeded
    | OutputSizeLimitExceeded
    | NonDeterministicBuild
    | Other
    deriving Eq

instance Show BuildStatus where
    show = \case
        Succeeded               -> "Build succeeded"
        Failed                  -> "Build failed"
        DependencyFailed        -> "Build dependency failed"
        Aborted                 -> "Build aborted"
        Cancelled               -> "Build cancelled"
        FailedWithOutput        -> "Build failed with output"
        TimedOut                -> "Build timed out"
        LogLimitExceeded        -> "Build log limit exceeded"
        OutputSizeLimitExceeded -> "Build output size limit exceeded"
        NonDeterministicBuild   -> "Build is non-deterministic"
        Other                   -> "Build failed due to unknown reason"

instance Enum BuildStatus where
    toEnum = \case
        ( 0) -> Succeeded
        ( 1) -> Failed
        ( 2) -> DependencyFailed
        ( 3) -> Aborted
        ( 9) -> Aborted
        ( 4) -> Cancelled
        ( 6) -> FailedWithOutput
        ( 7) -> TimedOut
        (10) -> LogLimitExceeded
        (11) -> OutputSizeLimitExceeded
        (12) -> NonDeterministicBuild
        ( _) -> Other
    fromEnum = \case
        Succeeded               ->  0
        Failed                  ->  1
        DependencyFailed        ->  2
        Aborted                 ->  3
        Cancelled               ->  4
        FailedWithOutput        ->  6
        TimedOut                ->  7
        LogLimitExceeded        -> 10
        OutputSizeLimitExceeded -> 11
        NonDeterministicBuild   -> 12
        Other                   -> 99
