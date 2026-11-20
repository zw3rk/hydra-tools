-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- Pure tests for the aggregate @hydra@ check-run and the
-- @deployment_protection_rule@ gate: no database or network.
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value, eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Lib.Bridge.CommitStatus (CommitState (..), commitStateText, describeStatus)
import Lib.Bridge.Gates
import Lib.Bridge.HydraCheck
import Lib.Bridge.HydraToGitHub (checkRunPriority)
import Lib.GitHub.Client (CheckRunPayload (..))

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label

json :: LBS.ByteString -> Value
json s = either (error . ("bad test JSON: " <>)) id (eitherDecode s)

-- Aggregate summary --------------------------------------------------------

succeeded, failed, unfinished :: Text -> BuildRow
succeeded job = (job, 1, Just 0, Just 100, Just 200)
failed job = (job, 1, Just 1, Just 100, Just 300)
unfinished job = (job, 0, Nothing, Nothing, Nothing)

summaries :: IO ()
summaries = do
  let allGood = summariseEval Nothing [succeeded "a", succeeded "b"]
  assertEqual "all succeeded: outcome" HydraSucceeded allGood.outcome
  assertEqual "all succeeded: title" "2 jobs succeeded" allGood.title

  let someFailed = summariseEval (Just "") [succeeded "a", failed "z", failed "b"]
  assertEqual "failures: outcome" HydraFailed someFailed.outcome
  assertEqual "failures: title" "2 of 3 jobs failed" someFailed.title
  assertEqual "failures: listed sorted" ["b", "z"] someFailed.failedJobs

  -- A failure does not finish the evaluation while builds are pending.
  let pendingWithFailure = summariseEval Nothing [failed "a", unfinished "b"]
  assertEqual "pending: outcome" HydraPending pendingWithFailure.outcome
  assertEqual "pending: title" "1 of 2 jobs finished (1 failed)" pendingWithFailure.title
  assertEqual "pending: no completion time" Nothing pendingWithFailure.completedAt

  let noBuilds = summariseEval Nothing []
  assertEqual "zero builds: done" HydraSucceeded noBuilds.outcome
  assertEqual "zero builds: title" "No jobs to build" noBuilds.title

  -- An evaluation error is final, even with builds still running.
  let evalErrors = summariseEval (Just "error: in job 'foo': ...") [unfinished "a"]
  assertEqual "eval error: outcome" HydraFailed evalErrors.outcome
  assertEqual "eval error: title" "Evaluation has errors" evalErrors.title
  assertEqual "blank eval error is no error" HydraSucceeded (summariseEval (Just "  \n") [succeeded "a"]).outcome

  let payload = summaryCheckRunPayload "https://hydra.example" "" 42 "abc" pendingWithFailure
  assertEqual "payload name" "hydra" payload.name
  assertEqual "prefixed name" "ci/hydra-build:hydra" (hydraCheckName "ci/hydra-build:")
  assertEqual "payload details" (Just "https://hydra.example/eval/42") payload.detailsUrl

  assert "hydra is high priority" (checkRunPriority "hydra" == 1)
  assert "prefixed hydra is high priority" (checkRunPriority "ci/hydra-build:hydra" == 1)
  assertEqual "ci/eval first" 0 (checkRunPriority "ci/eval")
  assertEqual "required" 1 (checkRunPriority "x86_64-linux.required")
  assertEqual "ordinary job" 2 (checkRunPriority "x86_64-linux.hello")
  assertEqual "job merely containing hydra" 2 (checkRunPriority "x86_64-linux.hydra-tools")

-- Webhook payloads ---------------------------------------------------------

-- Shaped after GitHub's documented deployment_protection_rule payload
-- (fields trimmed to those the bridge reads plus some it ignores).
pullRequestPayload :: LBS.ByteString
pullRequestPayload =
  "{\"action\":\"requested\",\"environment\":\"hydra\",\"event\":\"pull_request\",\
  \\"sha\":\"1111111111111111111111111111111111111111\",\"ref\":\"refs/pull/7/merge\",\
  \\"deployment_callback_url\":\"https://api.github.com/repos/ican-group/haskell.nix/actions/runs/19601234567/deployment_protection_rule\",\
  \\"deployment\":null,\
  \\"pull_requests\":[{\"number\":7,\"head\":{\"sha\":\"2222222222222222222222222222222222222222\",\"ref\":\"feature\"},\"base\":{\"ref\":\"master\"}}],\
  \\"repository\":{\"full_name\":\"ican-group/haskell.nix\",\"name\":\"haskell.nix\"},\
  \\"installation\":{\"id\":1234},\"sender\":{\"login\":\"someone\"}}"

pushPayload :: LBS.ByteString
pushPayload =
  "{\"action\":\"requested\",\"environment\":\"hydra\",\"event\":\"push\",\
  \\"sha\":\"3333333333333333333333333333333333333333\",\"ref\":\"refs/tags/nix-tools-0.4.0\",\
  \\"deployment_callback_url\":\"https://api.github.com/repos/ican-group/haskell.nix/actions/runs/42/deployment_protection_rule\",\
  \\"pull_requests\":[],\
  \\"repository\":{\"full_name\":\"ican-group/haskell.nix\"}}"

payloads :: IO ()
payloads = do
  pr <- either fail pure (parseGateRequest (json pullRequestPayload))
  assertEqual "pr: head sha, not the merge commit" "2222222222222222222222222222222222222222" pr.headSha
  assertEqual "pr: run id" 19601234567 pr.runId
  assertEqual "pr: number" (Just 7) pr.prNumber
  assertEqual "pr: owner" "ican-group" pr.owner
  assertEqual "pr: repo" "haskell.nix" pr.repo
  assertEqual "pr: environment" "hydra" pr.environment

  push <- either fail pure (parseGateRequest (json pushPayload))
  assertEqual "push: sha" "3333333333333333333333333333333333333333" push.headSha
  assertEqual "push: no PR" Nothing push.prNumber
  assertEqual "push: run id" 42 push.runId

  assertEqual "pr without head falls back" "s" (gateHeadSha "pull_request" "s" [(1, Nothing)])
  assertEqual "push ignores PRs" "s" (gateHeadSha "push" "s" [(1, Just "h")])
  assertEqual "pull_request_target uses head" "h" (gateHeadSha "pull_request_target" "s" [(1, Just "h")])
  assertEqual "callback without run" Nothing (runIdFromCallbackUrl "https://api.github.com/repos/o/r/actions")
  assert "missing callback is an error" (either (const True) (const False) (parseGateRequest (json "{\"environment\":\"hydra\",\"event\":\"push\",\"sha\":\"x\",\"repository\":{\"full_name\":\"o/r\"}}")))

-- GitHub's documented example for validating webhook deliveries.
signatures :: IO ()
signatures = do
  let secret = "It's a Secret to Everybody"
      body = "Hello, World!"
      good = "sha256=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17"
  assert "valid signature" (verifySignature256 secret body (Just good))
  assert "upper-case hex" (verifySignature256 secret body (Just "sha256=757107EA0EB2509FC211221CCE984B8A37570B6D7586C22C46F4379C8B043E17"))
  assert "wrong body" (not (verifySignature256 secret "Hello, World?" (Just good)))
  assert "wrong secret" (not (verifySignature256 "other" body (Just good)))
  assert "sha1 header rejected" (not (verifySignature256 secret body (Just "sha1=757107ea0eb2509fc211221cce984b8a37570b6d")))
  assert "missing header" (not (verifySignature256 secret body Nothing))

-- Gate decisions -------------------------------------------------------------

facts :: GateFacts
facts =
  GateFacts
    { headSha = "abc",
      age = 60,
      reported = Nothing,
      state = Nothing,
      superseded = False,
      publicBase = "https://hydra.example"
    }

isReject, isReport :: GateDecision -> Bool
isReject (GateReject _) = True
isReject _ = False
isReport (GateReport _) = True
isReport _ = False

decisions :: IO ()
decisions = do
  let done o = Just (HydraState (Just 5) o "3 jobs succeeded")
  assertEqual
    "success approves with a link"
    (GateApprove "Hydra succeeded: 3 jobs succeeded https://hydra.example/eval/5")
    (decideGate facts {state = done HydraSucceeded})
  assert "failure rejects" (isReject (decideGate facts {state = done HydraFailed}))
  assert "failure rejects even when superseded" (isReject (decideGate facts {state = done HydraFailed, superseded = True}))

  -- Pending: one report per distinct message, then wait.
  let pending = facts {state = Just (HydraState (Just 5) HydraPending "1 of 3 jobs finished")}
  case decideGate pending of
    GateReport msg -> do
      assert "pending report names the eval" ("evaluation 5" `Text.isInfixOf` msg)
      assertEqual "same report is not repeated" GateWait (decideGate pending {reported = Just msg})
      -- The build counts are not in the message, so progress alone sends nothing.
      assertEqual
        "progress does not re-report"
        GateWait
        (decideGate pending {reported = Just msg, state = Just (HydraState (Just 5) HydraPending "2 of 3 jobs finished")})
    other -> fail ("pending: expected a report, got " <> show other)

  -- Hydra has not started yet.
  assert "not started: report" (isReport (decideGate facts))
  assert "evaluating (no eval yet): report" (isReport (decideGate facts {state = Just (HydraState Nothing HydraPending "Evaluating")}))
  assert "evaluating is never timed out" (isReport (decideGate facts {age = 48 * 3600, state = Just (HydraState Nothing HydraPending "Evaluating")}))
  assert "superseded but young: still waits" (isReport (decideGate facts {superseded = True}))
  assert "superseded and older than 30 min: rejected" (isReject (decideGate facts {superseded = True, age = 31 * 60}))
  assert "no eval in 24 hours: rejected" (isReject (decideGate facts {age = 25 * 3600}))

-- Waiting runs -----------------------------------------------------------------

waitingRuns :: IO ()
waitingRuns = do
  runs <-
    either fail pure $
      parseWaitingRuns $
        json
          "{\"total_count\":2,\"workflow_runs\":[\
          \{\"id\":11,\"event\":\"pull_request\",\"head_sha\":\"aaa\",\"pull_requests\":[{\"number\":3}]},\
          \{\"id\":12,\"event\":\"push\",\"head_sha\":\"bbb\",\"pull_requests\":[]}]}"
  assertEqual
    "waiting runs"
    [WaitingRun 11 "pull_request" "aaa" (Just 3), WaitingRun 12 "push" "bbb" Nothing]
    runs
  envs <-
    either fail pure $
      parsePendingEnvironments $
        json "[{\"environment\":{\"id\":1,\"name\":\"hydra\"},\"wait_timer\":0},{\"environment\":{\"name\":\"prod\"}}]"
  assertEqual "pending environments" ["hydra", "prod"] envs
  assertEqual "hydra environment name" "hydra" hydraEnvironment

-- Commit statuses ------------------------------------------------------------

commitStatuses :: IO ()
commitStatuses = do
  let t p c = hydraCommitState (Transition p c)
  assertEqual "first pending posts pending" (Just StatusPending) (t Nothing HydraPending)
  assertEqual "first failure posts failure" (Just StatusFailure) (t Nothing HydraFailed)
  assertEqual "pending -> success" (Just StatusSuccess) (t (Just HydraPending) HydraSucceeded)
  assertEqual "pending -> failure" (Just StatusFailure) (t (Just HydraPending) HydraFailed)
  assertEqual "restart: success -> pending" (Just StatusPending) (t (Just HydraSucceeded) HydraPending)
  assertEqual "failure -> success" (Just StatusSuccess) (t (Just HydraFailed) HydraSucceeded)
  assertEqual "progress within pending posts nothing" Nothing (t (Just HydraPending) HydraPending)
  assertEqual "title change within success posts nothing" Nothing (t (Just HydraSucceeded) HydraSucceeded)
  assertEqual "states" ["pending", "success", "failure"] (map commitStateText [StatusPending, StatusSuccess, StatusFailure])
  assertEqual "description is at most 140 characters" 140 (Text.length (describeStatus (Text.replicate 200 "x")))
  assertEqual "short description kept" "2 jobs succeeded" (describeStatus "2 jobs succeeded")

main :: IO ()
main = do
  commitStatuses
  summaries
  payloads
  signatures
  decisions
  waitingRuns
  putStrLn "hydra-gate: all tests passed"
