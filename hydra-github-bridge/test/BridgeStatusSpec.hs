-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- Regressions for the retry-status and per-job evaluation reporting batch
-- (upstream fix-retries: 11a0629, 9bf8629, 1e33bb3).
--
-- Pure policy tests cover required/quiet gating and every failure-class
-- conclusion. Database tests run against the same private throwaway
-- PostgreSQL cluster seam as sync-source (HYDRA_SYNC_TEST_DATABASE) and
-- cover the bridge-table lookups: prior-failure stickiness without Hydra's
-- buildsteps table, owner/repo/head/name isolation, latest-payload eval
-- cleanup selection, and exclusion of build and aggregate eval names.
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless, void)
import Data.Aeson (object, toJSON, (.=))
import Data.ByteString.Char8 qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL, execute, execute_)
import Lib.Bridge.DB qualified as BridgeDB
import Lib.Bridge.HydraToGitHub (EmitDecision (..), decideEmit, hasRequiredKeyword, whenStatusOrJob)
import Lib.GitHub.Client (CheckRunConclusion (..), conclusionText, failureConclusions)
import System.Environment (getEnv)

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)

-- Pure emission policy: required/quiet jobs and every failure class.

purePolicy :: IO ()
purePolicy = do
  -- Keyword matching: prefix, suffix, dot-delimited component.
  assertEqual "bare required" True (hasRequiredKeyword "required")
  assertEqual "hyphenated required prefix" True (hasRequiredKeyword "required-aarch64-linux")
  assertEqual "hyphenated required suffix" True (hasRequiredKeyword "build-required")
  assertEqual "dot-delimited required" True (hasRequiredKeyword "x86_64-linux.required.foo")
  assertEqual "bare nonrequired" True (hasRequiredKeyword "nonrequired")
  assertEqual "dot-delimited nonrequired" True (hasRequiredKeyword "foo.nonrequired.bar")
  assertEqual "suffix nonrequired" True (hasRequiredKeyword "job-nonrequired")
  -- Negatives: mid-word embedding and lookalikes must not match.
  assertEqual "unrelated job" False (hasRequiredKeyword "build")
  assertEqual "mid-word embedding" False (hasRequiredKeyword "foo-not-required-yet")
  assertEqual "requirement lookalike" False (hasRequiredKeyword "requirement")
  assertEqual "truncated marker" False (hasRequiredKeyword "nonrequire")
  assertEqual "empty name" False (hasRequiredKeyword "")
  assertEqual "eval name carries no keyword" False (hasRequiredKeyword "ci/eval:retry-job")

  -- Required jobs always emit, whatever the current conclusion is.
  let requiredJob = "x86_64-linux.required.build"
  assertEqual "required, no status" Emit (decideEmit Nothing requiredJob)
  assertEqual "required, success" Emit (decideEmit (Just Success) requiredJob)
  assertEqual "required, failure" Emit (decideEmit (Just Failure) requiredJob)
  assertEqual "nonrequired keyword emits" Emit (decideEmit Nothing "foo.nonrequired.bar")

  -- Quiet jobs emit immediately only for failure-class conclusions.
  let quietJob = "x86_64-linux.build"
  assertEqual "quiet failure" Emit (decideEmit (Just Failure) quietJob)
  assertEqual "quiet cancelled" Emit (decideEmit (Just Cancelled) quietJob)
  assertEqual "quiet stale" Emit (decideEmit (Just Stale) quietJob)
  assertEqual "quiet timed out" Emit (decideEmit (Just TimedOut) quietJob)
  -- Everything else defers to the prior-failure lookup so retries keep
  -- flowing once a failure was reported.
  assertEqual "quiet success defers" CheckPriorFailure (decideEmit (Just Success) quietJob)
  assertEqual "quiet neutral defers" CheckPriorFailure (decideEmit (Just Neutral) quietJob)
  assertEqual "quiet action-required defers" CheckPriorFailure (decideEmit (Just ActionRequired) quietJob)
  assertEqual "quiet skipped defers" CheckPriorFailure (decideEmit (Just Skipped) quietJob)
  assertEqual "quiet no-status defers" CheckPriorFailure (decideEmit Nothing quietJob)

  -- Single source of truth: encoding and failure list cannot drift.
  assertEqual "failure list" [Failure, Cancelled, Stale, TimedOut] failureConclusions
  assertEqual "encode failure" "failure" (conclusionText Failure)
  assertEqual "encode cancelled" "cancelled" (conclusionText Cancelled)
  assertEqual "encode stale" "stale" (conclusionText Stale)
  assertEqual "encode timed_out" "timed_out" (conclusionText TimedOut)
  assertEqual "encode success" "success" (conclusionText Success)
  assertEqual "encode neutral" "neutral" (conclusionText Neutral)
  assertEqual "encode skipped" "skipped" (conclusionText Skipped)
  assertEqual "encode action_required" "action_required" (conclusionText ActionRequired)
  mapM_
    (\c -> assertEqual ("json matches text: " <> show c) (toJSON (conclusionText c)) (toJSON c))
    [ActionRequired, Cancelled, Failure, Neutral, Success, Skipped, Stale, TimedOut]

-- Bridge-table lookups against a private throwaway cluster.
--
-- NOTE: this database deliberately contains no Hydra tables (no builds,
-- no buildsteps). The retired heuristic read buildsteps; the ported lookup
-- must answer from the bridge's own status records alone, so any regression
-- back to buildsteps fails here with a missing-table error.

resetBridgeTables :: Connection -> IO ()
resetBridgeTables conn = do
  void $ execute_ conn "DROP TABLE IF EXISTS github_status_payload"
  void $ execute_ conn "DROP TABLE IF EXISTS github_status"
  void $ execute_ conn "CREATE TABLE github_status (id SERIAL PRIMARY KEY, owner TEXT NOT NULL, repo TEXT NOT NULL, headSha TEXT NOT NULL, name TEXT NOT NULL, UNIQUE (owner, repo, headSha, name))"
  void $ execute_ conn "CREATE TABLE github_status_payload (id SERIAL PRIMARY KEY, status_id INTEGER NOT NULL REFERENCES github_status (id) ON DELETE CASCADE, payload JSONB NOT NULL, created TIMESTAMP DEFAULT NOW(), sent TIMESTAMP DEFAULT NULL, tries INTEGER DEFAULT 0)"

insertPayload :: Connection -> Text -> Text -> Text -> Text -> Text -> IO ()
insertPayload conn owner repo headSha name conclusion =
  void $
    execute
      conn
      "WITH s AS (INSERT INTO github_status (owner, repo, headSha, name) VALUES (?,?,?,?) ON CONFLICT (owner, repo, headSha, name) DO UPDATE SET name = EXCLUDED.name RETURNING id) INSERT INTO github_status_payload (status_id, payload) SELECT id, ? FROM s"
      (owner, repo, headSha, name, object ["conclusion" .= conclusion, "status" .= ("completed" :: Text)])

priorFailure :: IO ()
priorFailure = withBridgeDb $ \conn -> do
  resetBridgeTables conn
  let check o r h n = BridgeDB.hasPriorReportedFailure conn o r h n
  assert "empty database reports no prior failure" . not =<< check "o" "r" "h" "ci/hydra-build:quiet"
  insertPayload conn "o" "r" "h" "ci/hydra-build:quiet" "failure"
  assert "failure payload reports prior failure" =<< check "o" "r" "h" "ci/hydra-build:quiet"
  -- Sticky: a later success payload does not erase the earlier failure, so a
  -- subsequent retry still emits.
  insertPayload conn "o" "r" "h" "ci/hydra-build:quiet" "success"
  assert "earlier failure still counts after success" =<< check "o" "r" "h" "ci/hydra-build:quiet"
  -- Isolation: every key component matters.
  assert "other owner isolated" . not =<< check "other" "r" "h" "ci/hydra-build:quiet"
  assert "other repo isolated" . not =<< check "o" "other" "h" "ci/hydra-build:quiet"
  assert "other head isolated" . not =<< check "o" "r" "other" "ci/hydra-build:quiet"
  assert "other name isolated" . not =<< check "o" "r" "h" "ci/hydra-build:other"
  assert "unprefixed name isolated from prefixed emission" . not =<< check "o" "r" "h" "quiet"
  -- Every failure-class conclusion counts; non-failure classes do not.
  resetBridgeTables conn
  mapM_
    ( \c -> do
        insertPayload conn "o" "r" "h" ("n:" <> c) c
        assert ("failure class counts: " <> show c) =<< check "o" "r" "h" ("n:" <> c)
    )
    ["failure", "cancelled", "stale", "timed_out"]
  mapM_
    ( \c -> do
        insertPayload conn "o" "r" "h" ("q:" <> c) c
        assert ("non-failure class ignored: " <> show c) . not =<< check "o" "r" "h" ("q:" <> c)
    )
    ["success", "neutral", "skipped", "action_required"]

failedEvalJobs :: IO ()
failedEvalJobs = withBridgeDb $ \conn -> do
  resetBridgeTables conn
  let fetch = BridgeDB.fetchPriorFailedEvalJobs conn "o" "r" "h"
  assertEqual "empty database clears nothing" [] =<< fetch
  insertPayload conn "o" "r" "h" "ci/eval:job-a" "failure"
  insertPayload conn "o" "r" "h" "ci/eval:job-b" "failure"
  insertPayload conn "o" "r" "h" "ci/eval:job-b" "success"
  insertPayload conn "o" "r" "h" "ci/eval:job-c" "success"
  insertPayload conn "o" "r" "h" "ci/eval:job-c" "failure"
  insertPayload conn "o" "r" "h" "ci/eval:job-d" "cancelled"
  insertPayload conn "o" "r" "h" "ci/eval" "failure"
  insertPayload conn "o" "r" "h" "ci/hydra-build:quiet" "failure"
  insertPayload conn "o" "r" "other" "ci/eval:job-a" "failure"
  insertPayload conn "other" "r" "h" "ci/eval:job-a" "failure"
  insertPayload conn "o" "r" "h" "ci/eval:100%_x" "timed_out"
  -- Latest payload wins: job-b healed, job-c regressed; the aggregate
  -- ci/eval status and build check-runs are never per-job clears; other
  -- owner/repo commits are isolated.
  assertEqual
    "latest-payload per-job selection"
    ["ci/eval:100%_x", "ci/eval:job-a", "ci/eval:job-c", "ci/eval:job-d"]
    . sort
    =<< fetch
  assertEqual
    "other head isolated"
    ["ci/eval:job-a"]
    =<< BridgeDB.fetchPriorFailedEvalJobs conn "o" "r" "other"
  -- After the retry clears a job (latest payload becomes success), a later
  -- successful eval finds nothing to clear: no repeat clearing.
  insertPayload conn "o" "r" "h" "ci/eval:job-a" "success"
  insertPayload conn "o" "r" "h" "ci/eval:job-c" "success"
  insertPayload conn "o" "r" "h" "ci/eval:job-d" "success"
  insertPayload conn "o" "r" "h" "ci/eval:100%_x" "success"
  assertEqual "cleared jobs are not cleared again" [] =<< fetch

-- Exercise the production emission gate with real bridge records. The
-- fixture has no BuildSteps table, so unrelated Hydra step history cannot
-- supply the prior-failure decision.
retryTransitions :: IO ()
retryTransitions = withBridgeDb $ \conn -> do
  resetBridgeTables conn
  emitted <- newIORef ([] :: [Text])
  let checkName = "custom-check:quiet-job"
      prior = BridgeDB.hasPriorReportedFailure conn "o" "r" "h" checkName
      emit phase = modifyIORef' emitted (<> [phase]) >> pure []
      transition conclusion phase =
        void $ whenStatusOrJob True conclusion prior "quiet-job" (emit phase)
  transition Nothing "unreported-queued"
  assertEqual "quiet job without prior failure stays quiet" [] =<< readIORef emitted
  transition (Just Failure) "failed"
  -- notificationWatcher persists emitted statuses in the same tables.
  insertPayload conn "o" "r" "h" checkName "failure"
  transition Nothing "queued"
  transition Nothing "started"
  transition (Just Success) "success"
  assertEqual "failed retry emits all transitions" ["failed", "queued", "started", "success"] =<< readIORef emitted
  void $
    whenStatusOrJob
      True
      Nothing
      (BridgeDB.hasPriorReportedFailure conn "o" "r" "h" "other-prefix:quiet-job")
      "quiet-job"
      (emit "wrong-prefix")
  assertEqual "custom prefixes remain isolated" ["failed", "queued", "started", "success"] =<< readIORef emitted
  void $ whenStatusOrJob False Nothing (fail "unfiltered jobs must not query prior failure") "quiet-job" (emit "unfiltered")
  void $ whenStatusOrJob True Nothing (fail "required jobs must not query prior failure") "x86_64-linux.required" (emit "required")
  assertEqual
    "filter flag and required job bypass lookup"
    ["failed", "queued", "started", "success", "unfiltered", "required"]
    =<< readIORef emitted

withBridgeDb :: (Connection -> IO ()) -> IO ()
withBridgeDb action = do
  -- This must name the throwaway cluster created by run-sync-source.sh.
  -- Never fall back to the developer's/default PostgreSQL database.
  database <- ByteString.pack <$> getEnv "HYDRA_SYNC_TEST_DATABASE"
  bracket (connectPostgreSQL database) close $ \conn -> do
    void $ execute_ conn "SET statement_timeout = '5s'"
    action conn

main :: IO ()
main = do
  purePolicy
  putStrLn "Pure emission-policy regressions passed"
  priorFailure
  putStrLn "Prior-failure lookup regressions passed"
  retryTransitions
  putStrLn "Retry transition and filtering regressions passed"
  failedEvalJobs
  putStrLn "Failed-eval cleanup regressions passed"
  putStrLn "All bridge-status regressions passed"
