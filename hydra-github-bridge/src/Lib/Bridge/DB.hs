-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Bridge.DB
  ( hasPriorReportedFailure,
    fetchPriorFailedEvalJobs,
  )
where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, In (..), Only (..), query)
import Lib.GitHub qualified as GitHub

-- | True iff the bridge has previously enqueued a failure-class conclusion
-- (see 'GitHub.failureConclusions') for the given commit+job. Used so that
-- once GitHub has been told a job is broken, subsequent transitions
-- (queued/in_progress/success) keep being reported even for jobs whose name
-- does not match the required/nonrequired keyword.
--
-- Callers pass the fully-qualified check-run name exactly as it is emitted
-- (i.e. @checkRunPrefix <> job@ for builds), so status lookup and emission
-- use matching keys. This replaces the earlier heuristic over Hydra's
-- @buildsteps@ table, which broke once unrelated newer steps were appended.
hasPriorReportedFailure ::
  Connection ->
  -- | repo owner
  Text ->
  -- | repo name
  Text ->
  -- | head sha
  Text ->
  -- | check-run name
  Text ->
  IO Bool
hasPriorReportedFailure conn owner repo headSha name = do
  [Only exists] <-
    query
      conn
      "SELECT EXISTS ( \
      \  SELECT 1 FROM github_status_payload p \
      \  JOIN github_status s ON p.status_id = s.id \
      \  WHERE s.owner = ? AND s.repo = ? AND s.headSha = ? AND s.name = ? \
      \    AND p.payload->>'conclusion' IN ? \
      \)"
      ( owner,
        repo,
        headSha,
        name,
        In (map GitHub.conclusionText GitHub.failureConclusions)
      )
  pure exists

-- | Names of all per-job evaluation check-runs (i.e. @ci/eval:<job>@) for
-- this commit whose most recent payload was a failure-class conclusion. These
-- are the ones that should be cleared with a fresh success status the next
-- time the evaluation succeeds.
--
-- Evaluation check-run names are fixed @ci/eval@ constants (never prefixed),
-- so the @LIKE@ pattern below is a constant too: no configured value is
-- interpolated into it and there are no wildcard surprises. Build check-runs
-- (which do carry the configured prefix) never match @ci/eval:...@.
fetchPriorFailedEvalJobs ::
  Connection ->
  -- | repo owner
  Text ->
  -- | repo name
  Text ->
  -- | head sha
  Text ->
  IO [Text]
fetchPriorFailedEvalJobs conn owner repo headSha = do
  rows <-
    query
      conn
      "SELECT s.name FROM github_status s \
      \WHERE s.owner = ? AND s.repo = ? AND s.headSha = ? \
      \  AND s.name LIKE 'ci/eval:%' \
      \  AND ( \
      \    SELECT p.payload->>'conclusion' FROM github_status_payload p \
      \    WHERE p.status_id = s.id ORDER BY p.id DESC LIMIT 1 \
      \  ) IN ?"
      ( owner,
        repo,
        headSha,
        In (map GitHub.conclusionText GitHub.failureConclusions)
      )
  pure (map fromOnly rows)
