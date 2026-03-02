-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Plain text and Markdown rendering for builds, evals, projects,
-- queue, and machines. Used for .txt and .md format dispatch endpoints.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Render
  ( -- * Project renderers
    renderProjectsText
  , renderProjectsMarkdown
  , renderProjectText
  , renderProjectMarkdown
    -- * Build renderers
  , renderBuildText
  , renderBuildMarkdown
    -- * Eval renderers
  , renderEvalText
  , renderEvalMarkdown
    -- * Jobset renderers
  , renderJobsetText
  , renderJobsetMarkdown
    -- * Queue renderers
  , renderQueueText
  , renderQueueMarkdown
    -- * Machines renderers
  , renderMachinesText
  , renderMachinesMarkdown
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import HydraWeb.Models.Build
import HydraWeb.Models.Eval (JobsetEval (..), EvalInfo (..))
import HydraWeb.Models.Project (Project (..), Jobset (..))
import HydraWeb.Models.Queue (QueueSummary (..), SystemQueueRow (..), ActiveStep (..))
import HydraWeb.View.BuildDiff (BuildDiff (..))
import HydraWeb.View.Components (fmtTime, fmtDuration, showT)

-- | Strip ssh:// prefix from machine names.
stripSSH :: Text -> Text
stripSSH t = fromMaybe t (Text.stripPrefix "ssh://" t)

-- | Truncate text to a maximum length.
trunc :: Int -> Text -> Text
trunc n t
  | Text.length t <= n = t
  | otherwise = Text.take (n - 3) t <> "..."

-- | Line separator.
sep :: Int -> Text
sep n = Text.replicate n "-"

-- ──────────────────────────────────────────────────────────────────────
-- Projects
-- ──────────────────────────────────────────────────────────────────────

-- | Project overview in plain text (for GET /.txt).
renderProjectsText :: [Project] -> Text
renderProjectsText ps = Text.unlines $
  [ "PROJECTS (" <> showT (length ps) <> ")"
  , ""
  , padR 30 "NAME" <> padR 8 "ENABLED" <> "DESCRIPTION"
  , sep 78
  ] ++ map renderRow ps
  where
    renderRow p =
      let enabled = if projEnabled p == 0 then "no" else "yes"
          desc    = trunc 38 (fromMaybe "" (projDescription p))
      in  padR 30 (projName p) <> padR 8 enabled <> desc

-- | Project overview in Markdown (for GET /.md).
renderProjectsMarkdown :: [Project] -> Text
renderProjectsMarkdown ps = Text.unlines $
  [ "# Projects (" <> showT (length ps) <> ")"
  , ""
  , "| Name | Enabled | Description |"
  , "|------|---------|-------------|"
  ] ++ map renderRow ps
  where
    renderRow p =
      let enabled = if projEnabled p == 0 then "no" else "yes"
          desc    = fromMaybe "" (projDescription p)
      in  "| " <> projName p <> " | " <> enabled <> " | " <> desc <> " |"

-- | Project detail in plain text (for GET /project/:name.txt).
renderProjectText :: Project -> [Jobset] -> Text
renderProjectText p js = Text.unlines $
  [ "PROJECT: " <> projDisplayName p
  , maybe "" (\d -> d <> "\n") (projDescription p)
  , ""
  , "JOBSETS (" <> showT (length js) <> ")"
  , padR 30 "NAME" <> padR 6 "OK" <> padR 6 "FAIL" <> padR 8 "QUEUED" <> "TOTAL"
  , sep 60
  ] ++ map renderRow js
  where
    renderRow j =
      padR 30 (jsName j) <> padR 6 (showT (jsNrSucceeded j)) <>
      padR 6 (showT (jsNrFailed j)) <> padR 8 (showT (jsNrScheduled j)) <>
      showT (jsNrTotal j)

-- | Project detail in Markdown (for GET /project/:name.md).
renderProjectMarkdown :: Project -> [Jobset] -> Text
renderProjectMarkdown p js = Text.unlines $
  [ "# " <> projDisplayName p
  , maybe "" (\d -> "\n" <> d <> "\n") (projDescription p)
  , "## Jobsets (" <> showT (length js) <> ")"
  , ""
  , "| Name | OK | Fail | Queued | Total |"
  , "|------|---:|-----:|-------:|------:|"
  ] ++ map renderRow js
  where
    renderRow j =
      "| " <> jsName j <>
      " | " <> showT (jsNrSucceeded j) <>
      " | " <> showT (jsNrFailed j) <>
      " | " <> showT (jsNrScheduled j) <>
      " | " <> showT (jsNrTotal j) <> " |"

-- ──────────────────────────────────────────────────────────────────────
-- Builds
-- ──────────────────────────────────────────────────────────────────────

-- | Build detail in plain text (for GET /build/:id.txt).
renderBuildText :: Build -> [BuildStep] -> [BuildOutput] -> Text
renderBuildText b steps outputs = Text.unlines $
  [ "BUILD #" <> showT (buildId b)
  , "Status:   " <> buildStatusText b
  , "Job:      " <> buildProject b <> ":" <> buildJobset b <> ":" <> buildJob b
  , "System:   " <> buildSystem b
  ] ++ durationLine ++ nixNameLine ++ outputLines ++ stepSection
  where
    durationLine = case (buildStartTime b, buildStopTime b) of
      (Just start, Just stop) -> ["Duration: " <> fmtDuration (stop - start)]
      _ -> []
    nixNameLine = case buildNixName b of
      Just n  -> ["Nix name: " <> n]
      Nothing -> []
    outputLines = map (\o -> "Output:   " <> boName o <> " → " <> fromMaybe "" (boPath o)) outputs
    stepSection
      | null steps = []
      | otherwise = ["", "BUILD STEPS"
                     , padR 4 "#" <> padR 12 "STATUS" <> padR 30 "MACHINE" <> "DURATION"
                     , sep 60
                     ] ++ map renderStep steps
    renderStep s =
      let dur = case (stepStartTime s, stepStopTime s) of
            (Just start, Just stop) -> fmtDuration (stop - start)
            _ -> ""
      in  padR 4 (showT (stepNr s)) <> padR 12 (stepStatusText s) <>
          padR 30 (stripSSH (stepMachine s)) <> dur

-- | Build detail in Markdown (for GET /build/:id.md).
renderBuildMarkdown :: Build -> [BuildStep] -> [BuildOutput] -> Text
renderBuildMarkdown b steps outputs = Text.unlines $
  [ "# Build #" <> showT (buildId b) <> " — " <> buildStatusText b
  , ""
  , "| Field | Value |"
  , "|-------|-------|"
  , "| Job | " <> buildProject b <> ":" <> buildJobset b <> ":" <> buildJob b <> " |"
  , "| System | " <> buildSystem b <> " |"
  ] ++ durationRow ++ outputRows ++ stepSection
  where
    durationRow = case (buildStartTime b, buildStopTime b) of
      (Just start, Just stop) -> ["| Duration | " <> fmtDuration (stop - start) <> " |"]
      _ -> []
    outputRows = map (\o ->
      "| Output (" <> boName o <> ") | `" <> fromMaybe "" (boPath o) <> "` |") outputs
    stepSection
      | null steps = []
      | otherwise =
        [ "", "## Build Steps", ""
        , "| # | Status | Machine | Duration |"
        , "|---|--------|---------|----------|"
        ] ++ map renderStep steps
    renderStep s =
      let dur = case (stepStartTime s, stepStopTime s) of
            (Just start, Just stop) -> fmtDuration (stop - start)
            _ -> ""
      in  "| " <> showT (stepNr s) <> " | " <> stepStatusText s <>
          " | " <> stripSSH (stepMachine s) <> " | " <> dur <> " |"

-- ──────────────────────────────────────────────────────────────────────
-- Evaluations
-- ──────────────────────────────────────────────────────────────────────

-- | Eval summary in plain text (for GET /eval/:id.txt).
renderEvalText :: JobsetEval -> BuildDiff -> Text
renderEvalText eval diff = Text.unlines $
  [ "EVALUATION #" <> showT (evalId eval)
  , "Jobset:     " <> evalProject eval <> ":" <> evalJobset eval
  , "Timestamp:  " <> fmtTime (evalTimestamp eval)
  , "Checkout:   " <> showT (evalCheckoutTime eval) <> "s"
  , "Eval time:  " <> showT (evalEvalTime eval) <> "s"
  ] ++ flakeLine ++
  [ "", "SUMMARY"
  , "  Now failing:      " <> showT (length (bdNowFail diff))
  , "  Now succeeding:   " <> showT (length (bdNowSucceed diff))
  , "  Still failing:    " <> showT (length (bdStillFail diff))
  , "  Still succeeding: " <> showT (length (bdStillSucceed diff))
  , "  New:              " <> showT (length (bdNew diff))
  , "  Queued:           " <> showT (length (bdUnfinished diff))
  , "  Aborted:          " <> showT (length (bdAborted diff))
  ]
  where
    flakeLine = case evalFlake eval of
      Just f  -> ["Flake:      " <> f]
      Nothing -> []

-- | Eval summary in Markdown (for GET /eval/:id.md).
renderEvalMarkdown :: JobsetEval -> BuildDiff -> Text
renderEvalMarkdown eval diff = Text.unlines $
  [ "# Evaluation #" <> showT (evalId eval)
  , ""
  , "| Field | Value |"
  , "|-------|-------|"
  , "| Jobset | " <> evalProject eval <> ":" <> evalJobset eval <> " |"
  , "| Timestamp | " <> fmtTime (evalTimestamp eval) <> " |"
  , "| Checkout time | " <> showT (evalCheckoutTime eval) <> "s |"
  , "| Eval time | " <> showT (evalEvalTime eval) <> "s |"
  ] ++ flakeRow ++
  [ "", "## Summary", ""
  , "| Category | Count |"
  , "|----------|------:|"
  , "| Now failing | " <> showT (length (bdNowFail diff)) <> " |"
  , "| Now succeeding | " <> showT (length (bdNowSucceed diff)) <> " |"
  , "| Still failing | " <> showT (length (bdStillFail diff)) <> " |"
  , "| Still succeeding | " <> showT (length (bdStillSucceed diff)) <> " |"
  , "| New | " <> showT (length (bdNew diff)) <> " |"
  , "| Queued | " <> showT (length (bdUnfinished diff)) <> " |"
  , "| Aborted | " <> showT (length (bdAborted diff)) <> " |"
  ]
  where
    flakeRow = case evalFlake eval of
      Just f  -> ["| Flake | `" <> f <> "` |"]
      Nothing -> []

-- ──────────────────────────────────────────────────────────────────────
-- Jobsets
-- ──────────────────────────────────────────────────────────────────────

-- | Jobset detail in plain text (for GET /jobset/:p/:j.txt).
renderJobsetText :: Jobset -> [EvalInfo] -> Text
renderJobsetText js evals = Text.unlines $
  [ "JOBSET " <> jsProject js <> ":" <> jsName js
  ] ++ flakeLine ++
  [ "Scheduled: " <> showT (jsNrScheduled js) <>
    "  Failed: " <> showT (jsNrFailed js) <>
    "  Succeeded: " <> showT (jsNrSucceeded js) <>
    "  Total: " <> showT (jsNrTotal js)
  ] ++ evalSection
  where
    flakeLine = case jsFlake js of
      Just f  -> ["Flake: " <> f]
      Nothing -> []
    evalSection
      | null evals = []
      | otherwise =
        [ "", "RECENT EVALUATIONS (" <> showT (length evals) <> ")"
        , padR 10 "ID" <> padR 24 "TIMESTAMP" <> padR 6 "BUILDS" <> "NEW"
        , sep 50
        ] ++ map renderEval evals
    renderEval ei =
      let eval = eiEval ei
          nb   = fromMaybe 0 (evalNrBuilds eval)
          new  = if evalHasNewBuilds eval /= 0 then "1" else "0"
      in  padR 10 (showT (evalId eval)) <> padR 24 (fmtTime (evalTimestamp eval)) <>
          padR 6 (showT nb) <> new

-- | Jobset detail in Markdown (for GET /jobset/:p/:j.md).
renderJobsetMarkdown :: Jobset -> [EvalInfo] -> Text
renderJobsetMarkdown js evals = Text.unlines $
  [ "# " <> jsProject js <> ":" <> jsName js
  ] ++ flakeLine ++
  [ ""
  , "| Metric | Count |"
  , "|--------|------:|"
  , "| Succeeded | " <> showT (jsNrSucceeded js) <> " |"
  , "| Failed | " <> showT (jsNrFailed js) <> " |"
  , "| Queued | " <> showT (jsNrScheduled js) <> " |"
  , "| Total | " <> showT (jsNrTotal js) <> " |"
  ] ++ evalSection
  where
    flakeLine = case jsFlake js of
      Just f  -> ["", "**Flake:** `" <> f <> "`"]
      Nothing -> []
    evalSection
      | null evals = []
      | otherwise =
        [ "", "## Recent Evaluations", ""
        , "| ID | Timestamp | Builds | New |"
        , "|----|-----------|-------:|----:|"
        ] ++ map renderEval evals
    renderEval ei =
      let eval = eiEval ei
          nb   = fromMaybe 0 (evalNrBuilds eval)
          new  = if evalHasNewBuilds eval /= 0 then "yes" else "no"
      in  "| " <> showT (evalId eval) <> " | " <> fmtTime (evalTimestamp eval) <>
          " | " <> showT nb <> " | " <> new <> " |"

-- ──────────────────────────────────────────────────────────────────────
-- Queue
-- ──────────────────────────────────────────────────────────────────────

-- | Queue summary in plain text (for GET /queue.txt).
renderQueueText :: [QueueSummary] -> [SystemQueueRow] -> Int -> Text
renderQueueText summary systems total = Text.unlines $
  [ "BUILD QUEUE (" <> showT total <> " queued)"
  , ""
  , padR 40 "JOBSET" <> padR 8 "QUEUED" <> padR 22 "OLDEST" <> "NEWEST"
  , sep 78
  ] ++ map renderRow summary ++
  [ ""
  , padR 20 "SYSTEM" <> "COUNT"
  , sep 30
  ] ++ map renderSys systems
  where
    renderRow s =
      padR 40 (qsProject s <> ":" <> qsJobset s) <> padR 8 (showT (qsQueued s)) <>
      padR 22 (fmtTime (qsOldest s)) <> fmtTime (qsNewest s)
    renderSys s =
      padR 20 (sqSystem s) <> showT (sqCount s)

-- | Queue summary in Markdown (for GET /queue.md).
renderQueueMarkdown :: [QueueSummary] -> [SystemQueueRow] -> Int -> Text
renderQueueMarkdown summary systems total = Text.unlines $
  [ "# Build Queue (" <> showT total <> " queued)"
  , ""
  , "| Jobset | Queued | Oldest | Newest |"
  , "|--------|-------:|--------|--------|"
  ] ++ map renderRow summary ++
  [ "", "## By System", ""
  , "| System | Count |"
  , "|--------|------:|"
  ] ++ map renderSys systems
  where
    renderRow s =
      "| " <> qsProject s <> ":" <> qsJobset s <>
      " | " <> showT (qsQueued s) <>
      " | " <> fmtTime (qsOldest s) <>
      " | " <> fmtTime (qsNewest s) <> " |"
    renderSys s =
      "| " <> sqSystem s <> " | " <> showT (sqCount s) <> " |"

-- ──────────────────────────────────────────────────────────────────────
-- Machines
-- ──────────────────────────────────────────────────────────────────────

-- | Active machines in plain text (for GET /machines.txt).
renderMachinesText :: [ActiveStep] -> Text
renderMachinesText steps = Text.unlines $
  [ "ACTIVE BUILD STEPS (" <> showT (length steps) <> ")"
  , ""
  , padR 30 "MACHINE" <> padR 8 "BUILD" <> padR 18 "SYSTEM" <> "JOB"
  , sep 90
  ] ++ map renderRow steps
  where
    renderRow s =
      padR 30 (stripSSH (asMachine s)) <> padR 8 (showT (asBuild s)) <>
      padR 18 (asSystem s) <>
      asProject s <> ":" <> asJobset s <> ":" <> asJob s

-- | Active machines in Markdown (for GET /machines.md).
renderMachinesMarkdown :: [ActiveStep] -> Text
renderMachinesMarkdown steps = Text.unlines $
  [ "# Active Build Steps (" <> showT (length steps) <> ")"
  , ""
  , "| Machine | Build | System | Job |"
  , "|---------|-------|--------|-----|"
  ] ++ map renderRow steps
  where
    renderRow s =
      "| " <> stripSSH (asMachine s) <>
      " | " <> showT (asBuild s) <>
      " | " <> asSystem s <>
      " | " <> asProject s <> ":" <> asJobset s <> ":" <> asJob s <> " |"

-- ──────────────────────────────────────────────────────────────────────
-- Helpers
-- ──────────────────────────────────────────────────────────────────────

-- | Right-pad a text to at least n characters.
padR :: Int -> Text -> Text
padR n t
  | Text.length t >= n = t <> " "
  | otherwise = t <> Text.replicate (n - Text.length t) " "
