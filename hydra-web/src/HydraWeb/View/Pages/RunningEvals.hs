-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Running and queued evaluations page HTML.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.RunningEvals
  ( runningEvalsPage
  , runningEvalsContent
  , renderRunningEvalsContentBS
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Lucid

import HydraWeb.Models.Eval (RunningEval (..), QueuedEval (..))
import HydraWeb.View.Components (projectURL, jobsetURL, fmtTime, fmtDuration, showT)

-- | Full running evaluations page (running + queued).
runningEvalsPage :: Text -> [RunningEval] -> [QueuedEval] -> Html ()
runningEvalsPage bp evals queued = do
  h1_ "Evaluations"
  runningEvalsContent bp evals
  queuedEvalsContent bp queued

-- | Running evaluations section.
runningEvalsContent :: Text -> [RunningEval] -> Html ()
runningEvalsContent bp evals = do
  h2_ $ toHtml ("Running (" <> showT (length evals) <> ")")
  case evals of
    [] -> p_ "No evaluations currently running."
    _  -> table_ $ do
      thead_ $ tr_ $ do
        th_ "Project"
        th_ "Jobset"
        th_ "Started"
        th_ "Duration"
        th_ "Flake"
      tbody_ $ mapM_ (renderRunningEval bp) evals

-- | Render a single running evaluation row.
renderRunningEval :: Text -> RunningEval -> Html ()
renderRunningEval bp re = tr_ [class_ "running-eval"] $ do
  td_ $ a_ [href_ (projectURL bp (reProject re))] $ toHtml (reProject re)
  td_ $ a_ [href_ (jobsetURL bp (reProject re) (reJobset re))] $ toHtml (reJobset re)
  td_ $ toHtml (fmtTime (reStartTime re))
  td_ [class_ "eval-duration"] $ do
    span_ [class_ "pulse"] ""
    toHtml (fmtDuration (reDuration re))
  td_ $ case reFlake re of
    Just f  -> code_ $ toHtml f
    Nothing -> em_ "legacy"

-- | Queued evaluations section.
queuedEvalsContent :: Text -> [QueuedEval] -> Html ()
queuedEvalsContent bp queued = do
  h2_ $ toHtml ("Queued (" <> showT (length queued) <> ")")
  case queued of
    [] -> p_ "No evaluations waiting."
    _  -> table_ $ do
      thead_ $ tr_ $ do
        th_ "#"
        th_ "Project"
        th_ "Jobset"
        th_ "Triggered"
        th_ "Flake"
        th_ "Status"
      tbody_ $ mapM_ (\(i, qe) -> renderQueuedEval bp i qe) (zip [1::Int ..] queued)

-- | Render a single queued evaluation row.
renderQueuedEval :: Text -> Int -> QueuedEval -> Html ()
renderQueuedEval bp pos qe = tr_ $ do
  td_ [class_ "num"] $ toHtml (showT pos)
  td_ $ a_ [href_ (projectURL bp (qeProject qe))] $ toHtml (qeProject qe)
  td_ $ a_ [href_ (jobsetURL bp (qeProject qe) (qeJobset qe))] $ toHtml (qeJobset qe)
  td_ $ toHtml (fmtTime (qeTriggerTime qe))
  td_ $ case qeFlake qe of
    Just f  -> code_ $ toHtml f
    Nothing -> em_ "legacy"
  td_ $ case qeErrorMsg qe of
    Just _  -> span_ [class_ "status-failed", title_ "Has previous error"] "error"
    Nothing -> span_ [class_ "status-ok"] "waiting"

-- | Render the running evals section to a strict ByteString (for SSE broadcasting).
renderRunningEvalsContentBS :: Text -> [RunningEval] -> ByteString
renderRunningEvalsContentBS bp evals = LBS.toStrict $ renderBS $ runningEvalsContent bp evals
