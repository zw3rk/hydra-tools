-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Running evaluations page HTML.
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

import HydraWeb.Models.Eval (RunningEval (..))
import HydraWeb.View.Components (projectURL, jobsetURL, fmtTime, fmtDuration)

-- | Full running evaluations page.
runningEvalsPage :: Text -> [RunningEval] -> Html ()
runningEvalsPage bp evals = do
  h1_ "Running Evaluations"
  runningEvalsContent bp evals

-- | Reusable content partial for both initial render and SSE updates.
runningEvalsContent :: Text -> [RunningEval] -> Html ()
runningEvalsContent bp evals = do
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

-- | Render the content partial to a strict ByteString (for SSE broadcasting).
renderRunningEvalsContentBS :: Text -> [RunningEval] -> ByteString
renderRunningEvalsContentBS bp = LBS.toStrict . renderBS . runningEvalsContent bp
