-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Jobset page HTML: jobset info + paginated evaluations with progress bars.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Jobset
  ( jobsetPage
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Lucid

import HydraWeb.Models.Project (Jobset (..), hasErrorMsg)
import HydraWeb.Models.Eval (EvalInfo (..), JobsetEval (..), JobsetEvalInput (..))
import HydraWeb.View.Components (projectURL, evalURL, breadcrumb, progressBar, showT, shortRev, parsePRNumber)
import HydraWeb.View.Pager (pager)

-- | Render the jobset page content with breadcrumbs and progress bars.
-- When @showActions@ is True, a "Trigger Evaluation" button is shown.
-- The @mOrgRepo@ parameter enables a "View PR on GitHub" link for PR jobsets.
jobsetPage :: Text -> Maybe (Text, Text) -> Jobset -> [EvalInfo] -> Int -> Int -> Int -> Bool -> Html ()
jobsetPage bp mOrgRepo js evals total page perPage showActions = do
  -- Breadcrumb: Home / Project / Jobset
  breadcrumb [ ("Projects", bp <> "/")
             , (jsProject js, projectURL bp (jsProject js))
             , (jsName js, "")
             ]

  hgroup_ $ do
    h1_ $ toHtml (jsProject js <> " / " <> jsName js)
    case jsDescription js of
      Just d  -> p_ $ toHtml d
      Nothing -> pure ()

  -- GitHub PR link for pullrequest-* jobsets.
  case parsePRNumber (jsName js) of
    Just prNum | Just (org, repo) <- mOrgRepo ->
      p_ $ a_ [ href_ ("https://github.com/" <> org <> "/" <> repo <> "/pull/" <> showT prNum)
               , target_ "_blank"
               ] $ toHtml ("View PR #" <> showT prNum <> " on GitHub")
    _ -> pure ()

  -- Trigger evaluation button (authenticated users only).
  if showActions
    then form_ [ method_ "POST"
               , action_ (bp <> "/projects/" <> jsProject js <> "/jobsets/" <> jsName js <> "/trigger")
               ] $
      button_ [type_ "submit", class_ "btn outline"] "Trigger Evaluation"
    else pure ()

  -- Overall progress bar.
  progressBar (jsNrSucceeded js) (jsNrFailed js) (jsNrScheduled js)

  -- Running evaluation indicator.
  case jsStartTime js of
    Just _ -> div_ [class_ "running-eval"] $ do
      span_ [class_ "pulse"] ""
      em_ "Currently evaluating..."
    Nothing -> pure ()

  -- Error box.
  if hasErrorMsg js
    then article_ [class_ "error-box"] $ do
      header_ "Evaluation Error"
      pre_ $ do
        toHtml $ fromMaybe "" (jsErrorMsg js)
        toHtml $ fromMaybe "" (jsFetchErrorMsg js)
    else pure ()

  -- Jobset info details.
  details_ $ do
    summary_ "Jobset info"
    dl_ $ do
      dt_ "Type"
      dd_ $ if jsType js == 1 then "Flake" else "Legacy"
      case jsFlake js of
        Just f  -> do
          dt_ "Flake"
          dd_ $ code_ $ toHtml f
        Nothing -> pure ()
      dt_ "Check interval"
      dd_ $ toHtml (showT (jsCheckInterval js) <> "s")
      dt_ "Scheduling shares"
      dd_ $ toHtml (showT (jsSchedulingShares js))

  -- Evaluations table.
  h2_ "Evaluations"
  table_ $ do
    thead_ $ tr_ $ do
      th_ "#"
      th_ "Time"
      th_ "Input changes"
      th_ "Progress"
      th_ [class_ "num"] "Succeeded"
      th_ [class_ "num"] "Failed"
      th_ [class_ "num"] "Queued"
      th_ "\x0394"
    tbody_ $
      mapM_ (renderEvalInfo bp) evals

  -- Pagination.
  pager total page perPage

-- | Render a single eval row with progress bar.
renderEvalInfo :: Text -> EvalInfo -> Html ()
renderEvalInfo bp ei = do
  let eval = eiEval ei
  tr_ $ do
    td_ $ a_ [href_ (evalURL bp (evalId eval))] $ toHtml (showT (evalId eval))
    td_ $ toHtml (showT (evalTimestamp eval))
    td_ $ mapM_ renderChangedInput (eiChangedInputs ei)
    td_ $ progressBar (eiNrSucceeded ei) (eiNrFailed ei) (eiNrScheduled ei)
    td_ [class_ "num status-succeeded"] $ toHtml (showT (eiNrSucceeded ei))
    td_ [class_ "num status-failed"] $ toHtml (showT (eiNrFailed ei))
    td_ [class_ "num status-queued"] $ toHtml (showT (eiNrScheduled ei))
    td_ [class_ diffClass] $ do
      if eiDiff ei > 0 then toHtml ("+" <> showT (eiDiff ei))
      else if eiDiff ei < 0 then toHtml (showT (eiDiff ei))
      else pure ()
  where
    diffClass
      | eiDiff ei > 0 = "num status-succeeded"
      | eiDiff ei < 0 = "num status-failed"
      | otherwise      = "num"

-- | Render a changed input badge.
renderChangedInput :: JobsetEvalInput -> Html ()
renderChangedInput input = do
  span_ [class_ "badge"] $ do
    toHtml (jeiName input)
    case jeiRevision input of
      Just rev -> do
        ": "
        toHtml (shortRev rev)
      Nothing -> pure ()
