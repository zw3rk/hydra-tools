-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Evaluation page HTML (eval info + inputs + build diff tabs).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Eval
  ( evalPage
  , evalTabContent
  , latestEvalsPage
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import HydraWeb.Models.Eval
import HydraWeb.Models.Build (Build (..))
import HydraWeb.View.BuildDiff (BuildDiff (..), defaultCategory)
import HydraWeb.View.Components
import HydraWeb.View.HTMX (hxGet_, hxTarget_)
import HydraWeb.View.Pager (pager)

-- | Render the eval detail page content.
evalPage :: Text -> JobsetEval -> [JobsetEvalInput] -> Maybe EvaluationError
         -> BuildDiff -> Html ()
evalPage bp eval inputs evalErr diff = do
  hgroup_ $ do
    h1_ $ toHtml ("Evaluation #" <> showT (evalId eval))
    p_ $ do
      a_ [href_ (projectURL bp (evalProject eval))] $ toHtml (evalProject eval)
      " / "
      a_ [href_ (jobsetURL bp (evalProject eval) (evalJobset eval))]
        $ toHtml (evalJobset eval)

  -- Eval info.
  dl_ [class_ "eval-info"] $ do
    dt_ "Timestamp"
    dd_ $ toHtml (fmtTime (evalTimestamp eval))
    dt_ "Checkout time"
    dd_ $ toHtml (showT (evalCheckoutTime eval) <> "s")
    dt_ "Eval time"
    dd_ $ toHtml (showT (evalEvalTime eval) <> "s")
    case evalFlake eval of
      Just f -> do
        dt_ "Flake"
        dd_ $ code_ $ toHtml f
      Nothing -> pure ()

  -- Inputs.
  case inputs of
    [] -> pure ()
    _  -> details_ $ do
      summary_ $ toHtml ("Inputs (" <> showT (length inputs) <> ")")
      table_ $ do
        thead_ $ tr_ $ do
          th_ "Name"; th_ "Type"; th_ "URI"; th_ "Revision"
        tbody_ $ mapM_ renderInput inputs

  -- Error box.
  case evalErr of
    Just ee -> article_ [class_ "error-box"] $ do
      header_ "Evaluation Error"
      pre_ $ toHtml (fromMaybe "" (eeErrorMsg ee))
    Nothing -> pure ()

  -- Build diff tabs.
  h2_ "Builds"
  div_ [role_ "tablist"] $ do
    tabButton bp eval "now-fail" "Now Fail" "status-failed" (bdNowFail diff)
    tabButton bp eval "now-succeed" "Now Succeed" "status-succeeded" (bdNowSucceed diff)
    tabButton bp eval "new" "New" "" (bdNew diff)
    tabButton bp eval "still-fail" "Still Fail" "status-failed" (bdStillFail diff)
    tabButton bp eval "still-succeed" "Still Succeed" "status-succeeded" (bdStillSucceed diff)
    tabButton bp eval "unfinished" "Queued" "status-queued" (bdUnfinished diff)
    tabButton bp eval "aborted" "Aborted" "status-aborted" (bdAborted diff)

  -- Default tab content.
  div_ [id_ "tab-content"] $ do
    let defBuilds = case defaultCategory diff of
          Just "now-fail"      -> bdNowFail diff
          Just "still-fail"    -> bdStillFail diff
          Just "now-succeed"   -> bdNowSucceed diff
          Just "unfinished"    -> bdUnfinished diff
          Just "still-succeed" -> bdStillSucceed diff
          Just "new"           -> bdNew diff
          Just "aborted"       -> bdAborted diff
          _                    -> []
    buildListTable bp defBuilds

-- | Render a tab button (only if the category is non-empty).
tabButton :: Text -> JobsetEval -> Text -> Text -> Text -> [Build] -> Html ()
tabButton bp eval tabName label cls builds =
  case builds of
    [] -> pure ()
    _  -> button_ ([role_ "tab"
                   , hxGet_ (evalURL bp (evalId eval) <> "/tab/" <> tabName)
                   , hxTarget_ "#tab-content"
                   ] ++ if Text.null cls then [] else [class_ cls]) $
            toHtml (label <> " (" <> showT (length builds) <> ")")

-- | Render tab content (HTMX partial response).
evalTabContent :: Text -> Text -> BuildDiff -> Html ()
evalTabContent bp tabName diff =
  let builds = case tabName of
        "now-fail"      -> bdNowFail diff
        "now-succeed"   -> bdNowSucceed diff
        "new"           -> bdNew diff
        "still-fail"    -> bdStillFail diff
        "still-succeed" -> bdStillSucceed diff
        "unfinished"    -> bdUnfinished diff
        "aborted"       -> bdAborted diff
        _               -> []
  in  buildListTable bp builds

-- | Render a build list table (used in eval tabs).
buildListTable :: Text -> [Build] -> Html ()
buildListTable bp builds = table_ $ do
  thead_ $ tr_ $ do
    th_ ""; th_ "Job"; th_ "System"; th_ "Nix name"
  tbody_ $ mapM_ renderBuildRow builds
  where
    renderBuildRow b = tr_ $ do
      td_ $ statusIcon (buildStatus b)
      td_ $ a_ [href_ (buildURL bp (buildId b))] $ toHtml (buildJob b)
      td_ $ toHtml (buildSystem b)
      td_ $ toHtml (fromMaybe "" (buildNixName b))

-- | Render an eval input row.
renderInput :: JobsetEvalInput -> Html ()
renderInput i = tr_ $ do
  td_ $ toHtml (jeiName i)
  td_ $ toHtml (jeiType i)
  td_ $ toHtml (fromMaybe "" (jeiUri i))
  td_ $ code_ $ toHtml (maybe "" shortRev (jeiRevision i))

-- | Render the latest evals page (GET /evals).
latestEvalsPage :: Text -> [EvalInfo] -> Int -> Int -> Int -> Html ()
latestEvalsPage bp evals total page perPage = do
  h1_ "Latest Evaluations"
  table_ $ do
    thead_ $ tr_ $ do
      th_ "#"; th_ "Project"; th_ "Jobset"; th_ "Time"
      th_ [class_ "num"] "Succeeded"
      th_ [class_ "num"] "Failed"
    tbody_ $ mapM_ renderLatestEval evals
  pager total page perPage
  where
    renderLatestEval ei = do
      let eval = eiEval ei
      tr_ $ do
        td_ $ a_ [href_ (evalURL bp (evalId eval))] $ toHtml (showT (evalId eval))
        td_ $ a_ [href_ (projectURL bp (evalProject eval))] $ toHtml (evalProject eval)
        td_ $ a_ [href_ (jobsetURL bp (evalProject eval) (evalJobset eval))]
          $ toHtml (evalJobset eval)
        td_ $ toHtml (showT (evalTimestamp eval))
        td_ [class_ "num status-succeeded"] $ toHtml (showT (eiNrSucceeded ei))
        td_ [class_ "num status-failed"] $ toHtml (showT (eiNrFailed ei))
