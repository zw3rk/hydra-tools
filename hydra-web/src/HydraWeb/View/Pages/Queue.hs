-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Queue, queue-summary, machines, and steps page HTML.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Queue
  ( queuePage
  , queueSummaryPage
  , machinesPage
  , stepsPage
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import HydraWeb.Models.Build (Build (..), BuildStep (..), stepStatusText)
import HydraWeb.Models.Queue (QueueSummary (..), SystemQueueRow (..), ActiveStep (..))
import HydraWeb.View.Components
import HydraWeb.View.HTMX (hxGet_, hxTrigger_, hxSwap_, hxTarget_)
import HydraWeb.View.Pager (pager)

-- | Render the full queue list page.
queuePage :: Text -> [Build] -> Int -> Html ()
queuePage bp builds total = do
  h1_ $ toHtml ("Build Queue (" <> showT total <> ")")
  p_ $ a_ [href_ (bp <> "/queue-summary")] "View queue summary"

  -- Auto-refresh wrapper.
  div_ [hxGet_ (bp <> "/queue"), hxTrigger_ "every 30s",
        hxSwap_ "innerHTML", hxTarget_ "#queue-table"] $ do
    table_ [id_ "queue-table"] $ do
      thead_ $ tr_ $ do
        th_ "ID"; th_ "Job"; th_ "System"; th_ "Priority"; th_ "Queued"
      tbody_ $ mapM_ renderQueuedBuild builds
  where
    renderQueuedBuild b = tr_ $ do
      td_ $ a_ [href_ (buildURL bp (buildId b))] $ toHtml ("#" <> showT (buildId b))
      td_ $ do
        a_ [href_ (projectURL bp (buildProject b))] $ toHtml (buildProject b)
        ":"
        a_ [href_ (jobsetURL bp (buildProject b) (buildJobset b))] $ toHtml (buildJobset b)
        ":"
        toHtml (buildJob b)
      td_ $ toHtml (buildSystem b)
      td_ $ toHtml (showT (buildGlobalPriority b))
      td_ $ toHtml (showT (buildTimestamp b))

-- | Render the queue summary page (by jobset and by system).
queueSummaryPage :: Text -> [QueueSummary] -> [SystemQueueRow] -> Int -> Html ()
queueSummaryPage bp summary systems total = do
  h1_ $ toHtml ("Queue Summary (" <> showT total <> " queued)")

  h2_ "By Jobset"
  table_ $ do
    thead_ $ tr_ $ do
      th_ "Jobset"; th_ [class_ "num"] "Queued"
      th_ "Oldest"; th_ "Newest"
    tbody_ $ mapM_ renderSummaryRow summary

  h2_ "By System"
  table_ $ do
    thead_ $ tr_ $ th_ "System" >> th_ [class_ "num"] "Count"
    tbody_ $ mapM_ (\s -> tr_ $ do
      td_ $ toHtml (sqSystem s)
      td_ [class_ "num"] $ toHtml (showT (sqCount s))
      ) systems
  where
    renderSummaryRow s = tr_ $ do
      td_ $ a_ [href_ (jobsetURL bp (qsProject s) (qsJobset s))]
        $ toHtml (qsProject s <> ":" <> qsJobset s)
      td_ [class_ "num"] $ toHtml (showT (qsQueued s))
      td_ $ toHtml (fmtTime (qsOldest s))
      td_ $ toHtml (fmtTime (qsNewest s))

-- | Render the machines/active steps page.
machinesPage :: Text -> [ActiveStep] -> Html ()
machinesPage bp steps = do
  h1_ "Machine Status"
  case steps of
    [] -> p_ "No active build steps."
    _  -> table_ $ do
      thead_ $ tr_ $ do
        th_ "Machine"; th_ "Build"; th_ "Step"; th_ "System"
        th_ "Job"; th_ "Running"
      tbody_ $ mapM_ renderActiveStep steps
  where
    renderActiveStep s = tr_ $ do
      td_ $ toHtml (stripSSH (asMachine s))
      td_ $ a_ [href_ (buildURL bp (asBuild s))] $ toHtml ("#" <> showT (asBuild s))
      td_ $ toHtml (showT (asStepNr s))
      td_ $ toHtml (asSystem s)
      td_ $ do
        a_ [href_ (projectURL bp (asProject s))] $ toHtml (asProject s)
        ":"
        a_ [href_ (jobsetURL bp (asProject s) (asJobset s))] $ toHtml (asJobset s)
        ":"
        toHtml (asJob s)
      td_ $ toHtml (maybe "" (fmtTime) (asStartTime s))

    stripSSH t = case Text.stripPrefix "ssh://" t of
      Just rest -> rest
      Nothing   -> t

-- | Render the recent build steps page.
stepsPage :: Text -> [BuildStep] -> Int -> Int -> Html ()
stepsPage bp steps page perPage = do
  h1_ "Latest Build Steps"
  table_ $ do
    thead_ $ tr_ $ do
      th_ "Build"; th_ "#"; th_ ""; th_ "Machine"
      th_ "System"; th_ "Duration"; th_ "Finished"
    tbody_ $ mapM_ renderRecentStep steps

  -- Simple prev/next pagination.
  if page > 1
    then a_ [href_ ("?page=" <> showT (page - 1))] "\xAB Previous"
    else pure ()
  " "
  a_ [href_ ("?page=" <> showT (page + 1))] "Next \xBB"
  where
    renderRecentStep s = tr_ $ do
      td_ $ a_ [href_ (buildURL bp (stepBuild s))] $ toHtml ("#" <> showT (stepBuild s))
      td_ $ toHtml (showT (stepNr s))
      td_ [class_ (statusClass (stepStatus s))] $ toHtml (stepStatusText s)
      td_ $ toHtml (stripSSH (stepMachine s))
      td_ $ toHtml (fromMaybe "" (stepSystem s))
      td_ $ case (stepStartTime s, stepStopTime s) of
        (Just start, Just stop) -> toHtml (fmtDuration (stop - start))
        _ -> pure ()
      td_ $ case stepStopTime s of
        Just t  -> toHtml (fmtTime t)
        Nothing -> pure ()

    stripSSH t = case Text.stripPrefix "ssh://" t of
      Just rest -> rest
      Nothing   -> t
