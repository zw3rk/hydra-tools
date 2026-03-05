-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Build page HTML: status hero card, timeline steps, outputs, products.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Build
  ( buildPage
  , buildLogPage
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import HydraWeb.Models.Build
import HydraWeb.View.Components

-- | Render the build detail page with status hero and timeline.
-- When @showActions@ is True, a "Restart Build" button is shown for finished builds.
buildPage :: Text -> Build -> [BuildStep] -> [BuildOutput] -> [BuildProduct]
          -> [BuildMetric] -> [BuildInput] -> [Int] -> [Build] -> Bool -> Html ()
buildPage bp build steps outputs products metrics inputs evalIDs constits showActions = do
  -- Breadcrumb: Home / Project / Jobset / Build #N
  breadcrumb [ ("Projects", bp <> "/")
             , (buildProject build, projectURL bp (buildProject build))
             , (buildJobset build, jobsetURL bp (buildProject build) (buildJobset build))
             , ("Build #" <> showT (buildId build), "")
             ]

  -- Status hero card.
  div_ [class_ ("build-hero " <> statusClass (buildStatus build))] $ do
    h1_ $ do
      statusIcon (buildStatus build)
      toHtml (" Build #" <> showT (buildId build))
    p_ $ do
      toHtml (buildJob build)
      " \x2014 "
      toHtml (buildSystem build)

  -- Restart button for finished builds (authenticated users only).
  if showActions && buildFinished build == 1
    then form_ [ method_ "POST"
               , action_ (bp <> "/build/" <> showT (buildId build) <> "/restart")
               ] $
      button_ [type_ "submit", class_ "btn outline"] "Restart Build"
    else pure ()

  -- Build summary.
  dl_ [class_ "build-summary"] $ do
    dt_ "Status"
    dd_ [class_ (statusClass (buildStatus build))] $
      toHtml (buildStatusText build)

    dt_ "System"
    dd_ $ toHtml (buildSystem build)

    maybeDt "Nix name" (buildNixName build)
    maybeDt "Release" (buildReleaseName build)

    case (buildStartTime build, buildStopTime build) of
      (Just start, Just stop) -> do
        dt_ "Duration"
        dd_ $ toHtml (fmtDuration (stop - start))
        dt_ "Started"
        dd_ $ toHtml (fmtTime start)
        dt_ "Finished"
        dd_ $ toHtml (fmtTime stop)
      _ -> pure ()

    dt_ "Queued"
    dd_ $ toHtml (fmtTime (buildTimestamp build))
    dt_ "Derivation"
    dd_ $ code_ $ toHtml (buildDrvPath build)

    case buildSize build of
      Just sz -> do
        dt_ "Output size"
        dd_ $ toHtml (humanBytes sz)
      Nothing -> pure ()
    case buildClosureSize build of
      Just sz -> do
        dt_ "Closure size"
        dd_ $ toHtml (humanBytes sz)
      Nothing -> pure ()

  -- Outputs.
  renderSection "Outputs" outputs $ \outs ->
    table_ $ do
      thead_ $ tr_ $ th_ "Name" >> th_ "Store path"
      tbody_ $ mapM_ (\o -> tr_ $ do
        td_ $ toHtml (boName o)
        td_ $ code_ $ toHtml (fromMaybe "" (boPath o))
        ) outs

  -- Build products.
  renderSection "Build Products" products $ \prods ->
    table_ $ do
      thead_ $ tr_ $ th_ "Name" >> th_ "Type" >> th_ "Size"
      tbody_ $ mapM_ (\p -> tr_ $ do
        td_ $ toHtml (bpName p)
        td_ $ do
          toHtml (bpType p)
          if Text.null (bpSubtype p) then pure () else do
            "/"
            toHtml (bpSubtype p)
        td_ $ toHtml (maybe "" (humanBytes) (bpFileSize p))
        ) prods

  -- Build steps (timeline style).
  renderSection "Build Steps" steps $ \ss ->
    div_ [class_ "timeline"] $
      mapM_ (renderTimelineStep bp (buildId build)) ss

  -- Build inputs.
  renderSection "Inputs" inputs $ \is ->
    table_ $ do
      thead_ $ tr_ $ do
        th_ "Name"; th_ "Type"; th_ "Value/URI"; th_ "Revision"
      tbody_ $ mapM_ (\i -> tr_ $ do
        td_ $ toHtml (biName i)
        td_ $ toHtml (biType i)
        td_ $ toHtml (fromMaybe "" (biUri i) <> fromMaybe "" (biValue i))
        td_ $ code_ $ toHtml (maybe "" shortRev (biRevision i))
        ) is

  -- Constituents (for aggregate builds).
  renderSection "Constituents" constits $ \cs ->
    table_ $ do
      thead_ $ tr_ $ do
        th_ ""; th_ "Build"; th_ "Job"; th_ "System"; th_ "Status"
      tbody_ $ mapM_ (\c -> tr_ $ do
        td_ $ statusIcon (buildStatus c)
        td_ $ a_ [href_ (buildURL bp (buildId c))] $ toHtml ("#" <> showT (buildId c))
        td_ $ toHtml (buildJob c)
        td_ $ toHtml (buildSystem c)
        td_ [class_ (statusClass (buildStatus c))] $ toHtml (statusText (buildStatus c))
        ) cs

  -- Evaluations list.
  case evalIDs of
    [] -> pure ()
    _  -> details_ $ do
      summary_ $ toHtml ("Part of " <> showT (length evalIDs) <> " evaluation(s)")
      ul_ $ mapM_ (\eid -> li_ $
        a_ [href_ (evalURL bp eid)] $ toHtml ("#" <> showT eid)
        ) evalIDs

  -- Metrics.
  renderSection' "Build Metrics" metrics $ \ms ->
    details_ $ do
      summary_ "Build Metrics"
      table_ $ do
        thead_ $ tr_ $ th_ "Name" >> th_ "Value" >> th_ "Unit"
        tbody_ $ mapM_ (\m -> tr_ $ do
          td_ $ toHtml (bmName m)
          td_ $ toHtml (show (bmValue m))
          td_ $ toHtml (fromMaybe "" (bmUnit m))
          ) ms

-- | Render a build step as a timeline item with optional log link.
renderTimelineStep :: Text -> Int -> BuildStep -> Html ()
renderTimelineStep bp bid s = do
  let cls = case stepStatus s of
        Just 0  -> "success"
        Just 1  -> "failed"
        Nothing -> "running"
        _       -> "failed"
  div_ [class_ ("timeline-item " <> cls)] $ do
    div_ $ do
      strong_ $ toHtml ("#" <> showT (stepNr s))
      " "
      toHtml (stepStatusText s)
      " on "
      toHtml (stripSSH (stepMachine s))
      -- Show log link for completed steps that have a derivation path.
      case (stepStatus s, stepDrvPath s) of
        (Just _, Just _) -> do
          " "
          a_ [href_ (bp <> "/build/" <> showT bid <> "/nixlog/" <> showT (stepNr s))
             , class_ "log-link"
             ] "log"
        _ -> pure ()
    div_ $ do
      case (stepStartTime s, stepStopTime s) of
        (Just start, Just stop) -> toHtml (fmtDuration (stop - start))
        _ -> pure ()
      " "
      code_ $ toHtml (maybe "" shortDrv (stepDrvPath s))

-- | Conditionally render a section if the list is non-empty.
renderSection :: Text -> [a] -> ([a] -> Html ()) -> Html ()
renderSection title items renderer = case items of
  [] -> pure ()
  _  -> do
    h2_ $ toHtml title
    renderer items

-- | Same as renderSection but wraps in details (for metrics).
renderSection' :: Text -> [a] -> ([a] -> Html ()) -> Html ()
renderSection' _ [] _        = pure ()
renderSection' _ items renderer = renderer items

-- | Maybe render a dt/dd pair.
maybeDt :: Text -> Maybe Text -> Html ()
maybeDt _ Nothing  = pure ()
maybeDt label (Just v) = do
  dt_ $ toHtml label
  dd_ $ toHtml v

-- | Render the build log page with breadcrumb and log output.
buildLogPage :: Text -> Int -> Int -> Text -> Text -> Html ()
buildLogPage bp bid sNr drv logText = do
  breadcrumb [ ("Projects", bp <> "/")
             , ("Build #" <> showT bid, bp <> "/build/" <> showT bid)
             , ("Step #" <> showT sNr <> " log", "")
             ]
  h1_ $ toHtml ("Build #" <> showT bid <> " \x2014 step " <> showT sNr <> " log")
  p_ $ do
    "Derivation: "
    code_ $ toHtml drv
  pre_ [class_ "build-log"] $ toHtml logText
