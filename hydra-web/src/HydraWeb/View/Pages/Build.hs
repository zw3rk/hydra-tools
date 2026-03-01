-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Build page HTML (build info, steps, outputs, products, inputs).
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Build
  ( buildPage
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import HydraWeb.Models.Build
import HydraWeb.View.Components

-- | Render the build detail page content.
buildPage :: Text -> Build -> [BuildStep] -> [BuildOutput] -> [BuildProduct]
          -> [BuildMetric] -> [BuildInput] -> [Int] -> [Build] -> Html ()
buildPage bp build steps outputs products metrics inputs evalIDs constits = do
  -- Header with status icon.
  hgroup_ $ do
    h1_ $ do
      span_ [class_ (statusClass (buildStatus build))] $ statusIcon (buildStatus build)
      toHtml (" Build #" <> showT (buildId build))
    p_ $ do
      a_ [href_ (projectURL bp (buildProject build))] $ toHtml (buildProject build)
      " / "
      a_ [href_ (jobsetURL bp (buildProject build) (buildJobset build))]
        $ toHtml (buildJobset build)
      " / "
      toHtml (buildJob build)

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

  -- Build steps.
  renderSection "Build Steps" steps $ \ss ->
    table_ $ do
      thead_ $ tr_ $ do
        th_ "#"; th_ ""; th_ "Machine"; th_ "Duration"; th_ "Derivation"
      tbody_ $ mapM_ renderStep ss

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

-- | Render a build step row.
renderStep :: BuildStep -> Html ()
renderStep s = tr_ $ do
  td_ $ toHtml (showT (stepNr s))
  td_ [class_ (statusClass (stepStatus s))] $
    toHtml (stepStatusText s)
  td_ $ toHtml (stripSSH (stepMachine s))
  td_ $ case (stepStartTime s, stepStopTime s) of
    (Just start, Just stop) -> toHtml (fmtDuration (stop - start))
    _ -> pure ()
  td_ $ code_ $ toHtml (maybe "" shortDrv (stepDrvPath s))

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

-- | Strip ssh:// prefix from machine names.
stripSSH :: Text -> Text
stripSSH t = case Text.stripPrefix "ssh://" t of
  Just rest -> rest
  Nothing   -> t
