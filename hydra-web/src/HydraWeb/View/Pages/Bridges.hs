-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Bridge status page with client-side tabs.
-- Tab buttons and panel wrappers live outside the SSE zone so they are
-- never replaced.  Each tab panel has its own SSE target that only
-- replaces the data inside the panel, preserving tab state.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Bridges
  ( bridgesPage
  , renderGitHubDataBS
  , renderAtticDataBS
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Lucid

import HydraWeb.Models.Bridge
import HydraWeb.View.Components (fmtTime, showT, shortPath)
import HydraWeb.View.HTMX (hxExt_, hxSwap_, sseConnect_, sseSwap_)

-- | Full bridges page.  Tab buttons and panel structure are rendered
-- here (never replaced by SSE).  Only the data divs inside each panel
-- are swapped live.
bridgesPage :: Text -> BridgeStatus -> Html ()
bridgesPage bp status = do
  h1_ "Bridge Status"

  -- Tab buttons — outside the SSE zone, never replaced.
  div_ [role_ "tablist"] $ do
    let ghLabel = "GitHub" <> ghBadge
        ghBadge = case bsGitHub status of
          Just gh | ghsPending gh > 0 -> " (" <> showT (ghsPending gh) <> ")"
          _ -> ""
        atLabel = "Attic" <> atBadge
        atBadge = case bsAttic status of
          Just at | absPending at > 0 -> " (" <> showT (absPending at) <> ")"
          _ -> ""
    button_ [ role_ "tab", id_ "tab-github-btn", class_ "active"
            , onclick_ "document.getElementById('tab-github').style.display='';document.getElementById('tab-attic').style.display='none';document.getElementById('tab-github-btn').className='active';document.getElementById('tab-attic-btn').className='';"
            ] (toHtml ghLabel)
    button_ [ role_ "tab", id_ "tab-attic-btn"
            , onclick_ "document.getElementById('tab-attic').style.display='';document.getElementById('tab-github').style.display='none';document.getElementById('tab-attic-btn').className='active';document.getElementById('tab-github-btn').className='';"
            ] (toHtml atLabel)

  -- SSE connection — wraps both panels but only targets the inner data divs.
  div_ [hxExt_ "sse", sseConnect_ (bp <> "/stream/bridges")] $ do
    -- GitHub tab panel (shown by default).
    div_ [id_ "tab-github"] $
      div_ [id_ "github-data", sseSwap_ "github-bridge-update", hxSwap_ "innerHTML"] $
        githubData (bsGitHub status)

    -- Attic tab panel (hidden by default).
    div_ [id_ "tab-attic", style_ "display:none"] $
      div_ [id_ "attic-data", sseSwap_ "attic-bridge-update", hxSwap_ "innerHTML"] $
        atticData (bsAttic status)

-- ── GitHub rendering ─────────────────────────────────────────────────

-- | Render GitHub bridge data (the inner content of the GitHub tab).
githubData :: Maybe GitHubBridgeStatus -> Html ()
githubData Nothing = p_ "GitHub bridge tables not found. Bridge may not be deployed."
githubData (Just gh) = do
  bridgeSummary (ghsTotal gh) (ghsPending gh) (ghsFailed gh)

  if null (ghsByRepo gh)
    then p_ "No unsent notifications."
    else do
      table_ $ do
        thead_ $ tr_ $ do
          th_ "Owner"
          th_ "Repo"
          th_ "Pending"
          th_ "Failed"
        tbody_ $ mapM_ renderRepoRow (ghsByRepo gh)

  if null (ghsRecentSent gh)
    then pure ()
    else do
      h3_ "Recently Sent"
      table_ $ do
        thead_ $ tr_ $ do
          th_ "Owner"
          th_ "Repo"
          th_ "Name"
          th_ "Sent"
        tbody_ $ mapM_ renderRecentRow (ghsRecentSent gh)

-- | Render GitHub data to a strict ByteString for SSE broadcasting.
renderGitHubDataBS :: BridgeStatus -> ByteString
renderGitHubDataBS = LBS.toStrict . renderBS . githubData . bsGitHub

-- ── Attic rendering ──────────────────────────────────────────────────

-- | Render Attic bridge data (the inner content of the Attic tab).
atticData :: Maybe AtticBridgeStatus -> Html ()
atticData Nothing = p_ "Attic bridge table not found. Bridge may not be deployed."
atticData (Just attic) = do
  bridgeSummary (absTotal attic) (absPending attic) (absFailed attic)

  if null (absRecentActive attic)
    then pure ()
    else do
      h3_ "Recent Queue Activity"
      table_ $ do
        thead_ $ tr_ $ do
          th_ "ID"
          th_ "Derivation"
          th_ "Last Attempt"
          th_ "Tries"
        tbody_ $ mapM_ renderQueueItem (absRecentActive attic)

-- | Render Attic data to a strict ByteString for SSE broadcasting.
renderAtticDataBS :: BridgeStatus -> ByteString
renderAtticDataBS = LBS.toStrict . renderBS . atticData . bsAttic

-- ── Shared ───────────────────────────────────────────────────────────

-- | Consistent summary line: Total / Pending / Failed.
bridgeSummary :: Int -> Int -> Int -> Html ()
bridgeSummary total pending failed = p_ $ do
  "Total: "
  strong_ (toHtml $ showT total)
  " | Pending: "
  strong_ (toHtml $ showT pending)
  " | Failed: "
  strong_ (toHtml $ showT failed)

renderRepoRow :: GitHubRepoRow -> Html ()
renderRepoRow r = tr_ $ do
  td_ (toHtml $ grrOwner r)
  td_ (toHtml $ grrRepo r)
  td_ (toHtml $ showT (grrPending r))
  td_ (toHtml $ showT (grrFailed r))

renderRecentRow :: GitHubRecentSend -> Html ()
renderRecentRow r = tr_ $ do
  td_ (toHtml $ grsOwner r)
  td_ (toHtml $ grsRepo r)
  td_ (toHtml $ grsName r)
  td_ (toHtml $ fmtTime (grsSent r))

renderQueueItem :: AtticQueueItem -> Html ()
renderQueueItem item = tr_ $ do
  td_ (toHtml $ showT (aqiId item))
  td_ [title_ (aqiDrvPath item)] (toHtml $ shortPath (aqiDrvPath item))
  td_ (toHtml $ fmtTime (aqiLast item))
  td_ (toHtml $ showT (aqiTries item))
