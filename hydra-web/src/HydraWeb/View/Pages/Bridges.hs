-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Bridge status page and reusable content partial.
-- The content partial is also used by the SSE broadcaster to push
-- live updates without a full page reload.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Bridges
  ( bridgesPage
  , bridgesContent
  , renderBridgesContentBS
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Lucid

import HydraWeb.Models.Bridge
import HydraWeb.View.Components (fmtTime, showT, shortPath)
import HydraWeb.View.HTMX (hxExt_, hxSwap_, sseConnect_, sseSwap_)

-- | Full bridges page with SSE live-update wrapper.
bridgesPage :: Text -> BridgeStatus -> Html ()
bridgesPage bp status = do
  h1_ "Bridge Status"
  div_ [hxExt_ "sse", sseConnect_ (bp <> "/stream/bridges")] $
    div_ [id_ "bridge-content", sseSwap_ "bridge-update", hxSwap_ "innerHTML"] $
      bridgesContent status

-- | Reusable content partial for both initial render and SSE updates.
-- Uses client-side tabs to switch between GitHub and Attic sections.
bridgesContent :: BridgeStatus -> Html ()
bridgesContent status = do
  -- Tab buttons with pending counts in labels.
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

  -- GitHub tab panel (shown by default).
  div_ [id_ "tab-github"] $
    case bsGitHub status of
      Nothing -> p_ "GitHub bridge tables not found. Bridge may not be deployed."
      Just gh -> renderGitHub gh

  -- Attic tab panel (hidden by default).
  div_ [id_ "tab-attic", style_ "display:none"] $
    case bsAttic status of
      Nothing -> p_ "Attic bridge table not found. Bridge may not be deployed."
      Just attic -> renderAttic attic

-- | Render the bridges content partial to a strict ByteString.
-- Used by the SSE broadcaster.
renderBridgesContentBS :: BridgeStatus -> ByteString
renderBridgesContentBS = LBS.toStrict . renderBS . bridgesContent

-- ── GitHub rendering ─────────────────────────────────────────────────

renderGitHub :: GitHubBridgeStatus -> Html ()
renderGitHub gh = do
  p_ $ do
    "Total: "
    strong_ (toHtml $ showT (ghsTotal gh))
    " | Sent: "
    strong_ (toHtml $ showT (ghsSent gh))
    " | Pending: "
    strong_ (toHtml $ showT (ghsPending gh))
    " | Failed: "
    strong_ (toHtml $ showT (ghsFailed gh))

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

-- ── Attic rendering ──────────────────────────────────────────────────

renderAttic :: AtticBridgeStatus -> Html ()
renderAttic attic = do
  p_ $ do
    "Total: "
    strong_ (toHtml $ showT (absTotal attic))
    " | Pending: "
    strong_ (toHtml $ showT (absPending attic))
    " | Waiting: "
    strong_ (toHtml $ showT (absWaiting attic))
    " | Failed: "
    strong_ (toHtml $ showT (absFailed attic))

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

renderQueueItem :: AtticQueueItem -> Html ()
renderQueueItem item = tr_ $ do
  td_ (toHtml $ showT (aqiId item))
  td_ [title_ (aqiDrvPath item)] (toHtml $ shortPath (aqiDrvPath item))
  td_ (toHtml $ fmtTime (aqiLast item))
  td_ (toHtml $ showT (aqiTries item))
