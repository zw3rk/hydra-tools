-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Project page HTML: jobset listing with progress bars and breadcrumbs.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Project
  ( projectPage
  ) where

import Data.Text (Text)
import Lucid

import HydraWeb.Models.Project (Project (..), Jobset (..), hasErrorMsg)
import HydraWeb.View.Components (jobsetURL, progressBar, breadcrumb, showT, renderJobsetName)

-- | Render the project page content with breadcrumb and jobset cards.
-- The @mOrgRepo@ parameter enables PR link rendering when a GitHub org/repo
-- mapping is available for the project.
projectPage :: Text -> Maybe (Text, Text) -> Project -> [Jobset] -> Html ()
projectPage bp mOrgRepo project jobsets = do
  -- Breadcrumb: Home / Project
  breadcrumb [("Projects", bp <> "/"), (projDisplayName project, "")]

  hgroup_ $ do
    h1_ $ toHtml (projDisplayName project)
    case projDescription project of
      Just d  -> p_ $ toHtml d
      Nothing -> pure ()

  h2_ "Jobsets"
  table_ $ do
    thead_ $ tr_ $ do
      th_ "Jobset"
      th_ "Type"
      th_ "Progress"
      th_ [class_ "num"] "Succeeded"
      th_ [class_ "num"] "Failed"
      th_ [class_ "num"] "Queued"
      th_ [class_ "num"] "Total"
      th_ "Status"
    tbody_ $
      mapM_ (renderJobset bp mOrgRepo) jobsets

-- | Render a single jobset table row with progress bar.
-- PR jobsets show a GitHub PR link; descriptions are shown alongside the name.
renderJobset :: Text -> Maybe (Text, Text) -> Jobset -> Html ()
renderJobset bp mOrgRepo js = do
  let cls | jsHidden js == 1 = [class_ "hidden-jobset"]
          | jsEnabled js == 0 = [class_ "disabled"]
          | otherwise = []
  tr_ cls $ do
    td_ $ do
      a_ [href_ (jobsetURL bp (jsProject js) (jsName js))] $
        renderJobsetName mOrgRepo (jsName js)
      case jsDescription js of
        Just d  -> span_ [class_ "jobset-desc"] $ toHtml (" \x2014 " <> d)
        Nothing -> pure ()
    td_ $ if jsType js == 1 then "flake" else "legacy"
    td_ $ progressBar (jsNrSucceeded js) (jsNrFailed js) (jsNrScheduled js)
    td_ [class_ "num status-succeeded"] $ toHtml (showT (jsNrSucceeded js))
    td_ [class_ "num status-failed"] $ toHtml (showT (jsNrFailed js))
    td_ [class_ "num status-queued"] $ toHtml (showT (jsNrScheduled js))
    td_ [class_ "num"] $ toHtml (showT (jsNrTotal js))
    td_ $ do
      if hasErrorMsg js
        then span_ [class_ "status-failed", title_ "Has errors"] "\x26A0"
        else pure ()
      if jsEnabled js == 0
        then span_ [class_ "disabled-badge"] "disabled"
        else pure ()
