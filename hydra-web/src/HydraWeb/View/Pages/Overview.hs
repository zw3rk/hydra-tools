-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Overview page HTML (project listing + news).
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Overview
  ( overviewPage
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Lucid

import HydraWeb.Models.Project (Project (..))
import HydraWeb.Models.Queue (NewsItem (..))
import HydraWeb.View.Components (projectURL, fmtTime)

-- | Render the overview page content.
overviewPage :: Text -> [Project] -> [NewsItem] -> Html ()
overviewPage bp projects news = do
  h1_ "Projects"

  -- News section (if any items).
  case news of
    [] -> pure ()
    _  -> details_ [open_ ""] $ do
      summary_ "News"
      mapM_ (renderNewsItem) news

  -- Projects table.
  table_ $ do
    thead_ $ tr_ $ do
      th_ "Project"
      th_ "Description"
    tbody_ $
      mapM_ (renderProject bp) projects

-- | Render a single project table row.
renderProject :: Text -> Project -> Html ()
renderProject bp p = do
  let cls = if projEnabled p == 0 then [class_ "disabled"] else []
  tr_ cls $ do
    td_ $ do
      a_ [href_ (projectURL bp (projName p))] $ toHtml (projDisplayName p)
      case projHomepage p of
        Just hp -> do
          " "
          a_ [href_ hp, class_ "small external", target_ "_blank"] "\x2197"
        Nothing -> pure ()
    td_ $ toHtml (fromMaybe "" (projDescription p))

-- | Render a news item.
renderNewsItem :: NewsItem -> Html ()
renderNewsItem ni = article_ $ do
  small_ $ do
    toHtml (fmtTime (niCreateTime ni))
    " \x2014 "
    toHtml (niAuthor ni)
  p_ $ toHtml (niContents ni)
