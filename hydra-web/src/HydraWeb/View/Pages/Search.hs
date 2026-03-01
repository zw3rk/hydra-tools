-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Search page HTML (search form + results).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Search
  ( searchPage
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Lucid

import HydraWeb.Models.Project (Project (..), Jobset (..))
import HydraWeb.Models.Build (Build (..))
import HydraWeb.DB.Search (SearchResults (..))
import HydraWeb.View.Components
import HydraWeb.View.HTMX (hxGet_, hxTrigger_, hxTarget_)

-- | Render the search page with form and optional results.
searchPage :: Text -> Text -> Maybe SearchResults -> Html ()
searchPage bp q mResults = do
  h1_ "Search"

  -- Search form with HTMX live search.
  form_ [action_ (bp <> "/search"), method_ "get"] $
    fieldset_ [role_ "group"] $ do
      input_ [type_ "search", name_ "query",
              placeholder_ "Search projects, jobsets, builds...",
              value_ q,
              hxGet_ (bp <> "/search"),
              hxTrigger_ "keyup changed delay:300ms",
              hxTarget_ "#results"]
      button_ [type_ "submit"] "Search"

  -- Results area.
  div_ [id_ "results"] $
    case mResults of
      Nothing -> pure ()
      Just results -> renderResults bp results

-- | Render search results grouped by type.
renderResults :: Text -> SearchResults -> Html ()
renderResults bp sr = do
  -- Projects.
  renderSection "Projects" (srProjects sr) $ \ps -> table_ $ do
    thead_ $ tr_ $ th_ "Name" >> th_ "Description"
    tbody_ $ mapM_ (\p -> tr_ $ do
      td_ $ a_ [href_ (projectURL bp (projName p))] $ toHtml (projDisplayName p)
      td_ $ toHtml (fromMaybe "" (projDescription p))
      ) ps

  -- Jobsets.
  renderSection "Jobsets" (srJobsets sr) $ \js -> table_ $ do
    thead_ $ tr_ $ th_ "Project" >> th_ "Jobset" >> th_ "Description"
    tbody_ $ mapM_ (\j -> tr_ $ do
      td_ $ a_ [href_ (projectURL bp (jsProject j))] $ toHtml (jsProject j)
      td_ $ a_ [href_ (jobsetURL bp (jsProject j) (jsName j))] $ toHtml (jsName j)
      td_ $ toHtml (fromMaybe "" (jsDescription j))
      ) js

  -- Builds by output path.
  renderBuildSection bp "Builds by output path" (srBuilds sr)

  -- Builds by derivation path.
  renderBuildSection bp "Builds by derivation path" (srBuildsDrv sr)

-- | Render a section with a title (only if list is non-empty).
renderSection :: Text -> [a] -> ([a] -> Html ()) -> Html ()
renderSection _ [] _    = pure ()
renderSection t xs body = do
  h2_ $ toHtml (t <> " (" <> showT (length xs) <> ")")
  body xs

-- | Render a build results table.
renderBuildSection :: Text -> Text -> [Build] -> Html ()
renderBuildSection bp title builds = renderSection title builds $ \bs -> table_ $ do
  thead_ $ tr_ $ th_ "" >> th_ "ID" >> th_ "Job" >> th_ "System"
  tbody_ $ mapM_ (\b -> tr_ $ do
    td_ $ statusIcon (buildStatus b)
    td_ $ a_ [href_ (buildURL bp (buildId b))] $ toHtml ("#" <> showT (buildId b))
    td_ $ toHtml (buildProject b <> ":" <> buildJobset b <> ":" <> buildJob b)
    td_ $ toHtml (buildSystem b)
    ) bs
