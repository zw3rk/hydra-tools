-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Page layout wrapper: HTML head, navigation bar, and footer.
-- All pages use 'pageLayout' to get a consistent shell. The nav bar
-- includes live badge counts for queue, machines, bridges, and evaluating.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Layout
  ( PageData (..)
  , pageLayout
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import HydraWeb.Models.Queue (NavCounts (..))

-- | Data passed to every page render: title, nav counts, page body.
data PageData = PageData
  { pdTitle    :: !Text       -- ^ Page title (appears in <title> tag)
  , pdBasePath :: !Text       -- ^ URL prefix for reverse proxy sub-path
  , pdCounts   :: !NavCounts  -- ^ Queue/running/bridge/evaluating badge counts
  }

-- | Render a full HTML page with navigation, main content, and footer.
-- The 'content' argument is the page-specific body rendered by each handler.
pageLayout :: PageData -> Html () -> Html ()
pageLayout pd content = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    meta_ [name_ "color-scheme", content_ "light dark"]
    title_ $ toHtml (pdTitle pd <> " — Hydra CI")
    link_ [rel_ "stylesheet", href_ (url pd "/static/style.css")]
    script_ [src_ (url pd "/static/htmx.min.js"), defer_ ""] ("" :: Text)
    script_ [src_ (url pd "/static/sse.js"), defer_ ""] ("" :: Text)

  body_ $ do
    nav_ [class_ "site-nav"] $ do
      div_ [class_ "nav-brand"] $
        a_ [href_ (url pd "/")] $ strong_ "Hydra CI"
      ul_ [class_ "nav-links"] $ do
        li_ $ a_ [href_ (url pd "/queue")] $ do
          toHtml ("Queue" :: Text)
          navBadge (ncQueued $ pdCounts pd) ""
        li_ $ a_ [href_ (url pd "/machines")] $ do
          toHtml ("Machines" :: Text)
          navBadge (ncRunning $ pdCounts pd) ""
        li_ $ a_ [href_ (url pd "/running-evals")] $ do
          toHtml ("Evaluating" :: Text)
          navBadge (ncRunningEvals $ pdCounts pd) "pulse"
        li_ $ a_ [href_ (url pd "/bridges")] $ do
          toHtml ("Bridges" :: Text)
          navBadge (ncBridgePending $ pdCounts pd) ""
        li_ $ a_ [href_ (url pd "/evals")] "Evals"
        li_ $ a_ [href_ (url pd "/steps")] "Steps"
        li_ $ a_ [href_ (url pd "/search")] "Search"
        li_ $ a_ [href_ (url pd "/login"), class_ "nav-signin"] "Sign in"

    main_ [class_ "container"] content

    footer_ [class_ "site-footer"] $ small_ $ do
      "Powered by "
      a_ [href_ "https://github.com/zw3rk/hydra-tools"] "hydra-web"

-- | Render a navigation badge pill. Only shown when count > 0.
navBadge :: Int -> Text -> Html ()
navBadge count' cls
  | count' <= 0 = pure ()
  | otherwise   = span_ [class_ ("nav-badge" <> if Text.null cls then "" else " " <> cls)] $
                    toHtml (showT count')

-- | Prepend the base path to a relative URL.
url :: PageData -> Text -> Text
url pd path = pdBasePath pd <> path

-- | Show an Int as Text.
showT :: Int -> Text
showT = Text.pack . show
