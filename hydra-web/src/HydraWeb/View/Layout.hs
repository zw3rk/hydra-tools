-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Page layout wrapper: HTML head, navigation bar, and footer.
-- All pages use 'pageLayout' to get a consistent shell. The nav bar
-- includes live badge counts for queue, machines, bridges, and evaluating,
-- and shows the logged-in user or a "Sign in" link.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Layout
  ( PageData (..)
  , pageLayout
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import HydraWeb.Models.Queue (NavCounts (..))
import HydraWeb.Models.User (GFUser (..))
import HydraWeb.View.Components (showT)

-- | Data passed to every page render: title, nav counts, optional user.
data PageData = PageData
  { pdTitle    :: !Text             -- ^ Page title (appears in <title> tag)
  , pdBasePath :: !Text             -- ^ URL prefix for reverse proxy sub-path
  , pdCounts   :: !NavCounts        -- ^ Queue/running/bridge/evaluating badge counts
  , pdUser     :: !(Maybe GFUser)   -- ^ Authenticated user (Nothing = not logged in)
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
    script_ [src_ (url pd "/static/tabs.js"), defer_ ""] ("" :: Text)

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
        -- Show user info or sign-in link.
        case pdUser pd of
          Just user -> do
            li_ $ a_ [href_ (url pd "/profile"), class_ "nav-user"] $
              toHtml (gfuGitHubLogin user)
            li_ $ a_ [href_ (url pd "/logout"), class_ "nav-signin"] "Sign out"
          Nothing ->
            li_ $ a_ [href_ (url pd "/auth/github"), class_ "nav-signin"] "Sign in"

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
