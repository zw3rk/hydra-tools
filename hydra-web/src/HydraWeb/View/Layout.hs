-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Page layout wrapper: HTML head, navigation bar, and footer.
-- All pages use 'pageLayout' to get a consistent shell. The nav bar
-- includes live badge counts for queue, machines, and bridges.
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
  , pdCounts   :: !NavCounts  -- ^ Queue/running/bridge badge counts
  }

-- | Render a full HTML page with navigation, main content, and footer.
-- The 'content' argument is the page-specific body rendered by each handler.
pageLayout :: PageData -> Html () -> Html ()
pageLayout pd content = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ $ toHtml (pdTitle pd <> " — Hydra CI")
    link_ [rel_ "stylesheet", href_ (url pd "/static/style.css")]
    script_ [src_ (url pd "/static/htmx.min.js"), defer_ ""] ("" :: Text)

  body_ $ do
    nav_ $ do
      ul_ $ do
        li_ $ a_ [href_ (url pd "/")] $ strong_ "Hydra CI"
      ul_ $ do
        li_ $ a_ [href_ (url pd "/queue")] $
          toHtml ("Queue (" <> showT (ncQueued $ pdCounts pd) <> ")")
        li_ $ a_ [href_ (url pd "/machines")] $
          toHtml ("Machines (" <> showT (ncRunning $ pdCounts pd) <> ")")
        li_ $ a_ [href_ (url pd "/bridges")] $ do
          toHtml ("Bridges" :: Text)
          let bp = ncBridgePending (pdCounts pd)
          if bp > 0
            then toHtml (" (" <> showT bp <> ")")
            else pure ()
        li_ $ a_ [href_ (url pd "/evals")] "Evals"
        li_ $ a_ [href_ (url pd "/steps")] "Steps"
        li_ $ a_ [href_ (url pd "/search")] "Search"
        -- TODO: user/auth links once Phase 4 is implemented
        li_ $ a_ [href_ (url pd "/login"), role_ "button", class_ "outline"] "Sign in"

    main_ content

    footer_ $ small_ $ do
      "Powered by "
      a_ [href_ "https://github.com/zw3rk/hydra-tools"] "hydra-web"

-- | Prepend the base path to a relative URL.
url :: PageData -> Text -> Text
url pd path = pdBasePath pd <> path

-- | Show an Int as Text.
showT :: Int -> Text
showT = Text.pack . show
