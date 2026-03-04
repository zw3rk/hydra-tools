-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | HTMX attribute helpers for use with Lucid.
-- Provides type-safe wrappers for common hx-* attributes so they compose
-- naturally with Lucid's HTML combinator style.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.HTMX
  ( hxGet_
  , hxPost_
  , hxTarget_
  , hxSwap_
  , hxSwapOob_
  , hxTrigger_
  , hxPushUrl_
  , hxIndicator_
  , hxVals_
  , hxBoost_
  , hxExt_
  , sseConnect_
  , sseSwap_
  , sseClose_
  ) where

import Data.Text (Text)
import Lucid.Base (Attribute, makeAttribute)

-- | hx-get="url" — issue a GET request to the given URL.
hxGet_ :: Text -> Attribute
hxGet_ = makeAttribute "hx-get"

-- | hx-post="url" — issue a POST request to the given URL.
hxPost_ :: Text -> Attribute
hxPost_ = makeAttribute "hx-post"

-- | hx-target="selector" — target element for swapping.
hxTarget_ :: Text -> Attribute
hxTarget_ = makeAttribute "hx-target"

-- | hx-swap="strategy" — how to swap content (innerHTML, outerHTML, etc.).
hxSwap_ :: Text -> Attribute
hxSwap_ = makeAttribute "hx-swap"

-- | hx-trigger="event" — what triggers the request.
hxTrigger_ :: Text -> Attribute
hxTrigger_ = makeAttribute "hx-trigger"

-- | hx-push-url="true|url" — push URL to browser history.
hxPushUrl_ :: Text -> Attribute
hxPushUrl_ = makeAttribute "hx-push-url"

-- | hx-indicator="selector" — element to show during request.
hxIndicator_ :: Text -> Attribute
hxIndicator_ = makeAttribute "hx-indicator"

-- | hx-vals='json' — additional values to include in request.
hxVals_ :: Text -> Attribute
hxVals_ = makeAttribute "hx-vals"

-- | hx-boost="true" — boost regular links/forms with AJAX.
hxBoost_ :: Text -> Attribute
hxBoost_ = makeAttribute "hx-boost"

-- | hx-ext="name" — HTMX extension to use.
hxExt_ :: Text -> Attribute
hxExt_ = makeAttribute "hx-ext"

-- | sse-connect="url" — SSE extension: connect to event source.
sseConnect_ :: Text -> Attribute
sseConnect_ = makeAttribute "sse-connect"

-- | sse-swap="event" — SSE extension: swap content on named event.
sseSwap_ :: Text -> Attribute
sseSwap_ = makeAttribute "sse-swap"

-- | sse-close="event" — SSE extension: close connection on named event.
sseClose_ :: Text -> Attribute
sseClose_ = makeAttribute "sse-close"

-- | hx-swap-oob="true|selector" — out-of-band swap (update element by ID outside target).
hxSwapOob_ :: Text -> Attribute
hxSwapOob_ = makeAttribute "hx-swap-oob"
