-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Reusable Lucid components: tables, status badges, pagination, formatting.
-- These are the building blocks for all page views.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Components
  ( -- * Status rendering
    statusIcon
  , statusClass
  , statusText

    -- * Formatting
  , fmtTime
  , fmtDuration
  , timeAgo
  , humanBytes
  , shortDrv
  , shortPath
  , shortRev
  , truncateText
  , showT

    -- * URL builders
  , projectURL
  , jobsetURL
  , buildURL
  , evalURL
  , orgRepoURL

    -- * New visual components
  , statusDot
  , progressBar
  , metricCard
  , breadcrumb
  , cardWithHeader
  , sseTarget
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Lucid
import qualified Lucid.Base

-- | Status icon character for a build status code.
statusIcon :: Maybe Int -> Html ()
statusIcon Nothing  = span_ [class_ "status-icon queued"]  "\x23F3"
statusIcon (Just 0) = span_ [class_ "status-icon success"] "\x2713"
statusIcon (Just 1) = span_ [class_ "status-icon failed"]  "\x2717"
statusIcon (Just 2) = span_ [class_ "status-icon depfail"] "\x26A0"
statusIcon (Just 3) = span_ [class_ "status-icon aborted"] "\x2298"
statusIcon (Just 4) = span_ [class_ "status-icon cancelled"] "\x2298"
statusIcon (Just 6) = span_ [class_ "status-icon failed"]  "\x2717"
statusIcon (Just 7) = span_ [class_ "status-icon timedout"] "\x23F1"
statusIcon _        = span_ [class_ "status-icon unknown"] "?"

-- | CSS class for a build status code.
statusClass :: Maybe Int -> Text
statusClass Nothing  = "queued"
statusClass (Just 0) = "success"
statusClass (Just 1) = "failed"
statusClass (Just 2) = "depfail"
statusClass (Just 3) = "aborted"
statusClass (Just 4) = "cancelled"
statusClass (Just 6) = "failed"
statusClass (Just 7) = "timedout"
statusClass _        = "unknown"

-- | Human-readable status label.
statusText :: Maybe Int -> Text
statusText Nothing   = "Queued"
statusText (Just 0)  = "Succeeded"
statusText (Just 1)  = "Failed"
statusText (Just 2)  = "Dependency failed"
statusText (Just 3)  = "Aborted"
statusText (Just 4)  = "Cancelled"
statusText (Just 6)  = "Failed with output"
statusText (Just 7)  = "Timed out"
statusText (Just 10) = "Log limit exceeded"
statusText (Just 11) = "Output size exceeded"
statusText (Just 12) = "Non-deterministic"
statusText _         = "Unknown"

-- | Format a UNIX timestamp as "YYYY-MM-DD HH:MM:SS".
fmtTime :: Int -> Text
fmtTime ts =
  Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $
    posixSecondsToUTCTime (fromIntegral ts)

-- | Format a duration in seconds as a human-readable string.
fmtDuration :: Int -> Text
fmtDuration secs
  | secs < 60    = showT secs <> "s"
  | secs < 3600  = showT (secs `div` 60) <> "m " <> showT (secs `mod` 60) <> "s"
  | otherwise     = showT (secs `div` 3600) <> "h " <>
                    showT ((secs `mod` 3600) `div` 60) <> "m"

-- | Format a timestamp as relative "X ago" text.
timeAgo :: Int -> Int -> Text
timeAgo now ts
  | diff < 60    = showT diff <> "s ago"
  | diff < 3600  = showT (diff `div` 60) <> "m ago"
  | diff < 86400 = showT (diff `div` 3600) <> "h ago"
  | otherwise     = showT (diff `div` 86400) <> "d ago"
  where diff = now - ts

-- | Format bytes as human-readable size.
humanBytes :: Int -> Text
humanBytes n
  | n < 1024       = showT n <> " B"
  | n < 1048576    = showT (n `div` 1024) <> " KiB"
  | n < 1073741824 = showT (n `div` 1048576) <> " MiB"
  | otherwise       = showT (n `div` 1073741824) <> " GiB"

-- | Shorten a derivation path to just the hash prefix + name.
shortDrv :: Text -> Text
shortDrv p = case Text.breakOn "-" (Text.takeWhileEnd (/= '/') p) of
  (hash, rest) -> Text.take 8 hash <> rest

-- | Shorten a store path to hash prefix + name.
shortPath :: Text -> Text
shortPath = shortDrv

-- | Shorten a revision hash to first 12 characters.
shortRev :: Text -> Text
shortRev = Text.take 12

-- | Truncate text to a maximum length, appending "..." if truncated.
truncateText :: Int -> Text -> Text
truncateText n t
  | Text.length t <= n = t
  | otherwise = Text.take (n - 3) t <> "..."

-- | Show an Int as Text.
showT :: Int -> Text
showT = Text.pack . show

-- | Build a project URL (REST-style: /projects/:name).
projectURL :: Text -> Text -> Text
projectURL basePath name = basePath <> "/projects/" <> name

-- | Build a jobset URL (REST-style: /projects/:p/jobsets/:j).
jobsetURL :: Text -> Text -> Text -> Text
jobsetURL basePath project jobset = basePath <> "/projects/" <> project <> "/jobsets/" <> jobset

-- | Build a build detail URL (flat: /build/:id).
buildURL :: Text -> Int -> Text
buildURL basePath bid = basePath <> "/build/" <> showT bid

-- | Build an eval URL (flat: /eval/:id).
evalURL :: Text -> Int -> Text
evalURL basePath eid = basePath <> "/eval/" <> showT eid

-- | Build an org/repo URL (/:org/:repo).
orgRepoURL :: Text -> Text -> Text -> Text
orgRepoURL basePath org repo = basePath <> "/" <> org <> "/" <> repo

-- | Small colored dot indicator for build status.
statusDot :: Maybe Int -> Html ()
statusDot st = span_ [class_ ("status-dot " <> statusClass st)] ""

-- | Stacked horizontal progress bar showing build status distribution.
-- Takes (succeeded, failed, queued) counts and renders proportional segments.
progressBar :: Int -> Int -> Int -> Html ()
progressBar succ' fail' queued' =
  let total = succ' + fail' + queued'
  in  if total == 0
        then div_ [class_ "progress-bar"] $ pure ()
        else div_ [class_ "progress-bar"] $ do
          when (succ' > 0) $
            div_ [class_ "progress-segment success"
                 , style_ ("width:" <> pct succ' total)] ""
          when (fail' > 0) $
            div_ [class_ "progress-segment failed"
                 , style_ ("width:" <> pct fail' total)] ""
          when (queued' > 0) $
            div_ [class_ "progress-segment queued"
                 , style_ ("width:" <> pct queued' total)] ""
  where
    pct n t = showT (n * 100 `div` t) <> "%"
    when True  f = f
    when False _ = pure ()

-- | A metric card showing a count and label (for dashboard).
metricCard :: Text -> Text -> Int -> Html ()
metricCard cls label count' =
  div_ [class_ ("metric-card " <> cls)] $ do
    div_ [class_ "metric-value"] $ toHtml (showT count')
    div_ [class_ "metric-label"] $ toHtml label

-- | Render a breadcrumb navigation trail from (label, url) pairs.
-- The last item is rendered without a link (current page).
breadcrumb :: [(Text, Text)] -> Html ()
breadcrumb items = nav_ [class_ "breadcrumb"] $
  ul_ $ mapM_ renderItem (zip [0 :: Int ..] items)
  where
    lastIdx = length items - 1
    renderItem :: (Int, (Text, Text)) -> Html ()
    renderItem (idx, (label, url'))
      | idx == lastIdx = li_ [class_ "current"] $ toHtml label
      | otherwise      = li_ $ a_ [href_ url'] $ toHtml label

-- | A card with a styled header.
cardWithHeader :: Text -> Html () -> Html ()
cardWithHeader title content = article_ [class_ "card"] $ do
  header_ $ h3_ $ toHtml title
  content

-- | SSE live-update wrapper div.
sseTarget :: Text -> Text -> Text -> Html () -> Html ()
sseTarget bp streamPath eventName content =
  div_ [hxExt_ "sse", sseConnect_ (bp <> streamPath)] $
    div_ [id_ (eventName <> "-content"), sseSwap_ eventName, hxSwap_ "innerHTML"] content
  where
    hxExt_ = makeAttribute "hx-ext"
    sseConnect_ = makeAttribute "sse-connect"
    sseSwap_ = makeAttribute "sse-swap"
    hxSwap_ = makeAttribute "hx-swap"
    makeAttribute k v = Lucid.Base.makeAttribute k v
