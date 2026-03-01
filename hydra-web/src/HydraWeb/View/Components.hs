-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Reusable Lucid components: tables, status badges, pagination, formatting.
-- These are the building blocks for all page views.
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
  , truncateText

    -- * URL builders
  , projectURL
  , jobsetURL
  , buildURL
  , evalURL
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Lucid

-- | Status icon character for a build status code.
statusIcon :: Maybe Int -> Html ()
statusIcon Nothing  = span_ [class_ "status-icon queued"]  "⏳"
statusIcon (Just 0) = span_ [class_ "status-icon success"] "✓"
statusIcon (Just 1) = span_ [class_ "status-icon failed"]  "✗"
statusIcon (Just 2) = span_ [class_ "status-icon depfail"] "⚠"
statusIcon (Just 3) = span_ [class_ "status-icon aborted"] "⊘"
statusIcon (Just 4) = span_ [class_ "status-icon cancelled"] "⊘"
statusIcon (Just 6) = span_ [class_ "status-icon failed"]  "✗"
statusIcon (Just 7) = span_ [class_ "status-icon timedout"] "⏱"
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

-- | Truncate text to a maximum length, appending "..." if truncated.
truncateText :: Int -> Text -> Text
truncateText n t
  | Text.length t <= n = t
  | otherwise = Text.take (n - 3) t <> "..."

-- | Build a project URL.
projectURL :: Text -> Text -> Text
projectURL basePath name = basePath <> "/project/" <> name

-- | Build a jobset URL.
jobsetURL :: Text -> Text -> Text -> Text
jobsetURL basePath project jobset = basePath <> "/jobset/" <> project <> "/" <> jobset

-- | Build a build detail URL.
buildURL :: Text -> Int -> Text
buildURL basePath bid = basePath <> "/build/" <> showT bid

-- | Build an eval URL.
evalURL :: Text -> Int -> Text
evalURL basePath eid = basePath <> "/eval/" <> showT eid

showT :: Int -> Text
showT = Text.pack . show
