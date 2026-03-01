-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Human-readable duration formatting.
{-# LANGUAGE OverloadedStrings #-}

module HydraLib.Data.Duration
  ( humanReadableDuration
  ) where

import qualified Data.Duration as Duration
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Format a duration in seconds as a human-readable string (e.g. "2h 3m 5s").
humanReadableDuration :: Duration.Seconds -> Text
humanReadableDuration 0 = "0s"
humanReadableDuration s = Text.strip . cs . Duration.humanReadableDuration $ s
