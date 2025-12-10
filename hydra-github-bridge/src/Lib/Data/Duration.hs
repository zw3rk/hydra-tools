{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib.Data.Duration where

import Data.Duration as Duration
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text

humanReadableDuration :: Duration.Seconds -> Text
humanReadableDuration 0 = "0s"
humanReadableDuration s = Text.strip . cs . Duration.humanReadableDuration $ s -- seems to always append whitespace
