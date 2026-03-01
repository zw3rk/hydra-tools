-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Text utility functions.
{-# LANGUAGE OverloadedStrings #-}

module HydraLib.Data.Text
  ( indent
  , indentLine
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Indent all lines in a text block by 4 spaces.
indent :: Text -> Text
indent = Text.unlines . map indentLine . Text.lines

-- | Prepend 4 spaces to a single line.
indentLine :: Text -> Text
indentLine = ("    " <>)
