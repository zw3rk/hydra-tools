-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Shared database utility functions.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.DB.Util
  ( escapeLike
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Escape SQL LIKE/ILIKE metacharacters so user input is treated literally.
escapeLike :: Text -> Text
escapeLike = Text.concatMap esc
  where
    esc '%'  = "\\%"
    esc '_'  = "\\_"
    esc '\\' = "\\\\"
    esc c    = Text.singleton c
