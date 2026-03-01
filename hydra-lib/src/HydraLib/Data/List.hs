-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | List utility functions.
module HydraLib.Data.List
  ( takeEnd
  ) where

-- | Take the last @n@ elements from a list.
takeEnd :: Int -> [a] -> [a]
takeEnd n xs = drop (length xs - n) xs
