-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Pagination component for use in Lucid views.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pager
  ( pager
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

-- | Render pagination controls if there are more items than perPage.
pager :: Int -> Int -> Int -> Html ()
pager total page perPage
  | total <= perPage = pure ()
  | otherwise = nav_ [class_ "pager"] $ do
      if page > 1
        then a_ [href_ ("?page=" <> showT (page - 1))] "\xAB Previous"
        else pure ()
      span_ $ toHtml ("Page " <> showT page)
      if page * perPage < total
        then a_ [href_ ("?page=" <> showT (page + 1))] "Next \xBB"
        else pure ()

showT :: Int -> Text
showT = Text.pack . show
