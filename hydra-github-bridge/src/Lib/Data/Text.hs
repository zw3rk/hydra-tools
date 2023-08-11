{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Lib.Data.Text where

import           Data.Text (Text)
import qualified Data.Text as Text

indent :: Text -> Text
indent = Text.unlines . (map indentLine) . Text.lines

indentLine :: Text -> Text
indentLine = ("    " <>)

renderLimited :: Int -> (Int -> (Text, Bool)) -> Maybe Text
renderLimited limit render =
    findInputSize 0 >>= Just . fst . render
    where
        -- XXX can we use this to do a binary search instead of a linear search?
        findInputSize current =
            let
                (rendered, done) = render current
            in
                if Text.length rendered > limit
                then
                    if current == 0
                    then Nothing
                    else Just $ current - 1
                else
                    if done
                    then Just current
                    else findInputSize $ current + 1
