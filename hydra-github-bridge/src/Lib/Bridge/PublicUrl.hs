-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Bridge.PublicUrl
  ( normalizePublicBase,
    hydraDetailsUrl,
  )
where

import Data.Char (isControl, isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI (URI (..), URIAuth (..), parseURI)
import Text.Read (readMaybe)

-- | Normalize a public Hydra base URL. Legacy bare hosts retain HTTPS, while
-- explicit HTTP or HTTPS schemes and path prefixes are preserved.
normalizePublicBase :: Text -> Either Text Text
normalizePublicBase raw
  | Text.null raw = Left "URL is empty"
  | Text.any (\c -> isControl c || isSpace c) raw = Left "URL contains whitespace or control characters"
  | Text.isInfixOf "://" lowerRaw && not (hasHttpScheme lowerRaw) = Left "scheme must be http or https"
  | malformedHttpScheme lowerRaw = Left "malformed HTTP URL scheme"
  | otherwise = validate candidate
  where
    lowerRaw = Text.toLower raw
    explicitHttp = hasHttpScheme lowerRaw
    candidate = if explicitHttp then raw else "https://" <> raw

    validate value =
      if hasHttpScheme (Text.toLower $ afterScheme value)
        then Left "URL must contain only one scheme"
        else case parseURI (Text.unpack value) of
          Just uri
            | validAuthority (uriScheme uri) (uriAuthority uri)
                && null (uriQuery uri)
                && null (uriFragment uri) ->
                Right (Text.dropWhileEnd (== '/') value)
          _ -> Left "expected an absolute HTTP(S) URL with a host and no query or fragment"

    afterScheme value
      | "https://" `Text.isPrefixOf` Text.toLower value = Text.drop 8 value
      | otherwise = Text.drop 7 value

    validAuthority scheme (Just auth) =
      Text.toLower (Text.pack scheme) `elem` ["http:", "https:"]
        && not (null (uriRegName auth))
        && null (uriUserInfo auth)
        && validPort (uriPort auth)
    validAuthority _ Nothing = False

    validPort "" = True
    validPort (':' : digits) =
      case readMaybe digits :: Maybe Int of
        Just port -> port >= 0 && port <= 65535
        Nothing -> False
    validPort _ = False

hasHttpScheme :: Text -> Bool
hasHttpScheme value = "http://" `Text.isPrefixOf` value || "https://" `Text.isPrefixOf` value

malformedHttpScheme :: Text -> Bool
malformedHttpScheme value =
  any (\prefix -> prefix `Text.isPrefixOf` value && not (Text.isPrefixOf (prefix <> "//") value)) ["http:", "https:"]

-- | Join an absolute path to a normalized public base without duplicating
-- the boundary slash. The base path prefix remains part of the result.
hydraDetailsUrl :: Text -> Text -> Text
hydraDetailsUrl base path =
  Text.dropWhileEnd (== '/') base <> "/" <> Text.dropWhile (== '/') path
