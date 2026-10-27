-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Text (Text)
import Lib.Bridge.PublicUrl (hydraDetailsUrl, normalizePublicBase)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $ fail (label <> ": expected " <> show expected <> ", got " <> show actual)

assertInvalid :: Text -> IO ()
assertInvalid value =
  case normalizePublicBase value of
    Left _ -> pure ()
    Right normalized -> fail ("expected invalid public URL, got " <> show normalized)

main :: IO ()
main = do
  assertEqual
    "explicit public HTTPS URL"
    (Right "https://hydra.example.com")
    (normalizePublicBase "https://hydra.example.com")
  assertEqual
    "loopback HTTP URL"
    (Right "http://127.0.0.1:3000")
    (normalizePublicBase "http://127.0.0.1:3000")
  assertEqual
    "trailing slash and path prefix"
    (Right "https://hydra.example.com/hydra/jobset/project/jobset")
    (hydraDetailsUrl <$> normalizePublicBase "https://hydra.example.com/hydra///" <*> pure "/jobset/project/jobset")
  assertEqual
    "legacy bare host"
    (Right "https://hydra.example.com:8080")
    (normalizePublicBase "hydra.example.com:8080")
  assertInvalid ""
  assertInvalid "ftp://hydra.example.com"
  assertInvalid "https://http://127.0.0.1:3000"
  assertInvalid "https:///missing-host"
  assertInvalid "http://hydra.example.com:bad-port"
  assertInvalid "https://hydra.example.com/path?query=1"
  assertInvalid "https://hydra.example.com/path#fragment"
  putStrLn "All public URL regressions passed"
