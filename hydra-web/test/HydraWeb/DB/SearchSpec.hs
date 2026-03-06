-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Tests for search query validation and ILIKE escaping in DB.Search.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.DB.SearchSpec (spec) where

import Test.Hspec

import HydraWeb.DB.Search (isValidQuery, escapeLike)

spec :: Spec
spec = do
  describe "isValidQuery" $ do
    it "accepts alphanumeric queries" $
      isValidQuery "hello" `shouldBe` True
    it "accepts underscores and hyphens" $
      isValidQuery "nix-store_path" `shouldBe` True
    it "accepts slashes and dots" $
      isValidQuery "/nix/store/abc.drv" `shouldBe` True
    it "accepts spaces" $
      isValidQuery "hello world" `shouldBe` True
    it "accepts colons" $
      isValidQuery "project:jobset" `shouldBe` True
    it "rejects empty string" $
      isValidQuery "" `shouldBe` False
    it "rejects whitespace-only" $
      isValidQuery "   " `shouldBe` False
    it "rejects special characters" $
      isValidQuery "foo;bar" `shouldBe` False
    it "rejects SQL metacharacters" $
      isValidQuery "foo'bar" `shouldBe` False
    it "rejects percent" $
      isValidQuery "foo%bar" `shouldBe` False

  describe "escapeLike" $ do
    it "passes through normal text" $
      escapeLike "hello" `shouldBe` "hello"
    it "escapes percent" $
      escapeLike "100%" `shouldBe` "100\\%"
    it "escapes underscore" $
      escapeLike "foo_bar" `shouldBe` "foo\\_bar"
    it "escapes backslash" $
      escapeLike "a\\b" `shouldBe` "a\\\\b"
    it "escapes multiple metacharacters" $
      escapeLike "%_\\" `shouldBe` "\\%\\_\\\\"
