-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Tests for cookie parsing and session extraction helpers.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Auth.MiddlewareSpec (spec) where

import Test.Hspec

import HydraWeb.Auth.Middleware (extractSessionId)

spec :: Spec
spec = do
  describe "extractSessionId" $ do
    it "extracts session from simple cookie" $
      extractSessionId "hydra_session=abc123"
        `shouldBe` Just "abc123"

    it "extracts session from multiple cookies" $
      extractSessionId "foo=bar; hydra_session=abc123; baz=qux"
        `shouldBe` Just "abc123"

    it "returns Nothing when session cookie is absent" $
      extractSessionId "foo=bar; baz=qux"
        `shouldBe` Nothing

    it "returns Nothing for empty cookie string" $
      extractSessionId "" `shouldBe` Nothing

    it "handles cookies with spaces around separators" $
      extractSessionId "hydra_session=abc123 ; other=val"
        `shouldBe` Just "abc123"

    it "handles long hex session IDs" $
      let sid = "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2"
      in  extractSessionId ("hydra_session=" <> sid)
            `shouldBe` Just sid
