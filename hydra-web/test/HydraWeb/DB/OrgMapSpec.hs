-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Tests for pure parsing functions in DB.OrgMap.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.DB.OrgMapSpec (spec) where

import Test.Hspec

import HydraWeb.DB.OrgMap (parseProjectName, parseGitHubFlake)

spec :: Spec
spec = do
  describe "parseGitHubFlake" $ do
    it "parses simple github:org/repo" $
      parseGitHubFlake "github:zw3rk/hydra-tools"
        `shouldBe` Just ("zw3rk", "hydra-tools")
    it "parses github:org/repo/ref" $
      parseGitHubFlake "github:input-output-hk/cardano-node/master"
        `shouldBe` Just ("input-output-hk", "cardano-node")
    it "parses github:org/repo?ref=..." $
      parseGitHubFlake "github:zw3rk/hello?ref=main"
        `shouldBe` Just ("zw3rk", "hello")
    it "parses github:org/repo/ref?dir=..." $
      parseGitHubFlake "github:org/repo/branch?dir=subdir"
        `shouldBe` Just ("org", "repo")
    it "returns Nothing for non-github flakes" $
      parseGitHubFlake "git+https://example.com/repo" `shouldBe` Nothing
    it "returns Nothing for empty github: prefix" $
      parseGitHubFlake "github:" `shouldBe` Nothing
    it "returns Nothing for org-only" $
      parseGitHubFlake "github:org" `shouldBe` Nothing

  describe "parseProjectName" $ do
    it "matches known multi-dash org prefix" $
      parseProjectName ["input-output-hk"] "input-output-hk-devx"
        `shouldBe` Just ("input-output-hk", "devx")
    it "falls back to first-dash split for unknown orgs" $
      parseProjectName [] "zw3rk-hydra-tools"
        `shouldBe` Just ("zw3rk", "hydra-tools")
    it "prefers known org over fallback" $
      parseProjectName ["zw3rk"] "zw3rk-hydra-tools"
        `shouldBe` Just ("zw3rk", "hydra-tools")
    it "returns Nothing for single segment (no dash)" $
      parseProjectName [] "standalone" `shouldBe` Nothing
    it "handles empty project name" $
      parseProjectName [] "" `shouldBe` Nothing
    it "handles org prefix without repo" $
      parseProjectName ["org"] "org-" `shouldBe` Nothing
