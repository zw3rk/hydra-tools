-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Tests for configuration parsing functions.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.ConfigSpec (spec) where

import Test.Hspec

import HydraWeb.Config (parseInstallationIDs, parseSuperAdmins)

spec :: Spec
spec = do
  describe "parseInstallationIDs" $ do
    it "parses empty string" $
      parseInstallationIDs "" `shouldBe` []
    it "parses single pair" $
      parseInstallationIDs "input-output-hk=12345"
        `shouldBe` [("input-output-hk", 12345)]
    it "parses multiple pairs" $
      parseInstallationIDs "org1=100,org2=200"
        `shouldBe` [("org1", 100), ("org2", 200)]
    it "strips whitespace" $
      parseInstallationIDs " org1 = 100 , org2 = 200 "
        `shouldBe` [("org1", 100), ("org2", 200)]
    it "skips malformed entries" $
      parseInstallationIDs "good=1,bad,also=2"
        `shouldBe` [("good", 1), ("also", 2)]
    it "skips entries with non-numeric IDs" $
      parseInstallationIDs "org=abc" `shouldBe` []

  describe "parseSuperAdmins" $ do
    it "parses empty string" $
      parseSuperAdmins "" `shouldBe` []
    it "parses single username" $
      parseSuperAdmins "alice" `shouldBe` ["alice"]
    it "parses comma-separated usernames" $
      parseSuperAdmins "alice,bob,carol" `shouldBe` ["alice", "bob", "carol"]
    it "strips whitespace" $
      parseSuperAdmins " alice , bob " `shouldBe` ["alice", "bob"]
    it "filters empty entries" $
      parseSuperAdmins "alice,,bob" `shouldBe` ["alice", "bob"]
