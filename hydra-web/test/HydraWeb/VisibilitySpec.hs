-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
module HydraWeb.VisibilitySpec (spec) where

import Test.Hspec

import HydraWeb.Visibility (accessDecision, isSuperAdmin, isAuthenticated)
import HydraWeb.Models.User (GFUser (..))

-- | Convenience: a minimal authenticated non-admin user.
regularUser :: Maybe GFUser
regularUser = Just GFUser
  { gfuId = 1, gfuGitHubId = 100, gfuGitHubLogin = "alice"
  , gfuDisplayName = Nothing, gfuEmail = Nothing, gfuAvatarURL = Nothing
  , gfuIsSuperAdmin = False, gfuCreatedAt = 0, gfuUpdatedAt = 0 }

-- | Convenience: a super-admin user.
adminUser :: Maybe GFUser
adminUser = Just GFUser
  { gfuId = 2, gfuGitHubId = 200, gfuGitHubLogin = "admin"
  , gfuDisplayName = Nothing, gfuEmail = Nothing, gfuAvatarURL = Nothing
  , gfuIsSuperAdmin = True, gfuCreatedAt = 0, gfuUpdatedAt = 0 }

spec :: Spec
spec = do
  describe "isSuperAdmin" $ do
    it "returns False for Nothing" $
      isSuperAdmin Nothing `shouldBe` False
    it "returns False for regular user" $
      isSuperAdmin regularUser `shouldBe` False
    it "returns True for super-admin" $
      isSuperAdmin adminUser `shouldBe` True

  describe "isAuthenticated" $ do
    it "returns False for Nothing" $
      isAuthenticated Nothing `shouldBe` False
    it "returns True for any user" $
      isAuthenticated regularUser `shouldBe` True

  describe "accessDecision" $ do
    -- Parameters: isHidden, mRepoPublic, isAuthed, isSuperAdmin'
    -- Super-admin always sees everything.
    describe "super-admin" $ do
      it "sees hidden project" $
        accessDecision True Nothing False True `shouldBe` True
      it "sees hidden + private repo" $
        accessDecision True (Just False) False True `shouldBe` True
      it "sees visible + private repo" $
        accessDecision False (Just False) False True `shouldBe` True
      it "sees visible + public repo" $
        accessDecision False (Just True) False True `shouldBe` True

    -- Hidden project, non-admin.
    describe "hidden project, non-admin" $ do
      it "denies anonymous user" $
        accessDecision True Nothing False False `shouldBe` False
      it "denies authenticated non-admin" $
        accessDecision True Nothing True False `shouldBe` False
      it "denies even with public repo info" $
        accessDecision True (Just True) True False `shouldBe` False

    -- Visible project, no repo mapping (no org/repo pair in DB).
    describe "visible, no repo mapping (fail-open)" $ do
      it "allows anonymous" $
        accessDecision False Nothing False False `shouldBe` True
      it "allows authenticated" $
        accessDecision False Nothing True False `shouldBe` True

    -- Visible project, public repo.
    describe "visible, public repo" $ do
      it "allows anonymous" $
        accessDecision False (Just True) False False `shouldBe` True
      it "allows authenticated" $
        accessDecision False (Just True) True False `shouldBe` True

    -- Visible project, private repo.
    describe "visible, private repo" $ do
      it "denies anonymous" $
        accessDecision False (Just False) False False `shouldBe` False
      it "allows authenticated" $
        accessDecision False (Just False) True False `shouldBe` True
