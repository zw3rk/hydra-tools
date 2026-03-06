-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Tests for pure formatting and URL-building functions in View.Components.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.ComponentsSpec (spec) where

import Test.Hspec

import HydraWeb.View.Components

spec :: Spec
spec = do
  describe "statusClass" $ do
    it "maps Nothing to queued" $
      statusClass Nothing `shouldBe` "queued"
    it "maps 0 to success" $
      statusClass (Just 0) `shouldBe` "success"
    it "maps 1 to failed" $
      statusClass (Just 1) `shouldBe` "failed"
    it "maps 2 to depfail" $
      statusClass (Just 2) `shouldBe` "depfail"
    it "maps 3 to aborted" $
      statusClass (Just 3) `shouldBe` "aborted"
    it "maps 4 to cancelled" $
      statusClass (Just 4) `shouldBe` "cancelled"
    it "maps 6 to failed" $
      statusClass (Just 6) `shouldBe` "failed"
    it "maps 7 to timedout" $
      statusClass (Just 7) `shouldBe` "timedout"
    it "maps unknown codes to unknown" $
      statusClass (Just 99) `shouldBe` "unknown"

  describe "statusText" $ do
    it "maps Nothing to Queued" $
      statusText Nothing `shouldBe` "Queued"
    it "maps 0 to Succeeded" $
      statusText (Just 0) `shouldBe` "Succeeded"
    it "maps 1 to Failed" $
      statusText (Just 1) `shouldBe` "Failed"
    it "maps 2 to Dependency failed" $
      statusText (Just 2) `shouldBe` "Dependency failed"
    it "maps 7 to Timed out" $
      statusText (Just 7) `shouldBe` "Timed out"
    it "maps 10 to Log limit exceeded" $
      statusText (Just 10) `shouldBe` "Log limit exceeded"
    it "maps 12 to Non-deterministic" $
      statusText (Just 12) `shouldBe` "Non-deterministic"
    it "maps unknown to Unknown" $
      statusText (Just 99) `shouldBe` "Unknown"

  describe "fmtDuration" $ do
    it "formats zero seconds" $
      fmtDuration 0 `shouldBe` "0s"
    it "formats seconds under a minute" $
      fmtDuration 45 `shouldBe` "45s"
    it "formats exact minutes" $
      fmtDuration 120 `shouldBe` "2m 0s"
    it "formats minutes and seconds" $
      fmtDuration 125 `shouldBe` "2m 5s"
    it "formats exact hours" $
      fmtDuration 7200 `shouldBe` "2h 0m"
    it "formats hours and minutes" $
      fmtDuration 7230 `shouldBe` "2h 0m"
    it "formats hours with nonzero minutes" $
      fmtDuration 5400 `shouldBe` "1h 30m"

  describe "timeAgo" $ do
    it "shows seconds for < 60" $
      timeAgo 1000 970 `shouldBe` "30s ago"
    it "shows minutes for < 3600" $
      timeAgo 1000 400 `shouldBe` "10m ago"
    it "shows hours for < 86400" $
      timeAgo 100000 64000 `shouldBe` "10h ago"
    it "shows days for >= 86400" $
      timeAgo 200000 0 `shouldBe` "2d ago"

  describe "humanBytes" $ do
    it "shows bytes" $
      humanBytes 512 `shouldBe` "512 B"
    it "shows KiB" $
      humanBytes 2048 `shouldBe` "2 KiB"
    it "shows MiB" $
      humanBytes (5 * 1048576) `shouldBe` "5 MiB"
    it "shows GiB" $
      humanBytes (3 * 1073741824) `shouldBe` "3 GiB"

  describe "shortDrv" $ do
    it "shortens a nix store derivation path" $
      shortDrv "/nix/store/abcdefghijklmnop-hello-2.12.drv"
        `shouldBe` "abcdefgh-hello-2.12.drv"

  describe "shortRev" $ do
    it "takes first 12 characters" $
      shortRev "a1b2c3d4e5f6g7h8" `shouldBe` "a1b2c3d4e5f6"
    it "handles short inputs" $
      shortRev "abc" `shouldBe` "abc"

  describe "truncateText" $ do
    it "leaves short text unchanged" $
      truncateText 20 "hello" `shouldBe` "hello"
    it "truncates and adds ellipsis" $
      truncateText 8 "hello world!" `shouldBe` "hello..."

  describe "fmtTime" $ do
    it "formats epoch 0 as 1970-01-01 00:00:00" $
      fmtTime 0 `shouldBe` "1970-01-01 00:00:00"

  describe "URL builders" $ do
    it "builds project URL" $
      projectURL "/ci" "myproj" `shouldBe` "/ci/projects/myproj"
    it "builds project URL without base path" $
      projectURL "" "myproj" `shouldBe` "/projects/myproj"
    it "builds jobset URL" $
      jobsetURL "/ci" "proj" "main" `shouldBe` "/ci/projects/proj/jobsets/main"
    it "builds build URL" $
      buildURL "" 42 `shouldBe` "/build/42"
    it "builds eval URL" $
      evalURL "/ci" 100 `shouldBe` "/ci/eval/100"
    it "builds orgRepo URL" $
      orgRepoURL "" "input-output-hk" "cardano-node"
        `shouldBe` "/input-output-hk/cardano-node"

  describe "parsePRNumber" $ do
    it "parses valid pullrequest- prefix" $
      parsePRNumber "pullrequest-6470" `shouldBe` Just 6470
    it "returns Nothing for non-PR names" $
      parsePRNumber "main" `shouldBe` Nothing
    it "returns Nothing for bare prefix" $
      parsePRNumber "pullrequest-" `shouldBe` Nothing
    it "returns Nothing for non-numeric suffix" $
      parsePRNumber "pullrequest-abc" `shouldBe` Nothing
    it "parses PR number 1" $
      parsePRNumber "pullrequest-1" `shouldBe` Just 1

  describe "showT" $ do
    it "converts positive integers" $
      showT 42 `shouldBe` "42"
    it "converts zero" $
      showT 0 `shouldBe` "0"
    it "converts negative integers" $
      showT (-1) `shouldBe` "-1"
