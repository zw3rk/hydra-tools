-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Property-based and unit tests for AES-256-GCM encryption.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Auth.EncryptSpec (spec) where

import Data.Bits (xor)
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString as BS

import HydraWeb.Auth.Encrypt

-- | A valid 32-byte AES key, hex-encoded (64 hex chars).
testKeyHex :: Text
testKeyHex = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

-- | A different valid key for cross-key tests.
altKeyHex :: Text
altKeyHex = "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"

-- | Extract an encryptor from a hex key, failing the test if invalid.
requireEncryptor :: Text -> IO Encryptor
requireEncryptor key = case newEncryptor key of
  Just enc -> pure enc
  Nothing  -> fail "Expected Just Encryptor, got Nothing"

spec :: Spec
spec = do
  describe "newEncryptor" $ do
    it "returns Nothing for empty key" $
      isNothing (newEncryptor "") `shouldBe` True

    it "returns Just for valid 64-char hex key" $
      isJust (newEncryptor testKeyHex) `shouldBe` True

    it "returns Nothing for too-short key" $
      isNothing (newEncryptor "0123") `shouldBe` True

  describe "encrypt / decrypt round-trip" $ do
    it "decrypts what was encrypted" $ do
      enc <- requireEncryptor testKeyHex
      ciphertext <- encrypt enc "hello world"
      decrypt enc ciphertext `shouldBe` Just "hello world"

    it "round-trips empty plaintext" $ do
      enc <- requireEncryptor testKeyHex
      ct <- encrypt enc ""
      decrypt enc ct `shouldBe` Just ""

    it "round-trips arbitrary bytestrings" $ property $ \bs -> ioProperty $ do
      enc <- requireEncryptor testKeyHex
      let plaintext = BS.pack bs
      ct <- encrypt enc plaintext
      pure $ decrypt enc ct === Just plaintext

    it "fails to decrypt with wrong key" $ do
      enc1 <- requireEncryptor testKeyHex
      enc2 <- requireEncryptor altKeyHex
      ct <- encrypt enc1 "secret data"
      decrypt enc2 ct `shouldBe` Nothing

    it "fails to decrypt truncated ciphertext" $ do
      enc <- requireEncryptor testKeyHex
      ct <- encrypt enc "test"
      decrypt enc (BS.take 20 ct) `shouldBe` Nothing

    it "fails to decrypt corrupted ciphertext" $ do
      enc <- requireEncryptor testKeyHex
      ct <- encrypt enc "test"
      -- Flip a bit in the middle of the ciphertext.
      let idx = BS.length ct `div` 2
          corrupted = BS.take idx ct
                   <> BS.singleton (BS.index ct idx `xor` 1)
                   <> BS.drop (idx + 1) ct
      decrypt enc corrupted `shouldBe` Nothing
  where
    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing  = False
    isNothing :: Maybe a -> Bool
    isNothing       = not . isJust
