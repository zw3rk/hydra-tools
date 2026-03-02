-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | AES-256-GCM encryption for at-rest token storage.
-- When encryption is disabled (empty key), tokens are stored in plaintext.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Auth.Encrypt
  ( Encryptor
  , newEncryptor
  , encrypt
  , decrypt
  ) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
  (cipherInit, AuthTag (..), AEADMode (..),
   AEAD, aeadInit, aeadSimpleEncrypt, aeadSimpleDecrypt)
import Crypto.Error (CryptoFailable (..))
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- | Opaque encryption handle wrapping the AES-256 cipher.
newtype Encryptor = Encryptor AES256

-- | Create an encryptor from a 32-byte hex-encoded key.
-- Returns Nothing if the key is empty (encryption disabled) or invalid.
newEncryptor :: Text -> Maybe Encryptor
newEncryptor keyHex
  | keyHex == mempty = Nothing
  | otherwise =
    let keyBytes = hexDecode (TE.encodeUtf8 keyHex)
    in case cipherInit keyBytes of
         CryptoPassed cipher -> Just (Encryptor cipher)
         CryptoFailed _      -> Nothing

-- | Encrypt plaintext using AES-256-GCM. Prepends 12-byte nonce.
-- Returns: nonce (12) || ciphertext || tag (16).
encrypt :: Encryptor -> ByteString -> IO ByteString
encrypt (Encryptor cipher) plaintext = do
  nonce <- getRandomBytes 12 :: IO ByteString
  case aeadInit AEAD_GCM cipher nonce of
    CryptoFailed _   -> error "AES-GCM init failed"
    CryptoPassed aead -> do
      let (tag, out) = aeadSimpleEncrypt (aead :: AEAD AES256) BS.empty plaintext 16
      pure $ nonce <> out <> convert tag

-- | Decrypt AES-256-GCM ciphertext. Expects: nonce (12) || ciphertext || tag (16).
decrypt :: Encryptor -> ByteString -> Maybe ByteString
decrypt (Encryptor cipher) combined
  | BS.length combined < 28 = Nothing  -- 12 nonce + 16 tag minimum
  | otherwise =
    let nonce = BS.take 12 combined
        rest  = BS.drop 12 combined
        ct    = BS.take (BS.length rest - 16) rest
        tag   = AuthTag (convert (BS.drop (BS.length rest - 16) rest))
    in case aeadInit AEAD_GCM cipher nonce of
         CryptoFailed _ -> Nothing
         CryptoPassed aead ->
           aeadSimpleDecrypt (aead :: AEAD AES256) BS.empty ct tag

-- | Decode hex-encoded bytes.
hexDecode :: ByteString -> ByteString
hexDecode = BS.pack . go . BS.unpack
  where
    go []       = []
    go [_]      = []
    go (a:b:xs) = (hexNibble a * 16 + hexNibble b) : go xs
    hexNibble w
      | w >= 0x30 && w <= 0x39 = w - 0x30
      | w >= 0x41 && w <= 0x46 = w - 0x41 + 10
      | w >= 0x61 && w <= 0x66 = w - 0x61 + 10
      | otherwise              = 0
