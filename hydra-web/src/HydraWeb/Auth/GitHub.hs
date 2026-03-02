-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | GitHub OAuth 2.0 flow: authorization URL, code exchange, user fetch.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Auth.GitHub
  ( GitHubUser (..)
  , generateOAuthState
  , authorizeURL
  , exchangeCode
  , fetchGitHubUser
  ) where

import Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON (..), Value (..), decode, withObject, (.:), (.:?))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Network.HTTP.Client
  (Manager, Request (..), parseRequest, httpLbs, responseBody,
   responseStatus, urlEncodedBody)
import Network.HTTP.Types (statusCode)
import Numeric (showHex)

-- | User info returned by GitHub's /user endpoint.
data GitHubUser = GitHubUser
  { ghuId        :: !Int
  , ghuLogin     :: !Text
  , ghuName      :: !(Maybe Text)
  , ghuEmail     :: !(Maybe Text)
  , ghuAvatarURL :: !(Maybe Text)
  } deriving (Show)

instance FromJSON GitHubUser where
  parseJSON = withObject "GitHubUser" $ \v -> GitHubUser
    <$> v .:  "id"
    <*> v .:  "login"
    <*> v .:? "name"
    <*> v .:? "email"
    <*> v .:? "avatar_url"

-- | Generate a 128-bit CSRF state token for OAuth, hex-encoded.
generateOAuthState :: IO Text
generateOAuthState = do
  bytes <- getRandomBytes 16 :: IO ByteString
  pure $ Text.pack $ concatMap showHex' $ BS.unpack bytes
  where
    showHex' b = let s = showHex b "" in if length s == 1 then '0':s else s

-- | Build the GitHub OAuth authorization URL.
authorizeURL :: Text -> Text -> Text -> Text
authorizeURL clientId callbackURL state =
  "https://github.com/login/oauth/authorize"
  <> "?client_id=" <> clientId
  <> "&redirect_uri=" <> callbackURL
  <> "&state=" <> state
  <> "&scope=read:user,read:org"

-- | Exchange an authorization code for an access token.
exchangeCode :: Manager -> Text -> Text -> Text -> IO (Maybe Text)
exchangeCode mgr clientId clientSecret code = do
  initReq <- parseRequest "https://github.com/login/oauth/access_token"
  let req = urlEncodedBody
        [ ("client_id", TE.encodeUtf8 clientId)
        , ("client_secret", TE.encodeUtf8 clientSecret)
        , ("code", TE.encodeUtf8 code)
        ]
        initReq
          { requestHeaders = [("Accept", "application/json")]
          }
  resp <- httpLbs req mgr
  if statusCode (responseStatus resp) /= 200
    then pure Nothing
    else pure $ parseAccessToken (responseBody resp)

-- | Parse access_token from GitHub's JSON response.
parseAccessToken :: LBS.ByteString -> Maybe Text
parseAccessToken body = do
  val <- decode body
  case val of
    Object o -> case KM.lookup (K.fromText "access_token") o of
      Just (String t) -> Just t
      _               -> Nothing
    _ -> Nothing

-- | Fetch the authenticated GitHub user using an access token.
fetchGitHubUser :: Manager -> Text -> IO (Maybe GitHubUser)
fetchGitHubUser mgr token = do
  initReq <- parseRequest "https://api.github.com/user"
  let req = initReq
        { requestHeaders =
            [ ("Authorization", "Bearer " <> TE.encodeUtf8 token)
            , ("Accept", "application/json")
            , ("User-Agent", "hydra-web")
            ]
        }
  resp <- httpLbs req mgr
  if statusCode (responseStatus resp) /= 200
    then pure Nothing
    else pure $ decode (responseBody resp)
