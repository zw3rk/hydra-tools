-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | GitHub REST API client utilities.
-- Provides a monad for GitHub API calls with configurable endpoint URL,
-- and helpers for GitHub App authentication (JWT signing, installation tokens).
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module HydraLib.GitHub.Client
  ( GitHubRestT (..)
  , GitHubRestConfig (..)
  , GitHubRestManager (..)
  , DecodeError (..)
  , RESTKeyValue (..)
  , runGitHubRestT
  , gitHubRestConfig
  , gitHubApiVersion
  , loadSigner
  , fetchInstallations
  , fetchAppInstallationToken
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadTrans)
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (FromJSON, Value)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LByteString
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.X509 (PrivKey (..))
import Data.X509.File (readKeyFile)
import GitHub.REST
  ( GHEndpoint (..)
  , GitHubSettings (..)
  , KeyValue (..)
  , MonadGitHubREST (..)
  , StdMethod (..)
  , Token (..)
  , (.:)
  )
import GitHub.REST.Auth (fromToken, getJWTToken)
import GitHub.REST.Endpoint (endpointPath, renderMethod)
import GitHub.REST.KeyValue (kvToValue)
import GitHub.REST.PageLinks (PageLinks, parsePageLinks)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)

import HydraLib.GitHub.Types (TokenLease (..))

-- | A monad for GitHub REST API requests with configurable endpoint URL.
-- Similar to @GitHubT@ from github-rest, but allows overriding the API URL
-- for testing with a fake API.
newtype GitHubRestT m a = GitHubRestT
  { unRestT :: ReaderT GitHubRestConfig m a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans)

data GitHubRestConfig = GitHubRestConfig
  { cfgSettings  :: GitHubSettings
  , cfgManager   :: Manager
  , cfgGitHubUrl :: Text
  }

data GitHubRestManager = GitHubManager
  { mgrSettings :: GitHubSettings
  , mgrUrl      :: Text
  , mgrManager  :: Manager
  }

data DecodeError = DecodeError
  { decodeErrorMessage  :: Text
  , decodeErrorResponse :: Text
  } deriving (Show)

instance Exception DecodeError

instance (MonadIO m) => MonadGitHubREST (GitHubRestT m) where
  queryGitHubPage ghEndpoint = do
    cfg <- GitHubRestT ask
    liftIO $ queryGitHubRestPage cfg ghEndpoint

-- | Type class for converting structured data to GitHub API key-value pairs.
class RESTKeyValue a where
  toKeyValue :: a -> [KeyValue]

-- | Execute a GitHub REST API request against a configurable endpoint URL.
queryGitHubRestPage ::
  (FromJSON json) =>
  GitHubRestConfig ->
  GHEndpoint ->
  IO (json, PageLinks)
queryGitHubRestPage (GitHubRestConfig {..}) ghEndpoint = do
  let GitHubSettings {..} = cfgSettings
      apiVersionHeader
        | "" <- apiVersion = []
        | otherwise = [("X-GitHub-Api-Version", apiVersion)]
      request =
        (HTTP.parseRequest_ $ Text.unpack $ cfgGitHubUrl <> endpointPath ghEndpoint)
          { HTTP.method = renderMethod ghEndpoint
          , HTTP.requestHeaders =
              [ (hAccept, "application/vnd.github+json")
              , (hUserAgent, userAgent)
              ] ++ apiVersionHeader
                ++ maybe [] ((: []) . (hAuthorization,) . fromToken) token
          , HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode $ kvToValue $ ghData ghEndpoint
          , HTTP.checkResponse = HTTP.throwErrorStatusCodes
          }
  response <- HTTP.httpLbs request cfgManager
  let body = HTTP.responseBody response
      nonEmptyBody = if LByteString.null body then Aeson.encode () else body
      pageLinks = maybe mempty parsePageLinks . lookupHeader "Link" $ response
  case Aeson.eitherDecode nonEmptyBody of
    Right payload -> return (payload, pageLinks)
    Left e -> throwIO $ DecodeError
      { decodeErrorMessage = Text.pack e
      , decodeErrorResponse = Text.decodeUtf8 $ LByteString.toStrict body
      }
  where
    lookupHeader headerName = fmap Text.decodeUtf8 . lookup headerName . HTTP.responseHeaders

runGitHubRestT :: (MonadIO m) => GitHubSettings -> Text -> GitHubRestT m a -> m a
runGitHubRestT settings url action = do
  cfg <- liftIO $ gitHubRestConfig settings url
  runReaderT (unRestT action) cfg

gitHubRestConfig :: GitHubSettings -> Text -> IO GitHubRestConfig
gitHubRestConfig ghSettings ghUrl = do
  ghManager <- HTTP.newManager tlsManagerSettings
  pure $ GitHubRestConfig
    { cfgSettings = ghSettings
    , cfgGitHubUrl = ghUrl
    , cfgManager = ghManager
    }

-- | GitHub API version header value.
gitHubApiVersion :: ByteString
gitHubApiVersion = "2022-11-28"

-- | Load an RSA private key from a PEM file (for GitHub App JWT signing).
loadSigner :: FilePath -> IO PrivateKey
loadSigner file = do
  keys <- readKeyFile file
  case keys of
    [PrivKeyRSA pk] -> pure pk
    _ -> fail $ "Not a valid RSA private key file: " <> file

-- | Fetch all app installations from the GitHub API.
fetchInstallations :: Text -> Int -> FilePath -> ByteString -> IO [(Text, Int)]
fetchInstallations ghEndpointUrl appId appKeyFile ghUserAgent = do
  signer <- loadSigner appKeyFile
  jwt <- getJWTToken signer appId
  let githubSettings = GitHubSettings
        { token = Just jwt
        , userAgent = ghUserAgent
        , apiVersion = gitHubApiVersion
        }
  response <- liftIO $ runGitHubRestT githubSettings ghEndpointUrl $ queryGitHub
    GHEndpoint
      { method = GET
      , endpoint = "/app/installations"
      , endpointVals = []
      , ghData = []
      }
  return $ map (\inst ->
    let account = inst .: "account" :: Value
    in (account .: "login", inst .: "id")
    ) response

-- | Fetch an installation access token for a specific GitHub App installation.
fetchAppInstallationToken :: Text -> Int -> FilePath -> ByteString -> Int -> IO TokenLease
fetchAppInstallationToken ghEndpointUrl appId appKeyFile ghUserAgent appInstallationId = do
  signer <- loadSigner appKeyFile
  jwt <- getJWTToken signer appId
  let githubSettings = GitHubSettings
        { token = Just jwt
        , userAgent = ghUserAgent
        , apiVersion = gitHubApiVersion
        }
  response <- liftIO $ runGitHubRestT githubSettings ghEndpointUrl $ queryGitHub
    GHEndpoint
      { method = POST
      , endpoint = "/app/installations/:appInstallId/access_tokens"
      , endpointVals = ["appInstallId" := appInstallationId]
      , ghData =
          [ "permissions" :=
              [ "checks" := ("write" :: String)
              , "statuses" := ("write" :: String)
              ]
          ]
      }
  expiry <- iso8601ParseM (response .: "expires_at" :: String)
  return $ TokenLease
    { token = BearerToken $ cs (response .: "token" :: String)
    , expiry = Just expiry
    }
