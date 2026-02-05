{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Lib.GitHub.Client
  ( GitHubRestT (..),
    GitHubRestConfig (..),
    GitHubRestManager (..),
    DecodeError (..),
    RESTKeyValue (..),
    CheckRunStatus (..),
    CheckRunConclusion (..),
    CheckRunOutput (..),
    CheckRunPayload (..),
    CheckRun (..),
    TokenLease (..),
    runGitHubRestT,
    gitHubRestConfig,
    gitHubApiVersion,
    loadSigner,
    fetchInstallations,
    fetchAppInstallationToken,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadTrans)
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.Types (ToJSON (..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LByteString
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.X509 (PrivKey (..))
import Data.X509.File (readKeyFile)
import GHC.Generics (Generic)
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue (..),
    MonadGitHubREST (..),
    StdMethod (..),
    Token (..),
    (.:),
  )
import GitHub.REST.Auth (fromToken, getJWTToken)
import GitHub.REST.Endpoint (endpointPath, renderMethod)
import GitHub.REST.KeyValue (kvToValue)
import GitHub.REST.PageLinks (PageLinks, parsePageLinks)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)

-- | A simple monad that can run GitHub Rest API requests. This is similar to @GitHubT@,
-- except that we allow overriding the GitHub API URL. This allows us to test locally
-- with a fake API.
newtype GitHubRestT m a = GitHubRestT
  {unRestT :: ReaderT GitHubRestConfig m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadIO,
      MonadTrans
    )

data GitHubRestConfig = GitHubRestConfig
  { cfgSettings :: GitHubSettings,
    cfgManager :: Manager,
    cfgGitHubUrl :: Text
  }

data GitHubRestManager = GitHubManager
  { mgrSettings :: GitHubSettings,
    mgrUrl :: Text,
    mgrManager :: Manager
  }

data DecodeError = DecodeError
  { decodeErrorMessage :: Text,
    decodeErrorResponse :: Text
  }
  deriving (Show)

instance Exception DecodeError

instance (MonadIO m) => MonadGitHubREST (GitHubRestT m) where
  queryGitHubPage ghEndpoint = do
    cfg <- GitHubRestT ask
    liftIO $ queryGitHubRestPage cfg ghEndpoint

class RESTKeyValue a where
  toKeyValue :: a -> [KeyValue]

data CheckRunStatus
  = Queued
  | InProgress
  | Completed
  deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRunStatus where
  toJSON = \case
    (Queued) -> "queued"
    (InProgress) -> "in_progress"
    (Completed) -> "completed"

instance FromJSON CheckRunStatus where
  parseJSON = \case
    "queued" -> return Queued
    "in_progress" -> return InProgress
    "completed" -> return Completed
    _ -> fail "Invalid CheckRunStatus"

data CheckRunConclusion
  = ActionRequired
  | Cancelled
  | Failure
  | Neutral
  | Success
  | Skipped
  | Stale
  | TimedOut
  deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRunConclusion where
  toJSON = \case
    (ActionRequired) -> "action_required"
    (Cancelled) -> "cancelled"
    (Failure) -> "failure"
    (Neutral) -> "neutral"
    (Success) -> "success"
    (Skipped) -> "skipped"
    (Stale) -> "stale"
    (TimedOut) -> "timed_out"

instance FromJSON CheckRunConclusion where
  parseJSON = \case
    "action_required" -> return ActionRequired
    "cancelled" -> return Cancelled
    "failure" -> return Failure
    "neutral" -> return Neutral
    "success" -> return Success
    "skipped" -> return Skipped
    "stale" -> return Stale
    "timed_out" -> return TimedOut
    _ -> fail "Invalid CheckRunConclusion"

data CheckRunOutput = CheckRunOutput
  { title :: Text,
    summary :: Text,
    text :: Maybe Text
    -- , annotations :: [CheckRunOutputAnnotation] -- TODO
    -- , images :: [CheckRunOutputImage] -- TODO
  }
  deriving (Eq, Generic, Read, Show)

instance RESTKeyValue CheckRunOutput where
  toKeyValue output =
    let maybeKV k v f = maybe mempty (\a -> [k := f a]) v
     in [ "title" := output.title,
          "summary" := output.summary
        ]
          ++ maybeKV "text" output.text id

instance ToJSON CheckRunOutput where
  toJSON = Aeson.genericToJSON $ aesonDrop 0 camelCase

instance FromJSON CheckRunOutput where
  parseJSON = Aeson.genericParseJSON $ aesonDrop 0 camelCase

data CheckRunPayload = CheckRunPayload
  { name :: Text,
    headSha :: Text,
    detailsUrl :: Maybe Text,
    externalId :: Maybe Text,
    status :: CheckRunStatus,
    conclusion :: Maybe CheckRunConclusion,
    startedAt :: Maybe UTCTime,
    completedAt :: Maybe UTCTime,
    output :: Maybe CheckRunOutput
    -- , actions      :: [KeyValue] -- TODO
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRunPayload where
  toJSON = Aeson.genericToJSON $ aesonDrop 0 camelCase

instance FromJSON CheckRunPayload where
  parseJSON = Aeson.genericParseJSON $ aesonDrop 0 camelCase

instance RESTKeyValue CheckRunPayload where
  toKeyValue payload =
    let maybeKV k v f = maybe mempty (\a -> [k := f a]) v
     in [ "name" := payload.name,
          "head_sha" := payload.headSha,
          "status" := payload.status
        ]
          ++ maybeKV "details_url" payload.detailsUrl id
          ++ maybeKV "external_id" payload.externalId id
          ++ maybeKV "conclusion" payload.conclusion id
          ++ maybeKV "started_at" payload.startedAt iso8601Show
          ++ maybeKV "completed_at" payload.completedAt iso8601Show
          ++ maybeKV "output" payload.output toKeyValue

-- The following table exists in the databse:
{-
DROP TABLE IF EXISTS github_status;
CREATE TABLE github_status (
    id SERIAL PRIMARY KEY,
    owner TEXT NOT NULL,
    repo TEXT NOT NULL,
    headSha TEXT NOT NULL,
    name TEXT NOT NULL,
    UNIQUE (owner, repo, headSha, name)
);
DROP TABLE IF EXISTS github_status_payload;
CREATE TABLE github_status_payload (
    id SERIAL PRIMARY KEY,
    status_id INTEGER NOT NULL, -- fk: github_status.id
    payload JSONB NOT NULL,
    created TIMESTAMP DEFAULT NOW(),
    sent TIMESTAMP DEFAULT NULL,
    tries INTEGER DEFAULT 0,
    FOREIGN KEY (status_id) REFERENCES github_status (id) ON DELETE CASCADE
);
-}

data CheckRun = CheckRun
  { owner :: Text,
    repo :: Text,
    payload :: CheckRunPayload
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRun where
  toJSON = Aeson.genericToJSON $ aesonDrop 0 camelCase

instance FromJSON CheckRun where
  parseJSON = Aeson.genericParseJSON $ aesonDrop 0 camelCase

data TokenLease = TokenLease
  { token :: Token,
    expiry :: Maybe UTCTime
  }
  deriving (Show)

-- | Same as @queryGitHubPage@, except taking a @GitHubRestConfig@ parameter and running
-- in IO. This is mostly the same as @queryGitHubPageIO@, except the GitHub Endpoint URL
-- can be overridden.
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
          { HTTP.method = renderMethod ghEndpoint,
            HTTP.requestHeaders =
              [ (hAccept, "application/vnd.github+json"),
                (hUserAgent, userAgent)
              ]
                ++ apiVersionHeader
                ++ maybe [] ((: []) . (hAuthorization,) . fromToken) token,
            HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode $ kvToValue $ ghData ghEndpoint,
            HTTP.checkResponse = HTTP.throwErrorStatusCodes
          }

  response <- HTTP.httpLbs request cfgManager

  let body = HTTP.responseBody response
      -- empty body always errors when decoding, even if the end user doesn't care about the
      -- result, like creating a branch, when the endpoint doesn't return anything.
      --
      -- In this case, pretend like the server sent back an encoded version of the unit type,
      -- so that `queryGitHub endpoint` would be typed to `m ()`.
      nonEmptyBody = if LByteString.null body then Aeson.encode () else body
      pageLinks = maybe mempty parsePageLinks . lookupHeader "Link" $ response

  case Aeson.eitherDecode nonEmptyBody of
    Right payload -> return (payload, pageLinks)
    Left e ->
      throwIO $
        DecodeError
          { decodeErrorMessage = Text.pack e,
            decodeErrorResponse = Text.decodeUtf8 $ LByteString.toStrict body
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
  pure $
    GitHubRestConfig
      { cfgSettings = ghSettings,
        cfgGitHubUrl = ghUrl,
        cfgManager = ghManager
      }

gitHubApiVersion :: ByteString
gitHubApiVersion = "2022-11-28"

loadSigner :: FilePath -> IO PrivateKey
loadSigner file = do
  keys <- readKeyFile file
  case keys of
    [PrivKeyRSA pk] -> pure pk
    _ -> fail $ "Not a valid RSA private key file: " <> file

fetchInstallations :: Text -> Int -> FilePath -> ByteString -> IO [(Text, Int)]
fetchInstallations ghEndpointUrl appId appKeyFile ghUserAgent = do
  signer <- loadSigner appKeyFile
  jwt <- getJWTToken signer appId

  let githubSettings =
        GitHubSettings
          { token = Just jwt,
            userAgent = ghUserAgent,
            apiVersion = gitHubApiVersion
          }
  response <-
    liftIO $
      runGitHubRestT githubSettings ghEndpointUrl $
        queryGitHub
          GHEndpoint
            { method = GET,
              endpoint = "/app/installations",
              endpointVals = [],
              ghData = []
            }

  return $
    map
      ( \inst ->
          let account = inst .: "account" :: Value
           in (account .: "login", inst .: "id")
      )
      response

fetchAppInstallationToken :: Text -> Int -> FilePath -> ByteString -> Int -> IO TokenLease
fetchAppInstallationToken ghEndpointUrl appId appKeyFile ghUserAgent appInstallationId = do
  signer <- loadSigner appKeyFile
  jwt <- getJWTToken signer appId

  let githubSettings =
        GitHubSettings
          { token = Just jwt,
            userAgent = ghUserAgent,
            apiVersion = gitHubApiVersion
          }
  response <-
    liftIO $
      runGitHubRestT githubSettings ghEndpointUrl $
        queryGitHub
          GHEndpoint
            { method = POST,
              endpoint = "/app/installations/:appInstallId/access_tokens",
              endpointVals = ["appInstallId" := appInstallationId],
              ghData =
                [ "permissions"
                    := [ "checks" := ("write" :: String),
                         "statuses" := ("write" :: String)
                       ]
                ]
            }

  expiry <- iso8601ParseM (response .: "expires_at" :: String)

  return $
    TokenLease
      { token = BearerToken $ cs (response .: "token" :: String),
        expiry = Just expiry
      }
