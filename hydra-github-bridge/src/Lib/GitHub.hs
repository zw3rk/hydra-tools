{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib.GitHub where

import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.Aeson hiding
  ( Error,
    KeyValue,
    Success,
    (.:),
  )
import Data.Aeson.Casing
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, readIORef, writeIORef)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.Clock
  ( NominalDiffTime,
    addUTCTime,
    getCurrentTime,
  )
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import GHC.Generics
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue ((:=)),
    StdMethod (GET, POST),
    Token (BearerToken),
    queryGitHub,
    runGitHubT,
    (.:),
  )
import GitHub.REST.Auth (getJWTToken, loadSigner)

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
  toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON CheckRunOutput where
  parseJSON = genericParseJSON $ aesonDrop 0 camelCase

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
  toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON CheckRunPayload where
  parseJSON = genericParseJSON $ aesonDrop 0 camelCase

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
  toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON CheckRun where
  parseJSON = genericParseJSON $ aesonDrop 0 camelCase

parseGitHubFlakeURI :: Text -> Maybe (Text, Text, Text)
parseGitHubFlakeURI uri
  | "github:" `Text.isPrefixOf` uri =
      case splitFlakeRef (Text.drop 7 uri) of
        Just (owner, repo, hash) | Text.length hash == 40 -> Just (owner, repo, hash)
        Just (owner, repo, hash)
          | (hash' : _) <- Text.splitOn "?" hash,
            Text.length hash' == 40 ->
              Just (owner, repo, hash')
        _ -> Nothing
  | otherwise = Nothing
  where
    splitFlakeRef t =
      case Text.splitOn "/" t of
        -- Query parameters can contain slashes that we don't want to split, so combine everything
        -- after repo
        (owner : repo : ts) -> Just (owner, repo, Text.concat ts)
        _ -> Nothing

data TokenLease = TokenLease
  { token :: Token,
    expiry :: Maybe UTCTime
  }
  deriving (Show)

apiVersion :: BS.ByteString
apiVersion = "2022-11-28"

fetchInstallations :: Int -> FilePath -> BS.ByteString -> IO [(Text, Int)]
fetchInstallations appId appKeyFile ghUserAgent = do
  signer <- loadSigner appKeyFile
  jwt <- getJWTToken signer appId

  let githubSettings =
        GitHubSettings
          { token = Just jwt,
            userAgent = ghUserAgent,
            apiVersion = Lib.GitHub.apiVersion
          }
  response <-
    liftIO $
      runGitHubT githubSettings $
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

fetchAppInstallationToken :: Int -> FilePath -> BS.ByteString -> Int -> IO TokenLease
fetchAppInstallationToken appId appKeyFile ghUserAgent appInstallationId = do
  signer <- loadSigner appKeyFile
  jwt <- getJWTToken signer appId

  let githubSettings =
        GitHubSettings
          { token = Just jwt,
            userAgent = ghUserAgent,
            apiVersion = Lib.GitHub.apiVersion
          }
  response <-
    liftIO $
      runGitHubT githubSettings $
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

getValidToken :: NominalDiffTime -> IORef [(String, TokenLease)] -> (String -> IO TokenLease) -> IO [(String, TokenLease)]
getValidToken buffer lease fetch = do
  leases' <- readIORef lease
  now <- getCurrentTime
  leases'' <- forM leases' $ \(owner, tok) -> do
    case tok.expiry of
      Just expiry | addUTCTime buffer now < expiry -> return (owner, tok)
      _ -> (owner,) <$> fetch owner
  writeIORef lease leases''
  return leases''
