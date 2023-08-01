{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Lib.GitHub where

import           Control.Monad.IO.Class
import           Data.Aeson               hiding (Error, KeyValue, Success,
                                           (.:))
import           Data.Aeson.Casing
import qualified Data.ByteString.Char8    as BS
import           Data.IORef               (IORef, readIORef, writeIORef)
import           Data.String.Conversions  (cs)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Time                (UTCTime)
import           Data.Time.Clock          (NominalDiffTime, addUTCTime,
                                           getCurrentTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import           GHC.Generics
import           GitHub.REST              (GHEndpoint (..), GitHubSettings (..),
                                           KeyValue ((:=)), StdMethod (POST),
                                           Token (BearerToken), queryGitHub,
                                           runGitHubT, (.:))
import           GitHub.REST.Auth         (getJWTToken, loadSigner)

class RESTKeyValue a where
    toKeyValue :: a -> [KeyValue]

data CheckRunStatus
    = Queued
    | InProgress
    | Completed
    deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRunStatus where
    toJSON = \case
        (Queued)     -> "queued"
        (InProgress) -> "in_progress"
        (Completed)  -> "completed"

instance FromJSON CheckRunStatus where
    parseJSON = genericParseJSON $ aesonDrop 0 camelCase

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
        (Cancelled)      -> "cancelled"
        (Failure)        -> "failure"
        (Neutral)        -> "neutral"
        (Success)        -> "success"
        (Skipped)        -> "skipped"
        (Stale)          -> "stale"
        (TimedOut)       -> "timed_out"

instance FromJSON CheckRunConclusion where
    parseJSON = genericParseJSON $ aesonDrop 0 camelCase

data CheckRunOutput = CheckRunOutput
    { title   :: Text
    , summary :: Text
    , text    :: Maybe Text
    -- , annotations :: [CheckRunOutputAnnotation] -- TODO
    -- , images :: [CheckRunOutputImage] -- TODO
    } deriving (Eq, Generic, Read, Show)

instance RESTKeyValue CheckRunOutput where
    toKeyValue output =
        let
            maybeKV k v f = maybe mempty (\a -> [k := f a]) v
        in
        [ "title" := output.title
        , "summary" := output.summary
        ]
        ++ maybeKV "text" output.text id

instance ToJSON CheckRunOutput where
    toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON CheckRunOutput where
    parseJSON = genericParseJSON $ aesonDrop 0 camelCase

data CheckRunPayload = CheckRunPayload
    { name        :: Text
    , headSha     :: Text
    , detailsUrl  :: Maybe Text
    , externalId  :: Maybe Text
    , status      :: CheckRunStatus
    , conclusion  :: Maybe CheckRunConclusion
    , startedAt   :: Maybe UTCTime
    , completedAt :: Maybe UTCTime
    , output      :: Maybe CheckRunOutput
    -- , actions      :: [KeyValue] -- TODO
    } deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRunPayload where
    toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON CheckRunPayload where
    parseJSON = genericParseJSON $ aesonDrop 0 camelCase

instance RESTKeyValue CheckRunPayload where
    toKeyValue payload =
        let
            maybeKV k v f = maybe mempty (\a -> [k := f a]) v
        in
        [ "name"     := payload.name
        , "head_sha" := payload.headSha
        , "status"   := payload.status
        ]
        ++ maybeKV "details_url"  payload.detailsUrl  id
        ++ maybeKV "external_id"  payload.externalId  id
        ++ maybeKV "conclusion"   payload.conclusion  id
        ++ maybeKV "started_at"   payload.startedAt   iso8601Show
        ++ maybeKV "completed_at" payload.completedAt iso8601Show
        ++ maybeKV "output"       payload.output      toKeyValue

data CheckRun = CheckRun
    { owner   :: Text
    , repo    :: Text
    , payload :: CheckRunPayload
    }
    deriving (Eq, Generic, Read, Show)

instance ToJSON CheckRun where
    toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON CheckRun where
    parseJSON = genericParseJSON $ aesonDrop 0 camelCase

parseGitHubFlakeURI :: Text -> Maybe (Text, Text, Text)
parseGitHubFlakeURI uri
    | "github:" `Text.isPrefixOf` uri =
        case Text.splitOn "/" (Text.drop 7 uri) of
            -- TODO: hash == 40 is a _very_ poor approximation to ensure this is a sha
            (owner:repo:hash:[]) | Text.length hash == 40 -> Just (owner, repo, hash)
            _                    -> Nothing
    | otherwise = Nothing

data TokenLease = TokenLease
    { token  :: Token
    , expiry :: Maybe UTCTime
    }

apiVersion :: BS.ByteString
apiVersion = "2022-11-28"

fetchAppInstallationToken :: Int -> FilePath -> Int -> BS.ByteString -> IO TokenLease
fetchAppInstallationToken appId appKeyFile appInstallationId ghUserAgent = do
    signer <- loadSigner appKeyFile
    jwt <- getJWTToken signer appId

    let githubSettings = GitHubSettings
            { token = Just jwt
            , userAgent = ghUserAgent
            , apiVersion = Lib.GitHub.apiVersion
            }
    response <- liftIO $ runGitHubT githubSettings $ queryGitHub GHEndpoint
        { method = POST
        , endpoint = "/app/installations/:appInstallId/access_tokens"
        , endpointVals = [ "appInstallId" := appInstallationId ]
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

getValidToken :: NominalDiffTime -> IORef TokenLease -> IO TokenLease -> IO TokenLease
getValidToken buffer lease fetch = do
    lease' <- readIORef lease
    (\j -> maybe (return lease') j lease'.expiry) $ \expiry -> do
        now <- getCurrentTime
        if addUTCTime buffer now < expiry
        then return lease'
        else do
            newLease <- fetch
            writeIORef lease newLease
            return newLease
