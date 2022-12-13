{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-# language TypeOperators     #-}
{-# language DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Control.Concurrent
import Control.Monad
import Control.Exception (catch, displayException, SomeException)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics
import Data.Text (Text)
import System.Environment (lookupEnv)
import Control.Concurrent.STM (newTChan, atomically)
import Control.Concurrent.STM (newTVarIO, TChan, readTChan, writeTChan, atomically)

import           Data.Aeson hiding (Success)
import           Data.Aeson.Casing
import Servant.Client
import           Data.Proxy
import           Servant.API
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Network.HTTP.Client.TLS (tlsManagerSettings)


-- Data Types
type JobSetId = Int
type EvalRecordId = Int

data HydraNotification
    = EvalStarted JobSetId
    | EvalAdded JobSetId EvalRecordId
    | EvalCached JobSetId EvalRecordId
    | EvalFailed JobSetId
    deriving (Show, Eq)

data StatusState = Error | Failure | Pending | Success
    deriving (Show, Eq, Generic)

instance ToJSON StatusState where
  toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON StatusState where
  parseJSON = genericParseJSON $ aesonDrop 0 camelCase

data GitHubStatus
    = GitHubStatus 
    { owner :: Text
    , repo :: Text
    , sha :: Text
    , status :: StatusState
    , target_url :: Text
    , description :: Maybe Text
    , context :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON GitHubStatus where
  toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON GitHubStatus where
  parseJSON = genericParseJSON $ aesonDrop 0 camelCase

-- Text utils
tshow :: Show a => a -> Text
tshow = Text.pack . show

-- split github:<owner>/<repo>/<hash> into (owner, repo, hash)
-- this is such a god aweful hack!
parseGitHubFlakeURI :: Text -> Maybe (Text, Text, Text)
parseGitHubFlakeURI uri | "github:" `Text.isPrefixOf` uri =
    case Text.splitOn "/" (Text.drop 7 uri) of
        -- TODO: hash == 40 is a _very_ poor approximation to ensure this is a sha
        (owner:repo:hash:[]) | Text.length hash == 40 -> Just (owner, repo, hash)
        _                    -> Nothing
parseGitHubFlakeURI _ = Nothing

toHydraNotification :: Notification -> HydraNotification
toHydraNotification Notification { notificationChannel = chan, notificationData = payload}
    | chan == "eval_started" = let [_, jid]      = words (BS.unpack payload) in EvalStarted (read jid)
    | chan == "eval_added"   = let [_, jid, eid] = words (BS.unpack payload) in EvalAdded (read jid) (read eid)
    | chan == "eval_cached"  = let [_, jid, eid] = words (BS.unpack payload) in EvalCached (read jid) (read eid)
    | chan == "eval_failed"  = let [_, jid]      = words (BS.unpack payload) in EvalFailed (read jid)

handleHydraNotification :: Connection -> HydraNotification -> IO (Maybe GitHubStatus)
handleHydraNotification conn e = flip catch (handler e) $ case e of
    (EvalStarted jid) -> do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        Text.putStrLn $ "Eval Started (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow flake
        case parseGitHubFlakeURI flake of
            Just (owner, repo, hash) -> pure $ Just (GitHubStatus owner repo hash Pending {- target url: -} "" {- description: -} Nothing "ci/eval")
            _ -> pure $ Nothing
    (EvalAdded jid eid) -> do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        [(Only flake')] <- query conn "select flake from jobsetevals where id = ?" (Only eid)
        Text.putStrLn $ "Eval Added (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
        case parseGitHubFlakeURI flake of
            Just (owner, repo, hash) -> pure $ Just (GitHubStatus owner repo hash Success {- target url: -} "" {- description: -} Nothing "ci/eval")
            _ -> pure $ Nothing
    (EvalCached jid eid) -> do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        [(Only flake')] <- query conn "select flake from jobsetevals where id = ?" (Only eid)
        Text.putStrLn $ "Eval Cached (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
        case parseGitHubFlakeURI flake of
            Just (owner, repo, hash) -> pure $ Just (GitHubStatus owner repo hash Success {- target url: -} "" {- description: -} Nothing "ci/eval")
            _ -> pure $ Nothing
    (EvalFailed jid) -> do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        Text.putStrLn $ "Eval Failed (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow (parseGitHubFlakeURI flake)
        case parseGitHubFlakeURI flake of
            Just (owner, repo, hash) -> pure $ Just (GitHubStatus owner repo hash Failure {- target url: -} "" {- description: -} Nothing "ci/eval")
            _ -> pure $ Nothing
    _ -> print e >> pure Nothing

  where handler :: HydraNotification -> SomeException -> IO (Maybe GitHubStatus)
        handler n ex = print (show n ++ " triggert exception " ++ displayException ex) >> pure Nothing

-- GitHub Status PI
-- /repos/{owner}/{repo}/statuses/{sha} with 
-- {"state":"success"
--  ,"target_url":"https://example.com/build/status"
--  ,"description":"The build succeeded!"
--  ,"context":"continuous-integration/jenkins"
-- }
type GitHubAPI = "repos"
                 :> Capture "owner" Text
                 :> Capture "repo" Text
                 :> "statuses"
                 :> Capture "sha" Text
                 :> ReqBody '[JSON] GitHubStatus
                 :> Post '[JSON] Value

mkStatus :: Text -> Text -> Text -> GitHubStatus -> ClientM Value

mkStatus = client (Proxy @GitHubAPI)

statusHandler :: TChan GitHubStatus -> IO ()
statusHandler queue = do
    action <- atomically (readTChan queue)
    print action
    -- todo make servant client request


-- Main
main :: IO ()
main = do 
    user <- maybe mempty id <$> lookupEnv "HYDRA_USER"
    pass <- maybe mempty id <$> lookupEnv "HYDRA_PASS"
    queue <- atomically $ newTChan
    forkIO $ forever $ statusHandler queue
    withConnect (ConnectInfo "localhost" 5432 user pass "hydra") $ \conn -> do
        _ <- execute_ conn "LISTEN eval_started" -- (opaque id, jobset id)
        _ <- execute_ conn "LISTEN eval_added"   -- (opaque id, jobset id, eval record id)
        _ <- execute_ conn "LISTEN eval_cached"  -- (opaque id, jobset id, prev identical eval id)
        _ <- execute_ conn "LISTEN eval_failed"  -- (opaque id, jobset id)
        -- _ <- forkIO $ do
        forever $ do 
            note <- toHydraNotification <$> getNotification conn
            handleHydraNotification conn note >>= \case
                Just status -> atomically (writeTChan queue status)
                Nothing -> pure ()
