{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM                  (TChan, atomically,
                                                          newTChan, readTChan,
                                                          writeTChan)
import           Control.Exception                       (SomeException, catch,
                                                          displayException)
import           Control.Monad
import qualified Data.ByteString.Char8                   as BS
import qualified Data.ByteString.Lazy.Char8              as BSL
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import qualified Data.Text.IO                            as Text
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Notification
import           GHC.Generics
import           System.Environment                      (lookupEnv)

import           Data.Aeson                              hiding (Error, Success)
import           Data.Aeson.Casing
import           Data.Proxy
import           Network.HTTP.Client                     (newManager)
import           Network.HTTP.Client.TLS                 (tlsManagerSettings)
import           Servant.API
import           Servant.Client                          hiding (manager)

import           System.Exit                             (die)
import           System.IO                               (BufferMode (LineBuffering),
                                                          hSetBuffering, stderr,
                                                          stdin, stdout)

-- Data Types
type JobSetId = Int
type EvalRecordId = Int
type BuildId = Int

data HydraNotification
    = EvalStarted JobSetId
    | EvalAdded JobSetId EvalRecordId
    | EvalCached JobSetId EvalRecordId
    | EvalFailed JobSetId
    | BuildQueued BuildId
    | BuildStarted BuildId
    | BuildFinished BuildId
    -- | CachedBuildQueued EvalId BuildId
    -- | CachedBuildFinished EvalId BuildId
    deriving (Show, Eq)

data StatusState = Error | Failure | Pending | Success
    deriving (Show, Eq, Generic)

instance ToJSON StatusState where
  toJSON Error   = "error"
  toJSON Failure = "failure"
  toJSON Pending = "pending"
  toJSON Success = "success"

instance FromJSON StatusState where
  parseJSON = genericParseJSON $ aesonDrop 0 camelCase

data GitHubStatusPayload
    = GitHubStatusPayload
    { state       :: StatusState
    , target_url  :: Text
    , description :: Maybe Text
    , context     :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON GitHubStatusPayload where
    toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON GitHubStatusPayload where
    parseJSON = genericParseJSON $ aesonDrop 0 camelCase

data GitHubStatus
    = GitHubStatus
    { owner   :: Text
    , repo    :: Text
    , sha     :: Text
    , payload :: GitHubStatusPayload
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
    | chan == "eval_started",   [_, jid]      <- words (BS.unpack payload) = EvalStarted (read jid)
    | chan == "eval_added",     [_, jid, eid] <- words (BS.unpack payload) = EvalAdded (read jid) (read eid)
    | chan == "eval_cached",    [_, jid, eid] <- words (BS.unpack payload) = EvalCached (read jid) (read eid)
    | chan == "eval_failed",    [_, jid]      <- words (BS.unpack payload) = EvalFailed (read jid)
    | chan == "build_queued",   [bid]         <- words (BS.unpack payload) = BuildQueued (read bid)
    | chan == "build_started",  [bid]         <- words (BS.unpack payload) = BuildStarted (read bid)
    | chan == "build_finished", [bid]         <- words (BS.unpack payload) = BuildFinished (read bid)
    | otherwise = error $ "Unhandled payload for chan: " ++ BS.unpack chan ++ ": " ++ BS.unpack payload



whenStatusOrJob :: (Maybe StatusState) -> Text -> IO (Maybe GitHubStatus) -> IO (Maybe GitHubStatus)
whenStatusOrJob status job action | or [name `Text.isPrefixOf` job || name `Text.isSuffixOf` job || ("." <> name <> ".") `Text.isInfixOf` job | name <- [ "required", "nonrequired" ]] = action
                                  | Just s <- status, s `elem` [Failure, Error] = action
                                  | otherwise = Text.putStrLn ("Ignoring job: " <> job) >> pure Nothing

withGithubFlake :: Text -> (Text -> Text -> Text -> IO (Maybe GitHubStatus)) -> IO (Maybe GitHubStatus)
withGithubFlake flake action | Just (owner, repo, hash) <- parseGitHubFlakeURI flake = action owner repo hash
                             | otherwise = Text.putStrLn ("Failed to parse flake: " <> flake) >> pure Nothing

handleHydraNotification :: Connection -> Text -> HydraNotification -> IO (Maybe GitHubStatus)
handleHydraNotification conn host e = flip catch (handler e) $ case e of
    -- Evaluations
    (EvalStarted jid) -> do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        Text.putStrLn $ "Eval Started (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow flake
        withGithubFlake flake $ \owner repo hash -> pure $ Just (GitHubStatus owner repo hash (GitHubStatusPayload Pending {- target url: -} ("https://" <> host <> "/jobset/" <> proj <> "/" <> name) {- description: -} Nothing "ci/eval"))

    (EvalAdded jid eid) -> do
        [(proj, name, flake, errmsg, fetcherrmsg)] <- query conn "select project, name, flake, errormsg, fetcherrormsg from jobsets where id = ?" (Only jid)
        [(Only flake')] <- query conn "select flake from jobsetevals where id = ?" (Only eid)
        Text.putStrLn $ "Eval Added (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
        withGithubFlake flake $ \owner repo hash -> pure $ case (errmsg, fetcherrmsg) :: (Maybe Text, Maybe Text) of
                (Just err,_) | not (Text.null err) -> Just (GitHubStatus owner repo hash (GitHubStatusPayload Failure {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors") {- description: -} (Just "Evaluation has errors.") "ci/eval"))
                (_,Just err) | not (Text.null err) -> Just (GitHubStatus owner repo hash (GitHubStatusPayload Failure {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors") {- description: -} (Just "Failed to fetch.") "ci/eval"))
                _            -> Just (GitHubStatus owner repo hash (GitHubStatusPayload Success {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid) {- description: -} Nothing "ci/eval"))

    (EvalCached jid eid) -> do
        [(proj, name, flake, errmsg, fetcherrmsg)] <- query conn "select project, name, flake, errormsg, fetcherrormsg from jobsets where id = ?" (Only jid)
        [(Only flake')] <- query conn "select flake from jobsetevals where id = ?" (Only eid)
        Text.putStrLn $ "Eval Cached (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
        withGithubFlake flake $ \owner repo hash -> pure $ case (errmsg, fetcherrmsg) :: (Maybe Text, Maybe Text) of
                (Just err,_) | not (Text.null err) -> Just (GitHubStatus owner repo hash (GitHubStatusPayload Failure {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors") {- description: -} (Just "Evaluation has errors.") "ci/eval"))
                (_,Just err) | not (Text.null err) -> Just (GitHubStatus owner repo hash (GitHubStatusPayload Failure {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors") {- description: -} (Just "Failed to fetch.") "ci/eval"))
                _            -> Just (GitHubStatus owner repo hash (GitHubStatusPayload Success {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid) {- description: -} Nothing "ci/eval"))

    (EvalFailed jid) -> do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        Text.putStrLn $ "Eval Failed (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow (parseGitHubFlakeURI flake)
        withGithubFlake flake $ \owner repo hash ->
            pure $ Just (GitHubStatus owner repo hash (GitHubStatusPayload Failure {- target url: -} ("https://" <> host <> "/jobset/" <> proj <> "/" <> name) {- description: -} Nothing "ci/eval"))

    -- Builds
    (BuildQueued bid) -> do
        [(proj, name, flake, job, desc, finished)] <- query conn "select j.project, j.name, j.flake, b.job, b.description, b.finished from builds b JOIN jobsets j on (b.jobset_id = j.id) where b.id = ?" (Only bid)
        Text.putStrLn $ "Build Queued (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
        let ghStatus | finished == (1 :: Int) = Success
                     | otherwise   = Failure
        whenStatusOrJob Nothing job $ withGithubFlake flake $ \owner repo hash ->
            pure $ Just (GitHubStatus owner repo hash (GitHubStatusPayload Pending {- target url: -} ("https://" <> host <> "/build/" <> tshow bid) {- description: -} (Just "Build Queued.") ("ci/hydra-build:" <> job)))

    (BuildStarted bid) -> do
        [(proj, name, flake, job, desc, finished)] <- query conn "select j.project, j.name, j.flake, b.job, b.description, b.finished from builds b JOIN jobsets j on (b.jobset_id = j.id) where b.id = ?" (Only bid)
        Text.putStrLn $ "Build Started (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
        let ghStatus | finished == (1 :: Int) = Success
                     | otherwise   = Failure
        whenStatusOrJob Nothing job $ withGithubFlake flake $ \owner repo hash ->
            pure $ Just (GitHubStatus owner repo hash (GitHubStatusPayload Pending {- target url: -} ("https://" <> host <> "/build/" <> tshow bid) {- description: -} (Just "Build Started.") ("ci/hydra-build:" <> job)))

    -- note; buildstatus is only != NULL for Finished, Queued and Started leave it as NULL.
    (BuildFinished bid) -> do
        [(proj, name, flake, job, desc, finished, status)] <- query conn "select j.project, j.name, j.flake, b.job, b.description, b.finished, b.buildstatus from builds b JOIN jobsets j on (b.jobset_id = j.id) where b.id = ?" (Only bid)
        Text.putStrLn $ "Build Finished (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
        let ghStatus | (finished, status) == ((1, 0) :: (Int, Int)) = Success
                     | otherwise   = Failure
        whenStatusOrJob (Just ghStatus) job $ withGithubFlake flake $ \owner repo hash ->
            pure $ Just (GitHubStatus owner repo hash (GitHubStatusPayload ghStatus {- target url: -} ("https://" <> host <> "/build/" <> tshow bid) {- description: -} (Just "Build Finished.") ("ci/hydra-build:" <> job)))

    -- _ -> print e >> pure Nothing

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
                 :> Header "User-Agent" Text
                 :> Header "Accept" Text -- "application/vnd.github+json"
                 :> Header "Authorization" Text -- token <pat> / Bearer ...
                 :> Header "X-GitHub-Api-Version" Text -- "2022-11-28"
                 :> Capture "owner" Text
                 :> Capture "repo" Text
                 :> "statuses"
                 :> Capture "sha" Text
                 :> ReqBody '[JSON] GitHubStatusPayload
                 :> PostCreated '[JSON] Value

-- Auth (Bearer <YOUR-TOKEN>)
-- owner, repo, sha, Status
mkStatus :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Text -> Text -> GitHubStatusPayload -> ClientM Value

mkStatus = client (Proxy @GitHubAPI)

statusHandler :: Text -> TChan GitHubStatus -> IO ()
statusHandler token queue = do
    action <- atomically (readTChan queue)
    print action
    manager <- newManager tlsManagerSettings
    let env = (mkClientEnv manager (BaseUrl Https "api.github.com" 443 ""))
    putStrLn $ BSL.unpack $ encode (payload action)
    res <- flip runClientM env $ do
        mkStatus (Just "hydra-github-bridge")
                 (Just "application/vnd.github+json")
                 (Just token)
                 (Just "2022-11-28")
                 (owner action)
                 (repo action)
                 (sha action)
                 (payload action)
    print res
    -- todo make servant client request


-- Main
main :: IO ()
main = do

    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    host <- maybe "localhost" id <$> lookupEnv "HYDRA_HOST"
    db <- maybe "localhost" id <$> lookupEnv "HYDRA_DB"
    user <- maybe mempty id <$> lookupEnv "HYDRA_USER"
    pass <- maybe mempty id <$> lookupEnv "HYDRA_PASS"
    token <- maybe mempty Text.pack <$> lookupEnv "GITHUB_TOKEN"
    queue <- atomically $ newTChan
    _threadId <- forkIO $ forever $ statusHandler token queue
    withConnect (ConnectInfo db 5432 user pass "hydra") $ \conn -> do
        _ <- execute_ conn "LISTEN eval_started" -- (opaque id, jobset id)
        _ <- execute_ conn "LISTEN eval_added"   -- (opaque id, jobset id, eval record id)
        _ <- execute_ conn "LISTEN eval_cached"  -- (opaque id, jobset id, prev identical eval id)
        _ <- execute_ conn "LISTEN eval_failed"  -- (opaque id, jobset id)
        _ <- execute_ conn "LISTEN build_queued" -- (build id)
        _ <- execute_ conn "LISTEN build_started" -- (build id)
        _ <- execute_ conn "LISTEN build_finished" -- (build id)
        -- _ <- execute_ conn "LISTEN cached_build_queued" -- (eval id, build id)
        -- _ <- execute_ conn "LISTEN cached_build_finished" -- (eval id, build id)
        -- _ <- forkIO $ do
        forever $ do
            note <- toHydraNotification <$> getNotification conn
            handleHydraNotification conn (Text.pack host) note >>= \case
                Just status -> atomically (writeTChan queue status)
                Nothing     -> pure ()
