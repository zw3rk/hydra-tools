{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Concurrent
import           Control.Exception                       (SomeException, catch,
                                                          displayException)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8                   as BS
import qualified Data.ByteString.Lazy.Char8              as BSL
import           Data.Duration                           (oneSecond)
import qualified Data.Duration                           as Duration
import           Data.IORef                              (IORef, newIORef,
                                                          readIORef, writeIORef)
import           Data.List                               (singleton)
import           Data.String.Conversions                 (cs)
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import qualified Data.Text.IO                            as Text
import           Data.Time                               (UTCTime)
import           Data.Time.Clock                         (NominalDiffTime,
                                                          addUTCTime,
                                                          getCurrentTime)
import           Data.Time.Format.ISO8601                (iso8601ParseM)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Notification
import           DiskStore                               (DiskStoreConfig (..))
import           DsQueue                                 (DsQueue)
import qualified DsQueue
import           GHC.Generics
import           GitHub.REST                             as GitHub
import qualified GitHub.REST.Auth                        as GitHub.Auth
import           System.Environment                      (getEnv, lookupEnv)
import           Text.Regex.TDFA                         ((=~))

import           Data.Aeson                              hiding (Error, Success,
                                                          (.:))
import           Data.Aeson.Casing
import           Data.Maybe                              (mapMaybe)

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
    deriving (Show, Eq)

data BuildStatus
    = Succeeded
    | Failed
    | DependencyFailed
    | Aborted
    | Cancelled
    | FailedWithOutput
    | TimedOut
    | LogLimitExceeded
    | OutputSizeLimitExceeded
    | NonDeterministicBuild
    | Other

instance Show BuildStatus where
    show s = case s of
        (Succeeded)               -> "Build succeeded."
        (Failed)                  -> "Build failed."
        (DependencyFailed)        -> "Build dependency failed."
        (Aborted)                 -> "Build aborted."
        (Cancelled)               -> "Build cancelled."
        (FailedWithOutput)        -> "Build failed with output."
        (TimedOut)                -> "Build timed out."
        (LogLimitExceeded)        -> "Build log limit exceeded."
        (OutputSizeLimitExceeded) -> "Build output size limit exceeded."
        (NonDeterministicBuild)   -> "Build is non-deterministic."
        (Other)                   -> "Build failed due to unknown reason."

instance Enum BuildStatus where
    toEnum i = case i of
        ( 0) -> Succeeded
        ( 1) -> Failed
        ( 2) -> DependencyFailed
        ( 3) -> Aborted
        ( 9) -> Aborted
        ( 4) -> Cancelled
        ( 6) -> FailedWithOutput
        ( 7) -> TimedOut
        (10) -> LogLimitExceeded
        (11) -> OutputSizeLimitExceeded
        (12) -> NonDeterministicBuild
        ( _) -> Other
    fromEnum i = case i of
        (Succeeded)               ->  0
        (Failed)                  ->  1
        (DependencyFailed)        ->  2
        (Aborted)                 ->  3
        (Cancelled)               ->  4
        (FailedWithOutput)        ->  6
        (TimedOut)                ->  7
        (LogLimitExceeded)        -> 10
        (OutputSizeLimitExceeded) -> 11
        (NonDeterministicBuild)   -> 12
        (Other)                   -> 99

data StatusState = Error | Failure | Pending | Success
    deriving (Eq, Generic, Read, Show)

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
    } deriving (Eq, Generic, Read, Show)

instance ToJSON GitHubStatusPayload where
    toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON GitHubStatusPayload where
    parseJSON = genericParseJSON $ aesonDrop 0 camelCase

toKeyValue :: GitHubStatusPayload -> [GitHub.KeyValue]
toKeyValue payload =
    [ "state" := payload.state
    , "target_url" := payload.target_url
    , "description" := payload.description
    , "context" := payload.context
    ]

data GitHubStatus
    = GitHubStatus
    { owner   :: Text
    , repo    :: Text
    , sha     :: Text
    , payload :: GitHubStatusPayload
    }
    deriving (Eq, Generic, Read, Show)

instance ToJSON GitHubStatus where
  toJSON = genericToJSON $ aesonDrop 0 camelCase

instance FromJSON GitHubStatus where
  parseJSON = genericParseJSON $ aesonDrop 0 camelCase

toStatusState :: BuildStatus -> StatusState
toStatusState b = case b of
    (Succeeded)               -> Success
    (Failed)                  -> Failure
    (DependencyFailed)        -> Failure
    (Aborted)                 -> Error
    (Cancelled)               -> Error
    (FailedWithOutput)        -> Failure
    (TimedOut)                -> Failure
    (LogLimitExceeded)        -> Error
    (OutputSizeLimitExceeded) -> Error
    (NonDeterministicBuild)   -> Failure
    (Other)                   -> Failure

-- Text utils
tshow :: Show a => a -> Text
tshow = cs . show

humanReadableDuration :: Duration.Seconds -> Text
humanReadableDuration s =
    if s == 0
    then "0s"
    -- seems to always append whitespace
    else Text.strip $ cs $ Duration.humanReadableDuration s

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
    | chan == "eval_started",   [_, jid]      <- words (cs payload) = EvalStarted (read jid)
    | chan == "eval_added",     [_, jid, eid] <- words (cs payload) = EvalAdded (read jid) (read eid)
    | chan == "eval_cached",    [_, jid, eid] <- words (cs payload) = EvalCached (read jid) (read eid)
    | chan == "eval_failed",    [_, jid]      <- words (cs payload) = EvalFailed (read jid)
    | chan == "build_queued",   [bid]         <- words (cs payload) = BuildQueued (read bid)
    | chan == "build_started",  [bid]         <- words (cs payload) = BuildStarted (read bid)
    | chan == "build_finished", (bid:_)       <- words (cs payload) = BuildFinished (read bid)
    | otherwise = error $ "Unhandled payload for chan: " ++ cs chan ++ ": " ++ cs payload



whenStatusOrJob :: (Maybe StatusState) -> Text -> IO [GitHubStatus] -> IO [GitHubStatus]
whenStatusOrJob status job action | or [name `Text.isPrefixOf` job || name `Text.isSuffixOf` job || ("." <> name <> ".") `Text.isInfixOf` job | name <- [ "required", "nonrequired" ]] = action
                                  | Just s <- status, s `elem` [Failure, Error] = action
                                  | otherwise = Text.putStrLn ("Ignoring job: " <> job) >> pure []

withGithubFlake :: Text -> (Text -> Text -> Text -> IO [GitHubStatus]) -> IO [GitHubStatus]
withGithubFlake flake action | Just (owner, repo, hash) <- parseGitHubFlakeURI flake = action owner repo hash
                             | otherwise = Text.putStrLn ("Failed to parse flake: " <> flake) >> pure []

handleHydraNotification :: Connection -> Text -> HydraNotification -> IO [GitHubStatus]
handleHydraNotification conn host e = flip catch (handler e) $ case e of
    -- Evaluations
    (EvalStarted jid) -> do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        Text.putStrLn $ "Eval Started (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow flake
        withGithubFlake flake $ \owner repo hash -> pure $ singleton (GitHubStatus owner repo hash (GitHubStatusPayload Pending {- target url: -} ("https://" <> host <> "/jobset/" <> proj <> "/" <> name) {- description: -} Nothing "ci/eval"))

    (EvalAdded jid eid) -> handleEvalDone jid eid "Added"

    (EvalCached jid eid) -> handleEvalDone jid eid "Cached"

    (EvalFailed jid) -> do
        [(proj, name, flake)] <- query conn "select project, name, flake from jobsets where id = ?" (Only jid)
        Text.putStrLn $ "Eval Failed (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow (parseGitHubFlakeURI flake)
        withGithubFlake flake $ \owner repo hash ->
            pure $ singleton (GitHubStatus owner repo hash (GitHubStatusPayload Failure {- target url: -} ("https://" <> host <> "/jobset/" <> proj <> "/" <> name) {- description: -} Nothing "ci/eval"))

    -- Builds
    (BuildQueued bid) -> do
        [(proj, name, flake, job, desc)] <- query conn ("select j.project, j.name, e.flake, b.job, b.description" <> sqlFromBuild) (Only bid)
        Text.putStrLn $ "Build Queued (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
        whenStatusOrJob Nothing job $ withGithubFlake flake $ \owner repo hash ->
            pure $ singleton (GitHubStatus owner repo hash (GitHubStatusPayload Pending {- target url: -} ("https://" <> host <> "/build/" <> tshow bid) {- description: -} (Just "Build Queued.") ("ci/hydra-build:" <> job)))

    (BuildStarted bid) -> do
        [(proj, name, flake, job, desc)] <- query conn ("select j.project, j.name, e.flake, b.job, b.description" <> sqlFromBuild) (Only bid)
        Text.putStrLn $ "Build Started (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
        whenStatusOrJob Nothing job $ withGithubFlake flake $ \owner repo hash ->
            pure $ singleton (GitHubStatus owner repo hash (GitHubStatusPayload Pending {- target url: -} ("https://" <> host <> "/build/" <> tshow bid) {- description: -} (Just "Build Started.") ("ci/hydra-build:" <> job)))

    -- note; buildstatus is only != NULL for Finished, Queued and Started leave it as NULL.
    (BuildFinished bid) -> do
        [(proj, name, flake, job, desc, finished, status)] <- query conn ("select j.project, j.name, e.flake, b.job, b.description, b.finished, b.buildstatus" <> sqlFromBuild) (Only bid)
        Text.putStrLn $ "Build Finished (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
        let buildStatus = toEnum status
        let ghStatus | finished == (1 :: Int) = toStatusState buildStatus
                     | otherwise              = Failure
        whenStatusOrJob (Just ghStatus) job $ withGithubFlake flake $ \owner repo hash -> do
            buildDuration <- showBuildDuration bid
            let buildDurationDescription = maybe "" (\d -> " Took " <> d <> ".") buildDuration
            pure $ singleton (GitHubStatus owner repo hash (GitHubStatusPayload ghStatus {- target url: -} ("https://" <> host <> "/build/" <> tshow bid) {- description: -} (Just $ tshow buildStatus <> buildDurationDescription) ("ci/hydra-build:" <> job)))

    -- _ -> print e >> pure []

    where
        handler :: HydraNotification -> SomeException -> IO [GitHubStatus]
        handler n ex = print ("ERROR: " ++ show n ++ " triggert exception " ++ displayException ex) >> pure ([] :: [GitHubStatus])

        sqlFromBuild = " from builds b JOIN jobsets j on b.jobset_id = j.id JOIN jobsetevalmembers m on m.build = b.id JOIN jobsetevals e on e.id = m.eval where b.id = ? order by e.id desc fetch first row only"

        handleEvalDone :: JobSetId -> EvalRecordId -> Text -> IO [GitHubStatus]
        handleEvalDone jid eid eventName = do
            [(proj, name, flake, errmsg, fetcherrmsg)] <- query conn "select project, name, flake, errormsg, fetcherrormsg from jobsets where id = ?" (Only jid)
            [(flake', checkouttime, evaltime)] <- query conn "select flake, checkouttime, evaltime from jobsetevals where id = ?" (Only eid) :: IO [(Text, Int, Int)]
            Text.putStrLn $ "Eval " <> eventName <> " (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
            withGithubFlake flake $ \owner repo hash -> do
                let durationDescription = "Fetching took " <> humanReadableDuration (fromIntegral checkouttime * oneSecond) <> ", evaluation took " <> humanReadableDuration (fromIntegral evaltime * oneSecond) <> "."
                evalStatuses <- pure $ case (errmsg, fetcherrmsg) :: (Maybe Text, Maybe Text) of
                    (Just err,_) | not (Text.null err) ->
                        singleton (GitHubStatus owner repo hash (
                            GitHubStatusPayload Failure
                                {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors")
                                {- description: -} (Just $ "Evaluation has errors. " <> durationDescription)
                                "ci/eval"))
                        ++ map
                            (\job -> (GitHubStatus owner repo hash
                                (GitHubStatusPayload Failure
                                    {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors")
                                    {- description: -} (Just $ "Evaluation failed. " <> durationDescription)
                                    ("ci/eval:" <> job))))
                            (parseFailedJobEvals err)
                    (_,Just err) | not (Text.null err) -> singleton (GitHubStatus owner repo hash
                        (GitHubStatusPayload Failure
                            {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors")
                            {- description: -} (Just $ "Failed to fetch. " <> durationDescription)
                            "ci/eval"))
                    _ -> singleton (GitHubStatus owner repo hash
                        (GitHubStatusPayload Success
                            {- target url: -} ("https://" <> host <> "/eval/" <> tshow eid)
                            {- description: -} (Just durationDescription)
                            "ci/eval"))
                -- If this evaluation has builds (is not identical to a previous one), this selects no rows.
                -- Otherwise this selects all builds of the latest previous evaluation that differed from its predecessor.
                -- We then submit a status for each of these builds to the jobset's flake URL (the current one).
                -- This is necessary because `(cached_)?build_finished` notifications are not sent by Hydra
                -- when an evaluation is identical to its predecessor / has no builds.
                rows <- query conn "\
                    \WITH prev_jobseteval AS (              \
                    \    SELECT *                           \
                    \    FROM jobsetevals                   \
                    \    WHERE                              \
                    \        id < ? AND                     \
                    \        jobset_id = ? AND              \
                    \        hasnewbuilds = 1               \
                    \    ORDER BY id DESC                   \
                    \    FETCH FIRST ROW ONLY               \
                    \)                                      \
                    \SELECT b.id, b.job, b.buildstatus      \
                    \FROM builds b                          \
                    \JOIN prev_jobseteval e ON NOT EXISTS ( \
                    \    SELECT NULL                        \
                    \    FROM jobsetevals                   \
                    \    WHERE                              \
                    \        id = ? AND                     \
                    \        hasnewbuilds = 1               \
                    \)                                      \
                    \JOIN jobsetevalmembers m ON            \
                    \    m.build = b.id AND                 \
                    \    m.eval = e.id                      \
                    \WHERE b.finished = 1                   \
                \ " [eid, jid, eid] :: IO [(Int, Text, Int)]
                buildStatuses <- sequence $ map
                    (\(bid, job, status) -> do
                        let buildStatus = toEnum status
                        let ghStatus = toStatusState buildStatus
                        whenStatusOrJob (Just ghStatus) job $ do
                            buildDuration <- showBuildDuration bid
                            let buildDurationDescription = maybe "" (\d -> " Took " <> d <> ".") buildDuration
                            pure $ singleton $ GitHubStatus owner repo hash $
                                GitHubStatusPayload ghStatus
                                    {- target url: -} ("https://" <> host <> "/build/" <> tshow bid)
                                    {- description: -} (Just $ tshow buildStatus <> buildDurationDescription)
                                    ("ci/hydra-build:" <> job))
                    rows
                pure $ evalStatuses ++ concat buildStatuses

        -- Given an evaluation's error message, returns the jobs that could not be evaluated.
        parseFailedJobEvals :: Text -> [Text]
        parseFailedJobEvals errormsg = mapMaybe
            (\line -> case line =~ ("^in job ‘([^’]*)’:$" :: Text) :: (Text, Text, Text, [Text]) of
                (_, _, _, (job:_)) -> Just job
                _                  -> Nothing)
            (Text.lines errormsg)

        showBuildDuration :: BuildId -> IO (Maybe Text)
        showBuildDuration bid = do
            rows <- query conn "\
                \WITH                                                          \
                \    given_build AS (                                          \
                \        SELECT *                                              \
                \        FROM builds                                           \
                \        WHERE id = ?                                          \
                \    ),                                                        \
                \    given_build_output AS (                                   \
                \        SELECT o.*                                            \
                \        FROM buildoutputs o                                   \
                \        JOIN given_build g_b ON o.build = g_b.id              \
                \        FETCH FIRST ROW ONLY                                  \
                \    ),                                                        \
                \    actual_build_step AS (                                    \
                \        SELECT s.*                                            \
                \        FROM buildsteps s                                     \
                \        JOIN buildstepoutputs o ON                            \
                \            o.build = s.build AND                             \
                \            o.stepnr = s.stepnr                               \
                \        JOIN given_build_output g_b_o ON o.path = g_b_o.path  \
                \        WHERE s.busy = 0                                      \
                \        ORDER BY s.status, s.stoptime DESC                    \
                \        FETCH FIRST ROW ONLY                                  \
                \    ),                                                        \
                \    actual_build AS (                                         \
                \        SELECT b.*                                            \
                \        FROM builds b                                         \
                \        JOIN actual_build_step a_b_s ON a_b_s.build = b.id    \
                \    ),                                                        \
                \    given_build_maybe AS (                                    \
                \        SELECT *                                              \
                \        FROM given_build                                      \
                \        WHERE                                                 \
                \            finished = 0 OR                                   \
                \            iscachedbuild = 0                                 \
                \    ),                                                        \
                \    selected_build AS (                                       \
                \        SELECT *                                              \
                \        FROM given_build_maybe                                \
                \                                                              \
                \        UNION ALL                                             \
                \                                                              \
                \        SELECT *                                              \
                \        FROM actual_build                                     \
                \        WHERE NOT EXISTS (SELECT NULL FROM given_build_maybe) \
                \    )                                                         \
                \SELECT selected_build.stoptime - selected_build.starttime     \
                \FROM selected_build                                           \
            \ " (Only bid) :: IO [(Only Int)]
            pure $ case rows of
                [(Only duration)] -> Just $ humanReadableDuration $ (fromIntegral duration) * oneSecond
                _                 -> Nothing

ghApiVersion :: BS.ByteString
ghApiVersion = "2022-11-28"

statusHandler :: BS.ByteString -> IO TokenLease -> DsQueue GitHubStatus -> IO ()
statusHandler ghUserAgent getGitHubToken queue = do
    action <- DsQueue.read queue
    print action
    BSL.putStrLn $ encode action.payload

    ghToken <- getGitHubToken

    let githubSettings = GitHubSettings
            { token = Just ghToken.token
            , userAgent = ghUserAgent
            , apiVersion = ghApiVersion
            }
    res <- liftIO $ runGitHubT githubSettings $ queryGitHub GHEndpoint
        { method = POST
        , endpoint = "/repos/:owner/:repo/statuses/:sha"
        , endpointVals =
            [ "owner" := action.owner
            , "repo" := action.repo
            , "sha" := action.sha
            ]
        , ghData = toKeyValue action.payload
        }
        :: IO Value
    BSL.putStrLn $ encode res

data TokenLease = TokenLease
    { token  :: Token
    , expiry :: Maybe UTCTime
    }

fetchGitHubToken :: BS.ByteString -> IO TokenLease
fetchGitHubToken ghUserAgent = do
    ghTokenBS <- maybe mempty cs <$> lookupEnv "GITHUB_TOKEN"
    let ghToken
            | BS.null ghTokenBS                = Nothing
            | "ghp_" `BS.isPrefixOf` ghTokenBS = Just $ AccessToken ghTokenBS
            | otherwise                        = Just $ BearerToken ghTokenBS

    (\n -> maybe n (\t -> return $ TokenLease t Nothing) ghToken) $ do
        ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
        signer <- GitHub.Auth.loadSigner ghAppKeyFile

        ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
        jwt <- GitHub.Auth.getJWTToken signer ghAppId

        ghAppInstallId <- getEnv "GITHUB_APP_INSTALL_ID" >>= return . read :: IO Int

        let githubSettings = GitHubSettings
                { token = Just jwt
                , userAgent = ghUserAgent
                , apiVersion = ghApiVersion
                }
        response <- liftIO $ runGitHubT githubSettings $ queryGitHub GHEndpoint
            { method = POST
            , endpoint = "/app/installations/:appInstallId/access_tokens"
            , endpointVals = [ "appInstallId" := ghAppInstallId ]
            , ghData =
                [ "permissions" :=
                    [ "checks" := ("write" :: String)
                    , "statuses" := ("write" :: String)
                    ]
                ]
            }

        expiry <- iso8601ParseM (response .: "expires_at" :: String)

        putStrLn $ "Fetched new GitHub App installation token valid until " <> show expiry

        return $ TokenLease
            { token = BearerToken $ cs (response .: "token" :: String)
            , expiry = Just expiry
            }

refreshGitHubToken :: IORef TokenLease -> IO TokenLease -> IO TokenLease
refreshGitHubToken lease fetch = do
    lease' <- readIORef lease
    (\j -> maybe (return lease') j lease'.expiry) $ \expiry -> do
        now <- getCurrentTime
        let buffer = 5 :: NominalDiffTime
        if addUTCTime buffer now < expiry
        then return lease'
        else do
            putStrLn $ "GitHub token expired or will expire within the next " <> show buffer <> ", fetching a new one..."
            newLease <- fetch
            writeIORef lease newLease
            return newLease

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    host <- maybe "localhost" id <$> lookupEnv "HYDRA_HOST"
    db <- maybe "localhost" id <$> lookupEnv "HYDRA_DB"
    user <- maybe mempty id <$> lookupEnv "HYDRA_USER"
    pass <- maybe mempty id <$> lookupEnv "HYDRA_PASS"

    ghUserAgent <- maybe "hydra-github-bridge" cs <$> lookupEnv "GITHUB_USER_AGENT"
    ghToken <- fetchGitHubToken ghUserAgent >>= newIORef
    let getGitHubToken = refreshGitHubToken ghToken (fetchGitHubToken ghUserAgent)

    mStateDir <- lookupEnv "HYDRA_STATE_DIR"
    putStrLn $ maybe "No $HYDRA_STATE_DIR specified." ("$HYDRA_STATE_DIR is: " ++) mStateDir

    queue <- DsQueue.new $ fmap (\sd -> DiskStoreConfig sd "hgb-" 10) mStateDir
    _threadId <- forkIO $ forever $ statusHandler ghUserAgent getGitHubToken queue
    withConnect (ConnectInfo db 5432 user pass "hydra") $ \conn -> do
        _ <- execute_ conn "LISTEN eval_started"   -- (opaque id, jobset id)
        _ <- execute_ conn "LISTEN eval_added"     -- (opaque id, jobset id, eval record id)
        _ <- execute_ conn "LISTEN eval_cached"    -- (opaque id, jobset id, prev identical eval id)
        _ <- execute_ conn "LISTEN eval_failed"    -- (opaque id, jobset id)
        _ <- execute_ conn "LISTEN build_queued"   -- (build id)
        _ <- execute_ conn "LISTEN build_started"  -- (build id)
        _ <- execute_ conn "LISTEN build_finished" -- (build id, dependent build ids...)
        forever $ do
            note <- toHydraNotification <$> getNotification conn
            statuses <- handleHydraNotification conn (cs host) note
            forM_ statuses $ \status -> DsQueue.write queue status
