{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Codec.Compression.BZip qualified as BZip
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async as Async
import Control.Exception
  ( SomeException,
    catchJust,
    displayException,
    fromException,
    throw,
    toException,
    try,
  )
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (Error, Success)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSLw
import Data.Duration (oneSecond)
import Data.Foldable (foldr')
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import Data.List
  ( find,
    intercalate,
    singleton,
  )
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock
  ( NominalDiffTime,
    addUTCTime,
    secondsToNominalDiffTime,
  )
import Data.Time.Clock.POSIX
  ( getPOSIXTime,
    posixSecondsToUTCTime,
  )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Debug.Trace (traceShowId)
import GitHub.REST
  ( GHEndpoint (..),
    GitHubSettings (..),
    KeyValue ((:=)),
    StdMethod (POST),
    queryGitHub,
  )
import Lib
  ( binarySearch,
    toCheckRunConclusion,
  )
import Lib.Data.Duration (humanReadableDuration)
import Lib.Data.List (takeEnd)
import Lib.Data.Text (indentLine)
import Lib.GitHub (parseGitHubFlakeURI)
import Lib.GitHub qualified as GitHub
import Lib.Hydra qualified as Hydra
import Network.HTTP.Client qualified as HTTP
import System.Environment (getEnv, lookupEnv)
import System.FilePath
  ( takeFileName,
    (<.>),
    (</>),
  )
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
    stdin,
    stdout,
  )
import System.IO.Error
  ( catchIOError,
    ioeGetErrorType,
    isDoesNotExistErrorType,
  )
import Text.Regex.TDFA ((=~))

-- Text utils
tshow :: (Show a) => a -> Text
tshow = cs . show

toHydraNotification :: Notification -> Hydra.Notification
toHydraNotification Notification {notificationChannel = chan, notificationData = payload}
  | chan == "eval_started", [_, jid] <- words (cs payload) = Hydra.EvalStarted (read jid)
  | chan == "eval_added", [_, jid, eid] <- words (cs payload) = Hydra.EvalAdded (read jid) (read eid)
  | chan == "eval_cached", [_, jid, eid] <- words (cs payload) = Hydra.EvalCached (read jid) (read eid)
  | chan == "eval_failed", [_, jid] <- words (cs payload) = Hydra.EvalFailed (read jid)
  | chan == "build_queued", [bid] <- words (cs payload) = Hydra.BuildQueued (read bid)
  | chan == "cached_build_queued", [_, bid] <- words (cs payload) = Hydra.BuildQueued (read bid)
  | chan == "build_started", [bid] <- words (cs payload) = Hydra.BuildStarted (read bid)
  | chan == "build_finished", (bid : depBids) <- words (cs payload) = Hydra.BuildFinished (read bid) (map read depBids)
  | chan == "cached_build_finished", [_, bid] <- words (cs payload) = Hydra.BuildFinished (read bid) []
  | otherwise = error $ "Unhandled payload for chan: " ++ cs chan ++ ": " ++ cs payload

whenStatusOrJob :: Maybe GitHub.CheckRunConclusion -> Maybe Hydra.BuildStatus -> Text -> IO [GitHub.CheckRun] -> IO [GitHub.CheckRun]
whenStatusOrJob status prevStepStatus job action
  | or [name `Text.isPrefixOf` job || name `Text.isSuffixOf` job || ("." <> name <> ".") `Text.isInfixOf` job | name <- ["required", "nonrequired"]] = action
  | Just s <- status, s `elem` [GitHub.Failure, GitHub.Cancelled, GitHub.Stale, GitHub.TimedOut] = action
  | Just pss <- prevStepStatus, pss /= Hydra.Succeeded && maybe True (== GitHub.Success) status = action
  | otherwise = Text.putStrLn ("Ignoring job: " <> job) >> pure []

withGithubFlake :: Text -> (Text -> Text -> Text -> IO [GitHub.CheckRun]) -> IO [GitHub.CheckRun]
withGithubFlake flake action
  | Just (owner, repo, hash) <- parseGitHubFlakeURI flake = action owner repo hash
  | otherwise = Text.putStrLn ("Failed to parse flake: " <> flake) >> pure []

handleHydraNotification :: Connection -> Text -> FilePath -> Hydra.Notification -> IO [GitHub.CheckRun]
handleHydraNotification conn host stateDir e = (\computation -> catchJust catchJustPredicate computation (handler e)) $ case e of
  -- Evaluations
  (Hydra.EvalStarted jid) -> do
    [(proj, name, flake, triggertime)] <- query conn "select project, name, flake, triggertime from jobsets where id = ?" (Only jid)
    Text.putStrLn $ "Eval Started (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow flake
    withGithubFlake flake $ \owner repo hash ->
      pure $
        singleton $
          GitHub.CheckRun owner repo $
            GitHub.CheckRunPayload
              { name = "ci/eval",
                headSha = hash,
                detailsUrl = Just $ "https://" <> host <> "/jobset/" <> proj <> "/" <> name,
                externalId = Just $ tshow jid,
                status = GitHub.InProgress,
                conclusion = Nothing,
                -- `triggertime` is `Nothing` if the evaluation is cached.
                startedAt = (triggertime :: Maybe Int) >>= Just . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral,
                completedAt = Nothing,
                output = Nothing
              }
  (Hydra.EvalAdded jid eid) -> handleEvalDone jid eid "Added"
  (Hydra.EvalCached jid eid) -> handleEvalDone jid eid "Cached"
  (Hydra.EvalFailed jid) -> do
    [(proj, name, flake, fetcherrormsg, errormsg, errortime)] <- query conn "select project, name, flake, fetcherrormsg, errormsg, errortime from jobsets where id = ?" (Only jid)
    Text.putStrLn $ "Eval Failed (" <> tshow jid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> tshow (parseGitHubFlakeURI flake)
    withGithubFlake flake $ \owner repo hash ->
      pure $
        singleton $
          GitHub.CheckRun owner repo $
            GitHub.CheckRunPayload
              { name = "ci/eval",
                headSha = hash,
                detailsUrl = Just $ "https://" <> host <> "/jobset/" <> proj <> "/" <> name,
                externalId = Just $ tshow jid,
                status = GitHub.Completed,
                conclusion = Just GitHub.Failure,
                startedAt = Nothing, -- Hydra does not record this information but GitHub still has it
                completedAt = Just . posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral (errortime :: Int),
                output =
                  Just $
                    GitHub.CheckRunOutput
                      { title = "Evaluation failed",
                        summary = "",
                        text =
                          maybe
                            (errormsg >>= mkEvalErrorSummary)
                            mkFetchErrorSummary
                            fetcherrormsg
                      }
              }

  -- Builds
  (Hydra.BuildQueued bid) -> do
    [(proj, name, flake, job, desc)] <- query conn ("select j.project, j.name, e.flake, b.job, b.description" <> sqlFromBuild) (Only bid)
    Text.putStrLn $ "Build Queued (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
    steps <- query conn ("SELECT status FROM buildsteps WHERE build = ? ORDER BY stepnr DESC LIMIT 2") (Only bid) :: IO [(Only (Maybe Int))]
    let prevStepStatus
          | length steps >= 2 = (\(Only statusInt) -> statusInt <&> toEnum) $ steps !! 1
          | otherwise = Nothing
    whenStatusOrJob Nothing prevStepStatus job $ withGithubFlake flake $ \owner repo hash ->
      pure $
        singleton $
          GitHub.CheckRun owner repo $
            GitHub.CheckRunPayload
              { name = "ci/hydra-build:" <> job,
                headSha = hash,
                detailsUrl = Just $ "https://" <> host <> "/build/" <> tshow bid,
                externalId = Just $ tshow bid,
                status = GitHub.Queued,
                conclusion = Nothing,
                startedAt = Nothing,
                completedAt = Nothing,
                output = Nothing
              }
  (Hydra.BuildStarted bid) -> do
    [(proj, name, flake, job, desc, starttime)] <- query conn ("select j.project, j.name, e.flake, b.job, b.description, b.starttime" <> sqlFromBuild) (Only bid)
    Text.putStrLn $ "Build Started (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
    steps <- query conn ("SELECT status FROM buildsteps WHERE build = ? ORDER BY stepnr DESC LIMIT 2") (Only bid) :: IO [(Only (Maybe Int))]
    let prevStepStatus
          | length steps >= 2 = (\(Only statusInt) -> statusInt <&> toEnum) $ steps !! 1
          | otherwise = Nothing
    whenStatusOrJob Nothing prevStepStatus job $ withGithubFlake flake $ \owner repo hash ->
      pure $
        singleton $
          GitHub.CheckRun owner repo $
            GitHub.CheckRunPayload
              { name = "ci/hydra-build:" <> job,
                headSha = hash,
                detailsUrl = Just $ "https://" <> host <> "/build/" <> tshow bid,
                externalId = Just $ tshow bid,
                status = GitHub.InProgress,
                conclusion = Nothing,
                -- apparently hydra may send the notification before actually starting the build... got 9 seconds difference when testing!
                startedAt = (starttime :: Maybe Int) >>= Just . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral,
                completedAt = Nothing,
                output = Nothing
              }

  -- note; buildstatus is only != NULL for Finished, Queued and Started leave it as NULL.
  (Hydra.BuildFinished bid depBids) -> do
    [(proj, name, flake, job, desc, finished, status)] <- query conn ("select j.project, j.name, e.flake, b.job, b.description, b.finished, b.buildstatus" <> sqlFromBuild) (Only bid)
    Text.putStrLn $ "Build Finished (" <> tshow bid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> (job :: Text) <> "(" <> maybe "" id (desc :: Maybe Text) <> ")" <> " " <> tshow (parseGitHubFlakeURI flake)
    withGithubFlake flake $ \owner repo hash -> do
      checkRun <- handleBuildDone bid job status (finished == (1 :: Int)) owner repo hash
      depCheckRuns <-
        sequence $
          (if toEnum status /= Hydra.Succeeded then depBids else []) <&> \depBid -> do
            [(depJob, depStatus, depFinished)] <- query conn "SELECT job, buildstatus, finished FROM builds WHERE id = ?" (Only depBid)
            handleBuildDone depBid depJob depStatus (depFinished == (1 :: Int)) owner repo hash
      return $ checkRun ++ concat depCheckRuns
  where
    catchJustPredicate ee
      | Just (_ :: Async.AsyncCancelled) <- fromException ee = Nothing
      | otherwise = Just ee

    handler :: Hydra.Notification -> SomeException -> IO [GitHub.CheckRun]
    handler n ex = print ("ERROR: " ++ show n ++ " triggert exception " ++ displayException ex) >> pure ([] :: [GitHub.CheckRun])

    sqlFromBuild = " from builds b JOIN jobsets j on b.jobset_id = j.id JOIN jobsetevalmembers m on m.build = b.id JOIN jobsetevals e on e.id = m.eval where b.id = ? order by e.id desc fetch first row only"

    handleEvalDone :: Hydra.JobSetId -> Hydra.EvalId -> Text -> IO [GitHub.CheckRun]
    handleEvalDone jid eid eventName = do
      [(proj, name, flake, errmsg, fetcherrmsg)] <- query conn "select project, name, flake, errormsg, fetcherrormsg from jobsets where id = ?" (Only jid)
      [(flake', timestamp, checkouttime, evaltime)] <- query conn "select flake, timestamp, checkouttime, evaltime from jobsetevals where id = ?" (Only eid) :: IO [(Text, Int, Int, Int)]
      Text.putStrLn $ "Eval " <> eventName <> " (" <> tshow jid <> ", " <> tshow eid <> "): " <> (proj :: Text) <> ":" <> (name :: Text) <> " " <> flake <> " eval for: " <> flake'
      withGithubFlake flake' $ \owner repo hash -> do
        let startedAt = posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral timestamp
            fetchCompletedAt = addUTCTime (fromIntegral checkouttime) startedAt
            evalCompletedAt = addUTCTime (fromIntegral evaltime) fetchCompletedAt
            summary = mkEvalDurationSummary checkouttime (if isNothing fetcherrmsg then Just evaltime else Nothing)
        evalStatuses <- pure $ case (errmsg, fetcherrmsg) :: (Maybe Text, Maybe Text) of
          (Just err, _)
            | not (Text.null err) ->
                ( singleton $
                    GitHub.CheckRun owner repo $
                      GitHub.CheckRunPayload
                        { name = "ci/eval",
                          headSha = hash,
                          detailsUrl = Just $ "https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors",
                          externalId = Just $ tshow eid,
                          status = GitHub.Completed,
                          conclusion = Just GitHub.Failure,
                          startedAt = Just startedAt,
                          completedAt = Just evalCompletedAt,
                          output =
                            Just $
                              GitHub.CheckRunOutput
                                { title = "Evaluation has errors",
                                  summary = summary,
                                  text = mkEvalErrorSummary err
                                }
                        }
                )
                  -- Creates a failed check run for each job that failed to evaluate.
                  -- This is temporarily disabled (by simply passing an empty string)
                  -- because there is no way to get rid of these later when the eval
                  -- succeeds on a retry, confusing everyone.
                  ++ ( (parseFailedJobEvals {- err -} "") <&> \(job, msg) ->
                         GitHub.CheckRun owner repo $
                           GitHub.CheckRunPayload
                             { name = "ci/eval:" <> job,
                               headSha = hash,
                               detailsUrl = Just $ "https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors",
                               externalId = Just $ tshow eid,
                               status = GitHub.Completed,
                               conclusion = Just GitHub.Failure,
                               startedAt = Just startedAt,
                               completedAt = Just evalCompletedAt,
                               output =
                                 Just $
                                   GitHub.CheckRunOutput
                                     { title = "Evaluation failed",
                                       summary = summary,
                                       text = mkEvalErrorSummary msg
                                     }
                             }
                     )
          (_, Just err)
            | not (Text.null err) ->
                singleton $
                  GitHub.CheckRun owner repo $
                    GitHub.CheckRunPayload
                      { name = "ci/eval",
                        headSha = hash,
                        detailsUrl = Just $ "https://" <> host <> "/eval/" <> tshow eid <> "#tabs-errors",
                        externalId = Just $ tshow eid,
                        status = GitHub.Completed,
                        conclusion = Just GitHub.Failure,
                        startedAt = Just startedAt,
                        completedAt = Just fetchCompletedAt,
                        output =
                          Just $
                            GitHub.CheckRunOutput
                              { title = "Failed to fetch",
                                summary = summary,
                                text = mkFetchErrorSummary err
                              }
                      }
          _ ->
            singleton $
              GitHub.CheckRun owner repo $
                GitHub.CheckRunPayload
                  { name = "ci/eval",
                    headSha = hash,
                    detailsUrl = Just $ "https://" <> host <> "/eval/" <> tshow eid,
                    externalId = Just $ tshow eid,
                    status = GitHub.Completed,
                    conclusion = Just GitHub.Success,
                    startedAt = Just startedAt,
                    completedAt = Just evalCompletedAt,
                    output =
                      Just $
                        GitHub.CheckRunOutput
                          { title = "Evaluation succeeded",
                            summary = summary,
                            text = Nothing
                          }
                  }
        -- If this evaluation has builds (is not identical to a previous one), this selects no rows.
        -- Otherwise this selects all builds of the latest previous evaluation that differed from its predecessor.
        -- We then submit a status for each of these builds to the jobset's flake URL (the current one).
        -- This is necessary because `(cached_)?build_finished` notifications are not sent by Hydra
        -- when an evaluation is identical to its predecessor / has no builds.
        rows <-
          query
            conn
            "\
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
            \ "
            [eid, jid, eid] ::
            IO [(Int, Text, Int)]
        buildStatuses <- sequence $ rows <&> \(bid, job, status) -> handleBuildDone bid job status True owner repo hash
        pure $ evalStatuses ++ concat buildStatuses

    handleBuildDone :: Hydra.BuildId -> Text -> Int -> Bool -> Text -> Text -> Text -> IO [GitHub.CheckRun]
    handleBuildDone bid job status finished owner repo hash = do
      let buildStatus = toEnum status
      let ghCheckRunConclusion
            | finished = toCheckRunConclusion buildStatus
            | otherwise = GitHub.Failure
      steps <- query conn ("SELECT stepnr, drvpath, status FROM buildsteps WHERE build = ? ORDER BY stepnr DESC") (Only bid) :: IO [(Int, String, Maybe Int)]
      let prevStepStatus
            | length steps >= 2 = (\(_, _, statusInt) -> statusInt <&> toEnum) $ steps !! 1
            | otherwise = Nothing
      whenStatusOrJob (Just ghCheckRunConclusion) prevStepStatus job $ do
        buildTimes <- getBuildTimes bid
        let failedSteps = filter (\(_, _, statusInt) -> maybe False (/= Hydra.Succeeded) $ statusInt <&> toEnum) steps
        failedStepLogs <-
          sequence $
            failedSteps <&> \(stepnr, drvpath, _) -> do
              let drvName = takeFileName drvpath
                  bucketed = take 2 drvName </> drop 2 drvName
                  path = stateDir </> "build-logs" </> bucketed
              logs <- catchIOError (readFile path >>= return . Just) $ \ioe ->
                if not . isDoesNotExistErrorType . ioeGetErrorType $ ioe
                  then ioError ioe
                  else catchIOError (BSLw.readFile (path <.> "bz2") >>= return . Just . cs . BZip.decompress) $ \ioe2 ->
                    if not . isDoesNotExistErrorType . ioeGetErrorType $ ioe2
                      then ioError ioe2
                      else return Nothing
              return (stepnr, drvpath, logs)
        output <- query conn ("SELECT path FROM buildoutputs WHERE name = 'out' and build = ? LIMIT 1") (Only bid) :: IO [(Only Text)]
        pure $
          singleton $
            GitHub.CheckRun owner repo $
              GitHub.CheckRunPayload
                { name = "ci/hydra-build:" <> job,
                  headSha = hash,
                  detailsUrl = Just $ "https://" <> host <> "/build/" <> tshow bid,
                  externalId = Just $ tshow bid,
                  status = GitHub.Completed,
                  conclusion = Just ghCheckRunConclusion,
                  startedAt = buildTimes >>= Just . fst,
                  completedAt = buildTimes >>= Just . snd,
                  output =
                    Just $
                      GitHub.CheckRunOutput
                        { title = tshow buildStatus,
                          summary =
                            if buildStatus == Hydra.Succeeded
                              then -- TODO: This is only the "out" path, maybe we do want to put _all_ paths in here JSON encoded?
                              -- The idea is that on successful builds, we can grab the nix paths (if needed) directly out of the
                              -- github status. And use it for nix-store -r, or similar.
                                Text.intercalate ", " (fmap fromOnly output)
                              else tshow (length failedSteps) <> " failed steps",
                          text =
                            if buildStatus == Hydra.Succeeded
                              then Nothing
                              else -- TODO: We should include some meta information about the build. Similar to what hydra provides on the
                              -- build page.
                                let limit = 65535
                                    maxLines = foldr' max 0 $ failedStepLogs <&> \(_, _, logs) -> maybe 0 length logs
                                    indentPrefix = cs $ indentLine "" :: String
                                    stepLogsLines = failedStepLogs <&> \(stepnr, drvpath, logs) -> (stepnr, drvpath, logs >>= Just . lines)
                                 in binarySearch 0 limit $ \numLines ->
                                      let parts =
                                            singleton "# Failed Steps\n\n"
                                              ++ intercalate
                                                (singleton "\n")
                                                ( stepLogsLines <&> \(stepnr, drvpath, logLines) ->
                                                    [ "## Step ",
                                                      show stepnr,
                                                      "\n\n",
                                                      -- making code blocks by indenting instead of triple backticks so they cannot be escaped
                                                      "### Derivation\n\n",
                                                      indentPrefix,
                                                      drvpath,
                                                      "\n\n",
                                                      "### Log\n\n"
                                                    ]
                                                      ++ (if numLines < maxLines then ["Last ", show numLines, " lines:\n\n"] else [])
                                                      ++ maybe
                                                        (singleton "*Not available.*\n")
                                                        ((concatMap (\l -> [indentPrefix, l, "\n"])) . (takeEnd numLines))
                                                        logLines
                                                )
                                          totalLength = foldr' ((+) . length) 0 parts
                                       in ( totalLength < limit && numLines < maxLines,
                                            if totalLength > limit then Nothing else Just . cs $ mconcat parts
                                          )
                        }
                }

    -- Given an evaluation's error message, returns the jobs that could not be evaluated and their excerpt from the error message.
    parseFailedJobEvals :: Text -> [(Text, Text)]
    parseFailedJobEvals errormsg =
      (internal errormsg) <&> \(_, job, msg) -> (job, msg)
      where
        internal :: Text -> [(Text, Text, Text)]
        internal rest =
          case rest =~ ("^in job ‘([^’]*)’:$" :: Text) :: (Text, Text, Text, [Text]) of
            (before, _, after, (job : _)) ->
              let next = internal after
                  msg = case next of
                    ((m, _, _) : _) -> m
                    [] -> after
               in singleton (before, job, msg) ++ next
            _ -> []

    getBuildTimes :: Hydra.BuildId -> IO (Maybe (UTCTime, UTCTime))
    getBuildTimes bid = do
      rows <-
        query
          conn
          "\
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
          \SELECT selected_build.starttime, selected_build.stoptime      \
          \FROM selected_build                                           \
          \ "
          (Only bid) ::
          IO [(Int, Int)]
      pure $ case rows of
        [(starttime, stoptime)] ->
          Just
            ( posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral starttime,
              posixSecondsToUTCTime . secondsToNominalDiffTime $ fromIntegral stoptime
            )
        _ -> Nothing

    mkEvalDurationSummary :: Int -> Maybe Int -> Text
    mkEvalDurationSummary checkouttime evaltime =
      "Checkout took "
        <> humanReadableDuration (fromIntegral checkouttime * oneSecond)
        <> "."
        <> maybe mempty (\j -> "\nEvaluation took " <> humanReadableDuration (fromIntegral j * oneSecond) <> ".") evaltime

    mkFetchErrorSummary :: Text -> Maybe Text
    mkFetchErrorSummary fetcherrmsg =
      let limit = 65535
          fetcherrmsgLines = Text.lines fetcherrmsg
          maxLines = length fetcherrmsgLines
          indentPrefix = indentLine ""
       in binarySearch 0 limit $ \numLines ->
            let parts =
                  singleton "Fetch error:\n\n"
                    ++ (if numLines < maxLines then ["Last ", tshow numLines, " lines:\n\n"] else [])
                    -- making code blocks by indenting instead of triple backticks so they cannot be escaped
                    ++ (concatMap (\l -> [indentPrefix, l, "\n"]) $ takeEnd numLines fetcherrmsgLines)
                totalLength = foldr' ((+) . Text.length) 0 parts
             in ( totalLength < limit && numLines < maxLines,
                  if totalLength > limit then Nothing else Just . cs $ Text.concat parts
                )

    mkEvalErrorSummary :: Text -> Maybe Text
    mkEvalErrorSummary errmsg =
      let limit = 65535
          errmsgLines = Text.lines errmsg
          maxLines = length errmsgLines
          indentPrefix = indentLine ""
       in binarySearch 0 limit $ \numLines ->
            let parts =
                  singleton "Evaluation error:\n\n"
                    ++ (if numLines < maxLines then ["Last ", tshow numLines, " lines:\n\n"] else [])
                    -- making code blocks by indenting instead of triple backticks so they cannot be escaped
                    ++ (concatMap (\l -> [indentPrefix, l, "\n"]) $ takeEnd numLines errmsgLines)
                totalLength = foldr' ((+) . Text.length) 0 parts
             in ( totalLength < limit && numLines < maxLines,
                  if totalLength > limit then Nothing else Just . cs $ Text.concat parts
                )

statusHandler :: Text -> BS.ByteString -> IO [(String, GitHub.TokenLease)] -> GitHub.CheckRun -> IO (Either SomeException Value)
statusHandler ghEndpointUrl ghUserAgent getGitHubToken checkRun = do
  Text.putStrLn $ "SENDING [" <> checkRun.owner <> "/" <> checkRun.repo <> "/" <> checkRun.payload.headSha <> "] " <> checkRun.payload.name <> ":" <> Text.pack (show checkRun.payload.status)

  -- putStrLn $ "Obtain GitHub token..."
  ghToken <- getGitHubToken
  -- putStrLn $ "GitHub Token: " <> show ghToken

  let token' = case [tok.token | (owner, tok) <- ghToken, Text.pack owner == checkRun.owner] of
        [t] -> Just t
        _ -> throw (toException $ userError ("No GitHub token found for " <> Text.unpack checkRun.owner))
  let githubSettings =
        GitHubSettings
          { token = token',
            userAgent = ghUserAgent,
            apiVersion = GitHub.gitHubApiVersion
          }
  try . liftIO . GitHub.runGitHubRestT githubSettings ghEndpointUrl $
    queryGitHub
      GHEndpoint
        { method = POST,
          endpoint = "/repos/:owner/:repo/check-runs",
          endpointVals =
            [ "owner" := checkRun.owner,
              "repo" := checkRun.repo
            ],
          ghData = GitHub.toKeyValue checkRun.payload
        } ::
    IO (Either SomeException Value)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  host <- maybe "localhost" id <$> lookupEnv "HYDRA_HOST"
  db <- maybe "localhost" id <$> lookupEnv "HYDRA_DB"
  user <- maybe mempty id <$> lookupEnv "HYDRA_USER"
  pass <- maybe mempty id <$> lookupEnv "HYDRA_PASS"
  stateDir <- getEnv "HYDRA_STATE_DIR"
  ghEndpointUrl <- Text.pack . maybe "https://api.github.com" id <$> lookupEnv "GITHUB_ENDPOINT_URL"

  ghUserAgent <- maybe "hydra-github-bridge" cs <$> lookupEnv "GITHUB_USER_AGENT"

  let fetchGitHubTokens :: IO [(String, GitHub.TokenLease)]
      fetchGitHubTokens = do
        ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
        ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"

        putStrLn "Fetching GitHub App installations..."
        ghAppInstalls <- GitHub.fetchInstallations ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent
        putStrLn $ "Found " <> show (length ghAppInstalls) <> " installations"
        forM_ ghAppInstalls $ \(owner, installId) -> do
          Text.putStrLn $ "\t- " <> owner <> " (" <> Text.pack (show installId) <> ")"

        forM ghAppInstalls $ \(owner, installId) -> do
          lease <- GitHub.fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent installId
          Text.putStrLn $ "Fetched new GitHub App installation token valid for " <> owner <> " until " <> Text.pack (show lease.expiry)
          return (Text.unpack owner, lease)

  -- ghTokens is basically [(String, Token)]
  ghTokens <- fetchGitHubTokens >>= newIORef
  let getValidGitHubToken =
        let buffer = 5 :: NominalDiffTime
         in GitHub.getValidToken buffer ghTokens $ \owner -> do
              putStrLn $ "GitHub token expired or will expire within the next " <> show buffer <> ", fetching a new one..."
              ghAppId <- getEnv "GITHUB_APP_ID" >>= return . read
              ghAppInstallIds <- getEnv "GITHUB_APP_INSTALL_IDS" >>= return . read @[(String, Int)]
              let ghAppInstallId = fmap snd . find ((owner ==) . fst) $ ghAppInstallIds
              ghAppKeyFile <- getEnv "GITHUB_APP_KEY_FILE"
              maybe
                (error $ "No configured GitHub App Installation ID " <> owner)
                (GitHub.fetchAppInstallationToken ghEndpointUrl ghAppId ghAppKeyFile ghUserAgent)
                ghAppInstallId

  let numWorkers = 10 -- default number of workers
  let statusHandlers =
        Async.replicateConcurrently_
          numWorkers
          ( withConnect (ConnectInfo db 5432 user pass "hydra") $ \conn -> forever $ do
              let processStatuses = withTransaction conn $ do
                    rows <-
                      query_
                        conn
                        ( fromString $
                            unwords
                              [ "WITH AllStatus AS (",
                                "  SELECT s.id, MAX(p.id) AS mostRecentPaylodID, s.owner, s.repo, s.headSha, s.name",
                                "  FROM github_status s",
                                "  JOIN github_status_payload p ON s.id = p.status_id",
                                "  GROUP BY s.id, s.owner, s.repo, s.headSha, s.name",
                                ")",
                                "SELECT p.id, g.owner, g.repo, p.payload",
                                "FROM AllStatus g",
                                "JOIN github_status_payload p ON g.id = p.status_id",
                                "WHERE p.id = g.mostRecentPaylodID AND p.sent IS NULL AND p.tries < 5",
                                "ORDER BY",
                                "  CASE WHEN g.name = 'ci/eval' THEN 0 ELSE 1 END,", -- Prioritize 'ci/eval'
                                "  p.id ASC",
                                "LIMIT 1",
                                "FOR UPDATE SKIP LOCKED"
                                -- "SELECT p.id, g.owner, g.repo, p.payload"
                                --                              , "FROM github_status_payload p"
                                --                              , "JOIN github_status g ON g.id = p.status_id"
                                --                              , "WHERE p.sent IS NULL AND p.tries < 5"
                                --                              , "ORDER BY p.created ASC"
                                --                              , "FOR UPDATE OF p, g SKIP LOCKED"
                              ]
                        )
                    -- by sorting on p.created, we can assume that "newer" statuses for the same owner/repo/sha/name, are
                    -- returned last. This is only applicable if we find multiple rows. If we find only a single row this
                    -- is irrelevant. However for multiple rows, the last item will be the most recent status and we can just
                    -- send that to GitHub, and skip all prior ones. They usually go through queued -> in_progress -> completed.
                    -- If we already have the status for completed, we don't need to send queued, and in_progress. This would
                    -- just eat two requests, which for many concurrent status-updates can lead to a lot of requests, and thus
                    -- us running into rate-limits.
                    case (reverse rows) of
                      (id', owner, repo, payload) : _ -> do
                        let payload' = case fromJSON payload of
                              Aeson.Success p -> p
                              Aeson.Error e -> error e
                        eres <- statusHandler ghEndpointUrl ghUserAgent getValidGitHubToken (GitHub.CheckRun owner repo payload')
                        case eres of
                          Left ex
                            | Just (HTTP.HttpExceptionRequest _req (HTTP.StatusCodeException resp _)) <- fromException ex,
                              Just n <- read . BS.unpack <$> lookup "Retry-After" (HTTP.responseHeaders resp) -> do
                                putStrLn $ "Hit the rate-limit: Retrying in " <> show n <> " seconds..."
                                threadDelay (n * 1000000)
                                return ()
                          Left ex
                            | Just (HTTP.HttpExceptionRequest _req (HTTP.StatusCodeException resp _)) <- fromException ex,
                              Just remaining <- read . BS.unpack <$> lookup "X-RateLimit-Remaining" (HTTP.responseHeaders resp),
                              remaining == (0 :: Int),
                              Just utc_epoch_offset <- read . BS.unpack <$> lookup "X-RateLimit-Reset" (HTTP.responseHeaders resp) -> do
                                current_utc_epoch <- round <$> getPOSIXTime
                                putStrLn $ "Hit the rate-limit: Retrying in " <> show (utc_epoch_offset - current_utc_epoch) <> " seconds..."
                                threadDelay ((utc_epoch_offset - current_utc_epoch) * 1000000)
                                return ()
                          Left ex
                            | Just (HTTP.HttpExceptionRequest _req HTTP.ConnectionTimeout) <- fromException ex -> do
                                putStrLn "Connection timeout, retrying..."
                                return ()
                          Left e -> do
                            Text.putStrLn $ "FAIL [" <> owner <> "/" <> repo <> "/" <> payload'.headSha <> "] " <> payload'.name <> ":" <> Text.pack (show payload'.status) <> ": " <> Text.pack (show e)
                            _ <- execute conn "UPDATE github_status_payload SET tries = tries + 1 WHERE id = ?" (Only id' :: Only Int)
                            return ()
                          Right _res -> do
                            Text.putStrLn $ "SENT [" <> owner <> "/" <> repo <> "/" <> payload'.headSha <> "] " <> payload'.name <> ":" <> Text.pack (show payload'.status)
                            -- mark all statuses as sent; previous statueses are overridded anyway.
                            forM_ rows $ \(_id, o, r, p) -> do
                              case fromJSON p of
                                Aeson.Success (p' :: GitHub.CheckRunPayload) ->
                                  Text.putStrLn $ "MARK [" <> o <> "/" <> r <> "/" <> p'.headSha <> "] " <> p'.name <> ":" <> Text.pack (show p'.status) <> " SENT"
                                Aeson.Error e -> error e
                            _ <- execute conn "UPDATE github_status_payload SET sent = NOW() WHERE id IN ?" (Only (In [id'' | (id'', _, _, _) <- rows] :: In [Int]))
                            -- BSL.putStrLn $ "<- " <> encode res
                            return ()
                        return True
                      _ -> return False
              _ <- execute_ conn "LISTEN github_status"
              let loop = do
                    executed <- processStatuses
                    unless executed $ void $ getNotification conn
                    when executed loop
              loop
          )
  let notificationWatcher = withConnect (ConnectInfo db 5432 user pass "hydra") $ \conn -> do
        _ <- execute_ conn "LISTEN eval_started" -- (opaque id, jobset id)
        _ <- execute_ conn "LISTEN eval_added" -- (opaque id, jobset id, eval record id)
        _ <- execute_ conn "LISTEN eval_cached" -- (opaque id, jobset id, prev identical eval id)
        _ <- execute_ conn "LISTEN eval_failed" -- (opaque id, jobset id)
        _ <- execute_ conn "LISTEN build_queued" -- (build id)
        _ <- execute_ conn "LISTEN cached_build_queued" -- (eval id, build id)
        _ <- execute_ conn "LISTEN build_started" -- (build id)
        _ <- execute_ conn "LISTEN build_finished" -- (build id, dependent build ids...)
        _ <- execute_ conn "LISTEN cached_build_finished" -- (eval id, build id)
        forever $ do
          putStrLn "Waiting for notification..."
          note <- toHydraNotification . traceShowId <$> getNotification conn
          statuses <- handleHydraNotification conn (cs host) stateDir note
          forM_ statuses $
            ( \(GitHub.CheckRun owner repo payload) -> do
                Text.putStrLn $ "QUEUEING [" <> owner <> "/" <> repo <> "/" <> payload.headSha <> "] " <> payload.name <> ":" <> Text.pack (show payload.status)
                [Only _id'] <-
                  query
                    conn
                    "with status_upsert as (insert into github_status (owner, repo, headSha, name) values (?, ?, ?, ?) on conflict (owner, repo, headSha, name) do update set name = excluded.name returning id) insert into github_status_payload (status_id, payload) select (select id from status_upsert), ? returning id"
                    (owner, repo, payload.headSha, payload.name, (toJSON payload)) ::
                    IO [Only Int]
                execute_ conn "NOTIFY github_status"
            )
  eres <- Async.race statusHandlers notificationWatcher
  either (const . putStrLn $ "statusHandler exited") (const . putStrLn $ "withConnect exited") eres
