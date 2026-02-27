{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception
  ( SomeException,
    catch,
    displayException,
  )
import Control.Monad (forever, replicateM_, unless, void, when)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
    stdin,
    stdout,
  )
import System.Process
  ( readCreateProcessWithExitCode,
    shell,
  )
import Text.Read (readMaybe)

-- | Maximum number of retries before permanently discarding an entry.
-- At 1.5^20 * 5 min ≈ 16 hours for the last retry.  Entries beyond
-- this threshold are dead weight that will never succeed (the store
-- path was never fetched back from the remote builder, or the
-- derivation is genuinely broken).
maxRetries :: Int
maxRetries = 20

processDrvPath :: Connection -> String -> IO ()
processDrvPath conn cache = do
  more <- withTransaction conn $ do
    -- Select by primary key so that DELETE/UPDATE below only touches
    -- one row, eliminating deadlocks from duplicate drvpath entries.
    result <- query conn
      "SELECT id, drvpath FROM DrvpathsToUpload WHERE last < NOW() AND tries < ? FOR UPDATE SKIP LOCKED LIMIT 1;"
      (Only maxRetries)
    case result of
      [(rowId, drvPath) :: (Int, String)] -> do
        -- Check that the store path actually exists locally before
        -- shelling out to attic.  Remote-built outputs may not have
        -- been fetched back to the Hydra server.
        (probeExit, _, _) <- readCreateProcessWithExitCode (shell $ "nix path-info " ++ drvPath ++ " >/dev/null 2>&1") ""
        case probeExit of
          ExitFailure _ -> do
            -- Path doesn't exist locally — backoff so we retry later
            -- (the queue-runner may still be uploading it from a
            -- remote builder).
            putStrLn $ "Path not valid locally, deferring: " ++ drvPath
            _ <- execute conn "UPDATE DrvpathsToUpload SET last = NOW() + interval '2 minutes', tries = tries + 1 WHERE id = ?;" (Only rowId)
            pure True
          ExitSuccess -> do
            (exitCode, _, errOutput) <- readCreateProcessWithExitCode (shell $ "attic push " ++ cache ++ " " ++ drvPath) ""
            case exitCode of
              ExitFailure code -> do
                putStrLn $ "Ran: attic push " ++ cache ++ " " ++ drvPath
                putStrLn $ "Attic push failed with exit code " ++ show code
                unless (null errOutput) $ putStrLn $ "Error output:\n" ++ errOutput
                -- Exponential backoff: 5 min * 1.5^tries, capped at 30 days.
                _ <- execute conn "UPDATE DrvpathsToUpload SET last = NOW() + (interval '5 minutes' * least(8640, 1.5 ^ tries)), tries = tries + 1 WHERE id = ?;" (Only rowId)
                pure True
              ExitSuccess -> do
                _ <- execute conn "DELETE FROM DrvpathsToUpload WHERE id = ?;" (Only rowId)
                pure True
      _ -> pure False
  when more (processDrvPath conn cache)

workerLoop :: Chan () -> ConnectInfo -> String -> IO ()
workerLoop wakeChan connectInfo cache =
  withConnect connectInfo $ \conn ->
    forever $ do
      -- Block until there is work to process.
      readChan wakeChan
      catch (processDrvPath conn cache) $ \err ->
        putStrLn $ "Worker encountered an error: " ++ displayException (err :: SomeException)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  host <- maybe mempty id <$> lookupEnv "HYDRA_HOST"
  user <- maybe mempty id <$> lookupEnv "HYDRA_USER"
  pass <- maybe mempty id <$> lookupEnv "HYDRA_PASS"
  attic <- maybe "localhost" id <$> lookupEnv "ATTIC_HOST"
  cache <- maybe mempty id <$> lookupEnv "ATTIC_CACHE"
  token <- maybe mempty id <$> lookupEnv "ATTIC_TOKEN"
  workersEnv <- lookupEnv "ATTIC_WORKERS"
  let workerCount =
        case workersEnv >>= readMaybe of
          Just n | n > 0 -> n
          _ -> 1

  (exitCode, _, errOutput) <-
    readCreateProcessWithExitCode
      (shell $ "attic login local " ++ attic ++ " " ++ token)
      ""

  case exitCode of
    ExitFailure code -> do
      putStrLn $ "Ran: attic login local " ++ attic ++ " <token>"
      putStrLn $ "Login failed with exit code " ++ show code
      unless (null errOutput) $ putStrLn $ "Error output:\n" ++ errOutput
    _ -> do
      let connectInfo = ConnectInfo host 5432 user pass "hydra"
      wakeChan <- newChan
      putStrLn $ "Starting " ++ show workerCount ++ " worker thread(s)"
      replicateM_ workerCount $
        void $
          forkIO $
            workerLoop wakeChan connectInfo cache
      withConnect connectInfo $ \listenConn -> do
        _ <- execute_ listenConn "LISTEN step_finished" -- (build id, step id, logpath)
        -- Kick workers once to process any backlog that exists at startup.
        replicateM_ workerCount $ writeChan wakeChan ()
        -- Trigger workers whenever Hydra notifies us of new entries.
        forever $ do
          _ <- getNotification listenConn
          replicateM_ workerCount $ writeChan wakeChan ()
