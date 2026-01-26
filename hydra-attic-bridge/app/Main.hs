{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception
  ( SomeException,
    catch,
    displayException,
  )
import Control.Monad (forever, replicateM_, unless, void, when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import GHC.Generics
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

processDrvPath :: Connection -> String -> IO ()
processDrvPath conn cache = do
  more <- withTransaction conn $ do
    result <- query_ conn "SELECT drvpath FROM DrvpathsToUpload WHERE last < NOW() FOR UPDATE SKIP LOCKED LIMIT 1;"
    case result of
      [Only drvPath] -> do
        (exitCode, _, errOutput) <- readCreateProcessWithExitCode (shell $ "attic push " ++ cache ++ " " ++ drvPath) ""
        case exitCode of
          ExitFailure code -> do
            putStrLn $ "Ran: attic push " ++ cache ++ " " ++ drvPath
            putStrLn $ "Attic push failed with exit code " ++ show code
            unless (null errOutput) $ putStrLn $ "Error output:\n" ++ errOutput
            -- Slow down retries until we only do them monthly (8640 * 5 minutes is 30 days)
            execute conn "UPDATE DrvpathsToUpload SET last = NOW() + (interval '5 minutes' * least(8640, 1.5 ^ tries)), tries = tries + 1 WHERE drvpath = ?;" (Only drvPath)
            pure True
          ExitSuccess -> do
            execute conn "DELETE FROM DrvpathsToUpload WHERE drvpath = ?;" (Only drvPath)
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

-- main ()
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
