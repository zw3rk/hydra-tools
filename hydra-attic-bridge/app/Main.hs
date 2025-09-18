{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent
import Control.Exception
  ( SomeException,
    catch,
    displayException,
  )
import Control.Monad
import Control.Monad (unless, when)
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
  ( callProcess,
    readCreateProcessWithExitCode,
    shell,
  )

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
            execute conn "UPDATE DrvpathsToUpload SET last = NOW() + interval '5 minutes', tries = tries + 1 WHERE drvpath = ?;" (Only drvPath)
            pure True
          ExitSuccess -> do
            execute conn "DELETE FROM DrvpathsToUpload WHERE drvpath = ?;" (Only drvPath)
            pure True
      _ -> pure False
  when more (processDrvPath conn cache)

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

  (exitCode, output, errOutput) <-
    readCreateProcessWithExitCode
      (shell $ "attic login local " ++ attic ++ " " ++ token)
      ""

  case exitCode of
    ExitFailure code -> do
      putStrLn $ "Ran: attic login local " ++ attic ++ " <token>"
      putStrLn $ "Login failed with exit code " ++ show code
      unless (null errOutput) $ putStrLn $ "Error output:\n" ++ errOutput
    _ -> withConnect (ConnectInfo host 5432 user pass "hydra") $ \conn -> do
      _ <- execute_ conn "LISTEN step_finished" -- (build id, step id, logpath)
      -- process all pre-existing drv paths we might have missed.
      _ <- processDrvPath conn cache
      -- use the notificaitons as triggers to run the processDrvPath
      forever $ getNotification conn >> processDrvPath conn cache
