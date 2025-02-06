{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import qualified Data.ByteString.Char8    as C8
import qualified Data.Text                as Text
import           Lib
import           Network.Wai.Handler.Warp (run)
import           System.Environment       (lookupEnv)

import           System.IO                (BufferMode (LineBuffering),
                                           hSetBuffering, stderr, stdin, stdout)
import           Control.Concurrent.Async                as Async
import           Database.PostgreSQL.Simple

main :: IO ()
main = do

  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  port <- maybe 8080 read <$> lookupEnv "PORT"
  key <- maybe mempty C8.pack <$> lookupEnv "KEY"
  db <- maybe mempty id <$> lookupEnv "HYDRA_DB"
  db_user <- maybe mempty id <$> lookupEnv "HYDRA_DB_USER"
  db_pass <- maybe mempty id <$> lookupEnv "HYDRA_DB_PASS"
  user <- maybe mempty Text.pack <$> lookupEnv "HYDRA_USER"
  pass <- maybe mempty Text.pack <$> lookupEnv "HYDRA_PASS"
  host <- maybe mempty Text.pack <$> lookupEnv "HYDRA_HOST"
  putStrLn $ "Server is starting on port " ++ show port
  env <- hydraClientEnv host user pass

  eres <- Async.race
    (withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn -> do
      hydraClient env conn)
    (withConnect (ConnectInfo db 5432 db_user db_pass "hydra") $ \conn -> do
      run port (app env conn (gitHubKey key)))
  either (const . putStrLn $ "hydraClient exited") (const . putStrLn $ "app exited") eres
