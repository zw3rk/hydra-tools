{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Monad            (void)
import qualified Data.ByteString.Char8    as C8
import           Lib
import           Network.Wai.Handler.Warp (run)
import           Control.Concurrent       (forkIO)
import           Control.Concurrent.STM   (atomically, newTChan)
import qualified Data.Text                as Text
import           System.Environment       (lookupEnv)

import           System.IO                (BufferMode (LineBuffering),
                                           hSetBuffering, stderr, stdin, stdout)

main :: IO ()
main = do

  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  port <- maybe 8080 read <$> lookupEnv "PORT"
  key <- maybe mempty C8.pack <$> lookupEnv "KEY"
  user <- maybe mempty Text.pack <$> lookupEnv "HYDRA_USER"
  pass <- maybe mempty Text.pack <$> lookupEnv "HYDRA_PASS"
  host <- maybe mempty Text.pack <$> lookupEnv "HYDRA_HOST"
  putStrLn $ "Server is starting on port " ++ show port
  queue <- atomically $ newTChan
  void . forkIO $ hydraClient host user pass queue
  run port (app queue (gitHubKey key))
