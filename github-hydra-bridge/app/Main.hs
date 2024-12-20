{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Concurrent       (forkIO)
import           Control.Monad            (void)
import qualified Data.ByteString.Char8    as C8
import qualified Data.Text                as Text
import           DiskStore                (DiskStoreConfig (..))
import qualified DsQueue
import           Lib
import           Network.Wai.Handler.Warp (run)
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
  mStateDir <- lookupEnv "HYDRA_STATE_DIR"
  putStrLn $ "Server is starting on port " ++ show port
  putStrLn $ maybe "No $HYDRA_STATE_DIR specified." ("$HYDRA_STATE_DIR is: " ++) mStateDir
  queue <- DsQueue.new (fmap (\sd -> DiskStoreConfig sd "github-hydra-bridge/queue" 10) mStateDir)
  env <- hydraClientEnv host user pass
  void . forkIO $ hydraClient host env queue
  run port (app env queue (gitHubKey key))
