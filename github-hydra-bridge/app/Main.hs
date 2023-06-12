{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- import Data.Aeson
-- import Data.Aeson.Schemas

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import GHC.Generics
import Lib
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
-- import Servant
-- import Servant.API.ContentTypes
import System.Environment (lookupEnv)
import Control.Concurrent.STM (newTChan, atomically)
import Control.Concurrent (forkIO)
import qualified Data.Text as Text

import System.IO (hSetBuffering, stdin, stdout, stderr, BufferMode(LineBuffering))

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
  putStrLn $ "Server is starting on port " ++ show port ++ " using test secret " ++ show key
  queue <- atomically $ newTChan
  forkIO $ hydraClient host user pass queue
  run port (app queue (gitHubKey $ pure key))
