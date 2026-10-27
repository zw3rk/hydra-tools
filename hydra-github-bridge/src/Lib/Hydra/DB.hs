{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Hydra.DB
  ( Command (..),
    readCommand,
    writeCommand,
    readBuildLog,
  )
where

import Codec.Compression.BZip (decompressErr)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LByteString
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query_)
import GHC.Generics (Generic)
import Lib.Hydra.Client (HydraJobset)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName, (<.>), (</>))

-- The following table exists in the database
--
-- CREATE TABLE IF NOT EXISTS github_commands (
--     id SERIAL PRIMARY KEY,
--     command JSONB NOT NULL,
--     created TIMESTAMP DEFAULT NOW(),
--     processed TIMESTAMP DEFAULT NULL
-- );
data Command
  = UpdateJobset Text Text Text HydraJobset -- only update it, never create
  | CreateOrUpdateJobset Text Text Text HydraJobset -- create or update.
  | DeleteJobset Text Text
  | EvaluateJobset Text Text Bool
  | RestartBuild Int
  deriving (Eq, Generic, Read, Show)

instance ToJSON Command

instance FromJSON Command

readCommand :: Connection -> IO Command
readCommand conn = do
  query_ conn "SELECT id, command FROM github_commands WHERE processed IS NULL ORDER BY created LIMIT 1" >>= \case
    [] -> threadDelay 10_000_000 >> readCommand conn -- 10 sec" \
    [(_id, cmd)] -> do
      void $ execute conn "UPDATE github_commands SET processed = NOW() WHERE id = ?" (Only _id :: Only Int)
      case (Aeson.fromJSON cmd) of
        Aeson.Error e -> error $ show cmd ++ " readCommand: " ++ e
        Aeson.Success x -> return x
    x -> error $ "readCommand: " ++ show x

writeCommand :: Connection -> Command -> IO ()
writeCommand conn cmd = do
  void $ execute conn "INSERT INTO github_commands (command) VALUES (?)" (Only (Aeson.toJSON cmd))

-- Adapted from IOG 952c575: finish decompression and decoding inside IO.
readBuildLog :: FilePath -> FilePath -> IO (Maybe Text)
readBuildLog hydraStateDir drvPath = do
  let drvName = takeFileName drvPath
      -- The first two characters of the derivation hash is the directory name and the
      -- rest is the file name
      bucketed = take 2 drvName </> drop 2 drvName
      path = hydraStateDir </> "build-logs" </> bucketed

  -- Attempt to read the drv file
  drvExists <- doesFileExist path
  if drvExists
    then ByteString.readFile path >>= tryDecode
    else do
      -- drv file does not exist, add ".bz2" to the end and try to read it again
      let bz2Path = path <.> "bz2"
      bz2Exists <- doesFileExist bz2Path
      if bz2Exists
        then LByteString.readFile bz2Path >>= tryDecompress >>= tryDecode
        else pure Nothing
  where
    tryDecode = either throwIO (pure . Just) . decodeUtf8'
    tryDecompress = either throwIO (pure . ByteString.toStrict) . decompressErr
