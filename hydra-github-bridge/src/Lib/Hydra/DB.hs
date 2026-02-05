{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Hydra.DB
  ( Command (..),
    readCommand,
    writeCommand,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query_)
import GHC.Generics (Generic)
import Lib.Hydra.Client (HydraJobset)

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
