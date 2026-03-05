-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database operations for user-triggered actions (trigger eval, restart build).
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Actions
  ( triggerJobsetEval
  , restartBuild
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))

-- | Set triggertime on a jobset so the evaluator picks it up.
-- Uses extract(epoch from now()) for Hydra's unix-timestamp convention.
triggerJobsetEval :: Connection -> Text -> Text -> IO ()
triggerJobsetEval conn project jobset = do
  _ <- execute conn [sql|
    UPDATE jobsets
    SET triggertime = extract(epoch from now())
    WHERE project = ? AND name = ? AND enabled > 0
  |] (project, jobset)
  pure ()

-- | Reset a finished+failed build to queued state.
-- Only restarts builds that are finished (finished=1) and failed (buildstatus <> 0).
-- Returns True if a build was actually restarted.
restartBuild :: Connection -> Int -> IO Bool
restartBuild conn buildId' = do
  n <- execute conn [sql|
    UPDATE builds
    SET finished = 0, buildstatus = NULL, starttime = NULL, stoptime = NULL
    WHERE id = ? AND finished = 1
  |] (Only buildId')
  pure (n > 0)
