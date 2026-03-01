-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for builds, build steps, outputs, and products.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Builds
  ( getBuild
  ) where

import Database.PostgreSQL.Simple (Connection, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))

import HydraWeb.Models.Build (Build (..))

-- | Fetch a single build by ID, with project/jobset denormalized via JOIN.
getBuild :: Connection -> Int -> IO (Maybe Build)
getBuild conn buildId' = do
  rows <- query conn [sql|
    SELECT b.id, b.finished, b.timestamp, b.job, b.system,
           b.nixname, b.description, b.drvpath,
           b.buildstatus, b.starttime, b.stoptime,
           b.iscachedbuild, b.size, b.closuresize,
           b.priority, b.globalpriority,
           j.project, j.name
    FROM builds b
    JOIN jobsets j ON b.jobset_id = j.id
    WHERE b.id = ?
  |] (Only buildId')
  case rows of
    [] -> pure Nothing
    ((bid, finished, ts, job, sys, nixname, desc, drv,
      status, start, stop, cached, sz, closure,
      prio, gprio, project, jobset):_) ->
      pure $ Just Build
        { buildId = bid
        , buildFinished = finished
        , buildTimestamp = ts
        , buildJob = job
        , buildSystem = sys
        , buildNixName = nixname
        , buildDescription = desc
        , buildDrvPath = drv
        , buildStatus = status
        , buildStartTime = start
        , buildStopTime = stop
        , buildIsCachedBuild = cached
        , buildSize = sz
        , buildClosureSize = closure
        , buildPriority = prio
        , buildGlobalPriority = gprio
        , buildProject = project
        , buildJobset = jobset
        }
