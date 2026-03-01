-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for builds, build steps, outputs, products, metrics, and inputs.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Builds
  ( getBuild
  , getBuildOutputs
  , getBuildProducts
  , getBuildSteps
  , getBuildMetrics
  , getBuildInputs
  , getBuildEvals
  , getConstituents
  , buildsByEval
  , queuedBuilds
  , latestBuildForJob
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.Types ((:.)((:.)))

import HydraWeb.Models.Build

-- | Scan a "list" build row (17 columns via nested tuples).
-- postgresql-simple only has FromRow for tuples up to ~10 elements,
-- so we split into nested tuples using :. .
scanBuildListRow :: ( (Int, Int, Int, Int, Text, Maybe Text, Text, Int, Int)
                    :. (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Text, Maybe Int, Text, Text) )
                 -> Build
scanBuildListRow ( (bid, finished, ts, jobsetId, job, nixName, sys, prio, gprio)
                 :. (start, stop, cached, status, drv, isCurrent, proj, js) ) =
  Build bid finished ts jobsetId job nixName sys prio gprio
        start stop cached status drv isCurrent
        proj js
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

-- | Fetch a single build by ID with all fields (including extended).
-- Uses nested tuples to work around FromRow size limits.
getBuild :: Connection -> Int -> IO (Maybe Build)
getBuild conn buildId' = do
  rows <- query conn [sql|
    SELECT b.id, b.finished, b.timestamp, b.jobset_id, b.job,
           b.nixname, b.system, b.priority, b.globalpriority,
           b.starttime, b.stoptime, b.iscachedbuild, b.buildstatus,
           b.drvpath, b.iscurrent,
           j.project, j.name,
           b.description, b.license, b.homepage, b.maintainers,
           b.size, b.closuresize, b.releasename, b.keep
    FROM builds b
    JOIN jobsets j ON j.id = b.jobset_id
    WHERE b.id = ?
  |] (Only buildId')
  case rows of
    [] -> pure Nothing
    (( (bid, finished, ts, jobsetId, job, nixName, sys, prio, gprio)
     :. (start, stop, cached, status, drv, isCurrent, proj, js)
     :. (desc, lic, hp, maint, sz, closure, release, keep)
     ):_) ->
      pure $ Just Build
        { buildId = bid, buildFinished = finished, buildTimestamp = ts
        , buildJobsetId = jobsetId, buildJob = job, buildNixName = nixName
        , buildSystem = sys, buildPriority = prio, buildGlobalPriority = gprio
        , buildStartTime = start, buildStopTime = stop
        , buildIsCachedBuild = cached, buildStatus = status
        , buildDrvPath = drv, buildIsCurrent = isCurrent
        , buildProject = proj, buildJobset = js
        , buildDescription = desc, buildLicense = lic
        , buildHomepage = hp, buildMaintainers = maint
        , buildSize = sz, buildClosureSize = closure
        , buildReleaseName = release, buildKeep = keep
        }

-- | Fetch outputs for a build, returned as a list ordered by name.
getBuildOutputs :: Connection -> Int -> IO [BuildOutput]
getBuildOutputs conn buildId' = do
  rows <- query conn [sql|
    SELECT name, path FROM buildoutputs WHERE build = ? ORDER BY name
  |] (Only buildId')
  pure [BuildOutput n p | (n, p) <- rows]

-- | Fetch products for a build, ordered by productnr.
getBuildProducts :: Connection -> Int -> IO [BuildProduct]
getBuildProducts conn buildId' = do
  rows <- query conn [sql|
    SELECT productnr, type, subtype, filesize, sha256hash, path, name, defaultpath
    FROM buildproducts WHERE build = ? ORDER BY productnr
  |] (Only buildId')
  pure [BuildProduct nr t st fs sha p n dp | (nr, t, st, fs, sha, p, n, dp) <- rows]

-- | Fetch steps for a build, ordered by stepnr DESC.
getBuildSteps :: Connection -> Int -> IO [BuildStep]
getBuildSteps conn buildId' = do
  rows <- query conn [sql|
    SELECT build, stepnr, type, drvpath, busy, status, errormsg,
           starttime, stoptime, machine, system,
           propagatedfrom, overhead, timesbuilt, isnondeterministic
    FROM buildsteps
    WHERE build = ?
    ORDER BY stepnr DESC
  |] (Only buildId')
  pure $ map scanStepRow rows

-- | Fetch metrics for a build, ordered by name.
getBuildMetrics :: Connection -> Int -> IO [BuildMetric]
getBuildMetrics conn buildId' = do
  rows <- query conn [sql|
    SELECT name, unit, value FROM buildmetrics WHERE build = ? ORDER BY name
  |] (Only buildId')
  pure [BuildMetric n u v | (n, u, v) <- rows]

-- | Fetch inputs for a build, ordered by name.
getBuildInputs :: Connection -> Int -> IO [BuildInput]
getBuildInputs conn buildId' = do
  rows <- query conn [sql|
    SELECT id, name, type, uri, revision, value, emailresponsible,
           dependency, path, sha256hash
    FROM buildinputs WHERE build = ? ORDER BY name
  |] (Only buildId')
  pure [BuildInput i n t u r v e d p s |
        ((i, n, t, u, r) :. (v, e, d, p, s)) <- rows]

-- | Fetch evaluation IDs that contain a build, ordered DESC.
getBuildEvals :: Connection -> Int -> IO [Int]
getBuildEvals conn buildId' = do
  rows <- query conn [sql|
    SELECT eval FROM jobsetevalmembers WHERE build = ? ORDER BY eval DESC
  |] (Only buildId')
  pure [eid | Only eid <- rows]

-- | Fetch constituent builds of an aggregate build.
getConstituents :: Connection -> Int -> IO [Build]
getConstituents conn buildId' = do
  rows <- query conn [sql|
    SELECT b.id, b.finished, b.timestamp, b.jobset_id, b.job,
           b.nixname, b.system, b.priority, b.globalpriority,
           b.starttime, b.stoptime, b.iscachedbuild, b.buildstatus,
           b.drvpath, b.iscurrent,
           j.project, j.name
    FROM builds b
    JOIN jobsets j ON j.id = b.jobset_id
    JOIN aggregateconstituents ac ON ac.constituent = b.id
    WHERE ac.aggregate = ?
    ORDER BY b.job, b.system
  |] (Only buildId')
  pure $ map scanBuildListRow rows

-- | Fetch all builds belonging to an evaluation, optionally filtered by job name.
buildsByEval :: Connection -> Int -> Maybe Text -> IO [Build]
buildsByEval conn evalId' mfilter = do
  case mfilter of
    Nothing -> do
      rows <- query conn [sql|
        SELECT b.id, b.finished, b.timestamp, b.jobset_id, b.job,
               b.nixname, b.system, b.priority, b.globalpriority,
               b.starttime, b.stoptime, b.iscachedbuild, b.buildstatus,
               b.drvpath, b.iscurrent,
               j.project, j.name
        FROM builds b
        JOIN jobsets j ON j.id = b.jobset_id
        JOIN jobsetevalmembers m ON m.build = b.id
        WHERE m.eval = ?
        ORDER BY b.job, b.system
      |] (Only evalId')
      pure $ map scanBuildListRow rows
    Just f -> do
      rows <- query conn [sql|
        SELECT b.id, b.finished, b.timestamp, b.jobset_id, b.job,
               b.nixname, b.system, b.priority, b.globalpriority,
               b.starttime, b.stoptime, b.iscachedbuild, b.buildstatus,
               b.drvpath, b.iscurrent,
               j.project, j.name
        FROM builds b
        JOIN jobsets j ON j.id = b.jobset_id
        JOIN jobsetevalmembers m ON m.build = b.id
        WHERE m.eval = ? AND b.job ILIKE '%' || ? || '%'
        ORDER BY b.job, b.system
      |] (evalId', f)
      pure $ map scanBuildListRow rows

-- | Fetch all unfinished builds ordered by priority.
queuedBuilds :: Connection -> IO [Build]
queuedBuilds conn = do
  rows <- query_ conn [sql|
    SELECT b.id, b.finished, b.timestamp, b.jobset_id, b.job,
           b.nixname, b.system, b.priority, b.globalpriority,
           b.starttime, b.stoptime, b.iscachedbuild, b.buildstatus,
           b.drvpath, b.iscurrent,
           j.project, j.name
    FROM builds b
    JOIN jobsets j ON j.id = b.jobset_id
    WHERE b.finished = 0
    ORDER BY b.globalpriority DESC, b.id
  |]
  pure $ map scanBuildListRow rows

-- | Find the latest succeeded build for a job, optionally filtered by system.
latestBuildForJob :: Connection -> Text -> Text -> Text
                  -> Maybe Text -> IO (Maybe Build)
latestBuildForJob conn project jobset job mSystem = do
  rows <- case mSystem of
    Nothing -> query conn [sql|
      SELECT b.id, b.finished, b.timestamp, b.jobset_id, b.job,
             b.nixname, b.system, b.priority, b.globalpriority,
             b.starttime, b.stoptime, b.iscachedbuild, b.buildstatus,
             b.drvpath, b.iscurrent,
             j.project, j.name
      FROM builds b
      JOIN jobsets j ON j.id = b.jobset_id
      WHERE j.project = ? AND j.name = ? AND b.job = ?
        AND b.finished = 1 AND b.buildstatus = 0
      ORDER BY b.id DESC LIMIT 1
    |] (project, jobset, job)
    Just sys -> query conn [sql|
      SELECT b.id, b.finished, b.timestamp, b.jobset_id, b.job,
             b.nixname, b.system, b.priority, b.globalpriority,
             b.starttime, b.stoptime, b.iscachedbuild, b.buildstatus,
             b.drvpath, b.iscurrent,
             j.project, j.name
      FROM builds b
      JOIN jobsets j ON j.id = b.jobset_id
      WHERE j.project = ? AND j.name = ? AND b.job = ? AND b.system = ?
        AND b.finished = 1 AND b.buildstatus = 0
      ORDER BY b.id DESC LIMIT 1
    |] (project, jobset, job, sys)
  case rows of
    []    -> pure Nothing
    (r:_) -> pure $ Just (scanBuildListRow r)

-- | Scan a BuildStep row using nested tuples.
scanStepRow :: ( (Int, Int, Int, Maybe Text, Int, Maybe Int, Maybe Text)
               :. (Maybe Int, Maybe Int, Text, Maybe Text, Maybe Int, Maybe Int, Maybe Int, Maybe Bool) )
            -> BuildStep
scanStepRow ( (b, nr, t, drv, busy, st, err)
            :. (start, stop, machine, sys, prop, overhead, times, nondet) ) =
  BuildStep b nr t drv busy st err start stop machine sys
            prop overhead times nondet
