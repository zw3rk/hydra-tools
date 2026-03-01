-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for projects and jobsets.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Projects
  ( visibleProjects
  , allProjects
  , getProject
  , getJobset
  , jobsetOverview
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.Types ((:.)((:.)))

import HydraWeb.Models.Project (Project (..), Jobset (..))

-- | Fetch all visible (non-hidden) projects.
visibleProjects :: Connection -> IO [Project]
visibleProjects conn = do
  rows <- query_ conn [sql|
    SELECT name, displayname, description, enabled, hidden, owner, homepage
    FROM projects
    WHERE hidden = 0
    ORDER BY enabled DESC, name
  |]
  pure $ map toProject rows
  where
    toProject (name, displayname, desc, enabled, hidden, owner, homepage) =
      Project name displayname desc enabled hidden owner homepage
             Nothing Nothing Nothing []

-- | Fetch all projects (including hidden).
allProjects :: Connection -> IO [Project]
allProjects conn = do
  rows <- query_ conn [sql|
    SELECT name, displayname, description, enabled, hidden, owner, homepage
    FROM projects
    ORDER BY enabled DESC, name
  |]
  pure $ map toProject rows
  where
    toProject (name, displayname, desc, enabled, hidden, owner, homepage) =
      Project name displayname desc enabled hidden owner homepage
             Nothing Nothing Nothing []

-- | Fetch a single project by name, with its jobset names.
getProject :: Connection -> Text -> IO (Maybe Project)
getProject conn name = do
  rows <- query conn [sql|
    SELECT name, displayname, description, enabled, hidden, owner, homepage,
           declfile, decltype, declvalue
    FROM projects
    WHERE name = ?
  |] (Only name)
  case rows of
    [] -> pure Nothing
    ((pname, displayname, desc, enabled, hidden, owner, homepage,
      declfile, decltype', declvalue):_) -> do
      jrows <- query conn [sql|
        SELECT name FROM jobsets WHERE project = ? ORDER BY name
      |] (Only name)
      let jobsets = map (\(Only jn) -> jn) jrows
      pure $ Just $ Project pname displayname desc enabled hidden owner homepage
                            declfile decltype' declvalue jobsets

-- | Fetch a single jobset by project and name, with build count subqueries.
getJobset :: Connection -> Text -> Text -> IO (Maybe Jobset)
getJobset conn project name = do
  rows <- query conn [sql|
    SELECT j.name, j.id, j.project, j.description,
           j.nixexprinput, j.nixexprpath,
           j.errormsg, j.errortime, j.lastcheckedtime, j.triggertime,
           j.enabled, j.enableemail, j.hidden, j.emailoverride,
           j.keepnr, j.checkinterval, j.schedulingshares,
           j.fetcherrormsg, j.starttime, j.type, j.flake,
           (SELECT count(*) FROM builds WHERE jobset_id = j.id AND finished = 0 AND iscurrent = 1),
           (SELECT count(*) FROM builds WHERE jobset_id = j.id AND finished = 1 AND buildstatus <> 0 AND iscurrent = 1),
           (SELECT count(*) FROM builds WHERE jobset_id = j.id AND finished = 1 AND buildstatus = 0 AND iscurrent = 1),
           (SELECT count(*) FROM builds WHERE jobset_id = j.id AND iscurrent = 1)
    FROM jobsets j
    WHERE j.project = ? AND j.name = ?
  |] (project, name)
  case rows of
    [] -> pure Nothing
    (r:_) -> pure $ Just (scanJobsetRow r)

-- | Fetch jobsets for a project with build count summaries.
jobsetOverview :: Connection -> Text -> IO [Jobset]
jobsetOverview conn project = do
  rows <- query conn [sql|
    SELECT j.name, j.id, j.project, j.description,
           j.nixexprinput, j.nixexprpath,
           j.errormsg, j.errortime, j.lastcheckedtime, j.triggertime,
           j.enabled, j.enableemail, j.hidden, j.emailoverride,
           j.keepnr, j.checkinterval, j.schedulingshares,
           j.fetcherrormsg, j.starttime, j.type, j.flake,
           (SELECT count(*) FROM builds WHERE jobset_id = j.id AND finished = 0 AND iscurrent = 1),
           (SELECT count(*) FROM builds WHERE jobset_id = j.id AND finished = 1 AND buildstatus <> 0 AND iscurrent = 1),
           (SELECT count(*) FROM builds WHERE jobset_id = j.id AND finished = 1 AND buildstatus = 0 AND iscurrent = 1),
           (SELECT count(*) FROM builds WHERE jobset_id = j.id AND iscurrent = 1)
    FROM jobsets j
    WHERE j.project = ?
    ORDER BY j.hidden ASC, j.enabled DESC, j.name
  |] (Only project)
  pure $ map scanJobsetRow rows

-- | Scan a full jobset row (25 columns) into a Jobset value.
-- Uses nested tuples via (:.) to work around postgresql-simple's
-- FromRow instance limit of ~10 elements per tuple.
scanJobsetRow :: ( (Text, Int, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Int, Maybe Int)
                 :. (Maybe Int, Int, Int, Int, Text, Int, Int, Int)
                 :. (Maybe Text, Maybe Int, Int, Maybe Text, Int, Int, Int, Int) )
              -> Jobset
scanJobsetRow ( (name, jid, proj, desc, nixExprInput, nixExprPath, errMsg, errTime, lastChecked)
              :. (triggerTime, enabled, enableEmail, hidden, emailOverride, keepNr, checkInterval, schedulingShares)
              :. (fetchErrMsg, startTime, typ, flake, nrScheduled, nrFailed, nrSucceeded, nrTotal) ) =
  Jobset name jid proj desc nixExprInput nixExprPath
         errMsg errTime lastChecked triggerTime
         enabled enableEmail hidden emailOverride
         keepNr checkInterval schedulingShares
         fetchErrMsg startTime typ flake
         nrScheduled nrFailed nrSucceeded nrTotal
