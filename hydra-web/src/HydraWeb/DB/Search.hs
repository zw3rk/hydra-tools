-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Database queries for cross-entity search.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HydraWeb.DB.Search
  ( SearchResults (..)
  , search
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (Connection, query, execute_)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Database.PostgreSQL.Simple.Types ((:.)((:.)))

import HydraWeb.Models.Project (Project (..), Jobset (..))
import HydraWeb.Models.Build (Build (..))

-- | Results from a cross-entity search.
data SearchResults = SearchResults
  { srProjects  :: ![Project]   -- ^ Projects matching by name/displayname/description
  , srJobsets   :: ![Jobset]    -- ^ Jobsets matching by name/description
  , srBuilds    :: ![Build]     -- ^ Builds matching by output path
  , srBuildsDrv :: ![Build]     -- ^ Builds matching by derivation path
  }

-- | Perform a cross-entity search with input validation.
-- Returns Nothing if the query contains invalid characters.
search :: Connection -> Text -> Int -> IO (Maybe SearchResults)
search conn q rawLimit
  | not (isValidQuery q) = pure Nothing
  | otherwise = do
      let limit = max 1 (min 50 rawLimit)
          pattern = "%" <> q <> "%"

      -- Set a statement timeout for safety.
      _ <- execute_ conn [sql|SET LOCAL statement_timeout = 20000|]

      -- Search projects.
      prows <- query conn [sql|
        SELECT name, displayname, description, enabled, hidden, owner, homepage
        FROM projects
        WHERE hidden = 0
          AND (name ILIKE ? OR displayname ILIKE ? OR description ILIKE ?)
        ORDER BY name
      |] (pattern, pattern, pattern)
      let projects = map (\(n, dn, d, e, h, o, hp) ->
            Project n dn d e h o hp Nothing Nothing Nothing []) prows

      -- Search jobsets.
      jrows <- query conn [sql|
        SELECT j.name, j.id, j.project, j.description,
               j.enabled, j.hidden, j.type, j.flake
        FROM jobsets j
        JOIN projects p ON p.name = j.project
        WHERE p.hidden = 0 AND j.hidden = 0
          AND (j.name ILIKE ? OR j.description ILIKE ?)
        ORDER BY j.project, j.name
      |] (pattern, pattern)
      let jobsets = map scanSearchJobset jrows

      -- Search builds by output path.
      brows <- query conn [sql|
        SELECT b.id, b.finished, b.timestamp, b.jobset_id, b.job,
               b.nixname, b.system, b.priority, b.globalpriority,
               b.starttime, b.stoptime, b.iscachedbuild, b.buildstatus,
               b.drvpath, b.iscurrent,
               j.project, j.name
        FROM builds b
        JOIN jobsets j ON j.id = b.jobset_id
        JOIN buildoutputs bo ON bo.build = b.id
        WHERE bo.path ILIKE ?
        ORDER BY b.id DESC
        LIMIT ?
      |] (pattern, limit)
      let builds = map scanBuildRow brows

      -- Search builds by derivation path.
      drows <- query conn [sql|
        SELECT b.id, b.finished, b.timestamp, b.jobset_id, b.job,
               b.nixname, b.system, b.priority, b.globalpriority,
               b.starttime, b.stoptime, b.iscachedbuild, b.buildstatus,
               b.drvpath, b.iscurrent,
               j.project, j.name
        FROM builds b
        JOIN jobsets j ON j.id = b.jobset_id
        WHERE b.drvpath ILIKE ?
        ORDER BY b.id DESC
        LIMIT ?
      |] (pattern, limit)
      let buildsDrv = map scanBuildRow drows

      pure $ Just SearchResults
        { srProjects  = projects
        , srJobsets   = jobsets
        , srBuilds    = builds
        , srBuildsDrv = buildsDrv
        }

-- | Validate search query: only alphanumeric, underscore, hyphen, slash, dot.
isValidQuery :: Text -> Bool
isValidQuery = Text.all isAllowed
  where
    isAllowed c = c >= 'a' && c <= 'z'
              || c >= 'A' && c <= 'Z'
              || c >= '0' && c <= '9'
              || c == '_' || c == '-' || c == '/' || c == '.'

-- | Scan a search jobset row (partial fields).
scanSearchJobset :: (Text, Int, Text, Maybe Text, Int, Int, Int, Maybe Text) -> Jobset
scanSearchJobset (n, jid, p, d, e, h, t, f) =
  Jobset n jid p d Nothing Nothing
         Nothing Nothing Nothing Nothing
         e 0 h "" 0 0 0 Nothing Nothing t f 0 0 0 0

-- | Scan a build list row (17 columns) using nested tuples via (:.).
scanBuildRow :: ( (Int, Int, Int, Int, Text, Maybe Text, Text, Int, Int)
                :. (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Text, Maybe Int, Text, Text) )
             -> Build
scanBuildRow ( (bid, finished, ts, jobsetId, job, nixName, sys, prio, gprio)
             :. (start, stop, cached, status, drv, isCurrent, proj, js) ) =
  Build bid finished ts jobsetId job nixName sys prio gprio
        start stop cached status drv isCurrent
        proj js
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
