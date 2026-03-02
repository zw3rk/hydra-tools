-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | JSON API handlers for backward-compatible Hydra API endpoints.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.API
  ( apiJobsetsHandler
  , apiNrQueueHandler
  , apiLatestBuildsHandler
  , apiQueueHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Projects (jobsetOverview)
import HydraWeb.DB.Queue (queueCount)
import HydraWeb.DB.Builds (queuedBuilds, latestBuilds)
import HydraWeb.Models.Project (Jobset (..))
import HydraWeb.Models.Build (Build (..))

-- | JSON representation of a jobset for the API.
data APIJobset = APIJobset
  { ajName        :: !Text
  , ajProject     :: !Text
  , ajNrScheduled :: !Int
  , ajNrFailed    :: !Int
  , ajNrSucceeded :: !Int
  , ajNrTotal     :: !Int
  } deriving (Generic)

instance ToJSON APIJobset where
  toJSON j = object
    [ "name"        .= ajName j
    , "project"     .= ajProject j
    , "nrscheduled" .= ajNrScheduled j
    , "nrfailed"    .= ajNrFailed j
    , "nrsucceeded" .= ajNrSucceeded j
    , "nrtotal"     .= ajNrTotal j
    ]

-- | JSON representation of a build for the API.
data APIBuild = APIBuild
  { abId       :: !Int
  , abFinished :: !Int
  , abJob      :: !Text
  , abSystem   :: !Text
  , abProject  :: !Text
  , abJobset   :: !Text
  , abNixName  :: !(Maybe Text)
  , abStatus   :: !(Maybe Int)
  , abDrvPath  :: !Text
  } deriving (Generic)

instance ToJSON APIBuild where
  toJSON b = object
    [ "id"          .= abId b
    , "finished"    .= abFinished b
    , "job"         .= abJob b
    , "system"      .= abSystem b
    , "project"     .= abProject b
    , "jobset"      .= abJobset b
    , "nixname"     .= abNixName b
    , "buildstatus" .= abStatus b
    , "drvpath"     .= abDrvPath b
    ]

-- | Convert a Build to its API representation.
buildToAPI :: Build -> APIBuild
buildToAPI b = APIBuild
  { abId       = buildId b
  , abFinished = buildFinished b
  , abJob      = buildJob b
  , abSystem   = buildSystem b
  , abProject  = buildProject b
  , abJobset   = buildJobset b
  , abNixName  = buildNixName b
  , abStatus   = buildStatus b
  , abDrvPath  = buildDrvPath b
  }

-- | GET /api/jobsets?project=... — jobset overview as JSON.
apiJobsetsHandler :: Maybe Text -> AppM [APIJobset]
apiJobsetsHandler mProject = do
  pool <- asks appPool
  let project = fromMaybe "" mProject
  if Text.null project
    then pure []
    else do
      jobsets <- liftIO $ withConn pool $ \conn ->
        jobsetOverview conn project
      pure $ map jobsetToAPI jobsets
  where
    jobsetToAPI j = APIJobset
      { ajName        = jsName j
      , ajProject     = jsProject j
      , ajNrScheduled = jsNrScheduled j
      , ajNrFailed    = jsNrFailed j
      , ajNrSucceeded = jsNrSucceeded j
      , ajNrTotal     = jsNrTotal j
      }

-- | GET /api/nrqueue — queue count as plain text.
apiNrQueueHandler :: AppM Text
apiNrQueueHandler = do
  pool <- asks appPool
  n <- liftIO $ withConn pool queueCount
  pure $ Text.pack (show n)

-- | GET /api/latestbuilds?nr=N&project=...&jobset=...&job=...&system=...
apiLatestBuildsHandler :: Maybe Int -> Maybe Text -> Maybe Text
                       -> Maybe Text -> Maybe Text -> AppM [APIBuild]
apiLatestBuildsHandler mNr mProject mJobset mJob mSystem = do
  pool <- asks appPool
  let nr = fromMaybe 10 mNr
  builds <- liftIO $ withConn pool $ \conn ->
    latestBuilds conn nr mProject mJobset mJob mSystem
  pure $ map buildToAPI builds

-- | GET /api/queue?nr=N — queued builds as JSON.
apiQueueHandler :: Maybe Int -> AppM [APIBuild]
apiQueueHandler mNr = do
  pool <- asks appPool
  builds <- liftIO $ withConn pool queuedBuilds
  let nr = fromMaybe 100 mNr
  pure $ map buildToAPI (take nr builds)
