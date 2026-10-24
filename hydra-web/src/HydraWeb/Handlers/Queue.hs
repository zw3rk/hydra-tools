-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for queue pages, queue summary, machines, and steps.
module HydraWeb.Handlers.Queue
  ( queueHandler,
    queueSummaryHandler,
    machinesHandler,
    stepsHandler,
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Builds (queuedBuilds)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Queue (activeSteps, queueSummary, recentSteps, systemQueueSummary)
import HydraWeb.Models.Build (Build (..))
import HydraWeb.Models.Queue (ActiveStep (..), QueueSummary (..))
import HydraWeb.Types (App (..), AppM)
import HydraWeb.View.Components (showT)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Queue (machinesPage, queuePage, queueSummaryPage, stepsPage)
import HydraWeb.Visibility (filterByProjectAccess)
import Lucid

-- | Render the full queue list page (GET /queue).
queueHandler :: Maybe Text -> AppM (Html ())
queueHandler mCookie = do
  pool <- asks appPool
  bp <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  builds <- liftIO $ withConn pool $ \conn -> do
    bs <- queuedBuilds conn
    filterByProjectAccess conn mUser buildProject bs
  let total = length builds
      pd =
        PageData
          { pdTitle = "Build Queue (" <> showT total <> ")",
            pdBasePath = bp,
            pdCounts = counts,
            pdUser = mUser
          }
  pure $ pageLayout pd $ queuePage bp builds total

-- | Render queue summary by jobset and system (GET /queue-summary).
-- Derives the total from visible summary rows to avoid leaking the
-- existence of private-repo builds via count discrepancy.
queueSummaryHandler :: Maybe Text -> AppM (Html ())
queueSummaryHandler mCookie = do
  pool <- asks appPool
  bp <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  (summary, systems) <- liftIO $ withConn pool $ \conn -> do
    s <- queueSummary conn
    visible <- filterByProjectAccess conn mUser qsProject s
    sy <- systemQueueSummary conn
    pure (visible, sy)
  let total = sum (map qsQueued summary)
      pd =
        PageData
          { pdTitle = "Queue Summary",
            pdBasePath = bp,
            pdCounts = counts,
            pdUser = mUser
          }
  pure $ pageLayout pd $ queueSummaryPage bp summary systems total

-- | Render the machines/active-steps page (GET /machines).
machinesHandler :: Maybe Text -> AppM (Html ())
machinesHandler mCookie = do
  pool <- asks appPool
  bp <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  steps <- liftIO $ withConn pool $ \conn -> do
    s <- activeSteps conn
    filterByProjectAccess conn mUser asProject s
  let pd =
        PageData
          { pdTitle = "Machine Status",
            pdBasePath = bp,
            pdCounts = counts,
            pdUser = mUser
          }
  pure $ pageLayout pd $ machinesPage bp steps

-- | Render recent build steps page (GET /steps).
stepsHandler :: Maybe Text -> Maybe Int -> AppM (Html ())
stepsHandler mCookie mPage = do
  pool <- asks appPool
  bp <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  let page = min 10000 (max 1 (fromMaybe 1 mPage))
      perPage = 20
      offset = (page - 1) * perPage
  steps <- liftIO $ withConn pool $ \conn -> do
    s <- recentSteps conn offset perPage
    -- Phase 2: filter by repo privacy (project name is the first element).
    visible <- filterByProjectAccess conn mUser fst s
    pure (map snd visible)
  let pd =
        PageData
          { pdTitle = "Latest Build Steps",
            pdBasePath = bp,
            pdCounts = counts,
            pdUser = mUser
          }
  pure $ pageLayout pd $ stepsPage bp steps page perPage
