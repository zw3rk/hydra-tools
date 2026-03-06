-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handlers for queue pages, queue summary, machines, and steps.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Queue
  ( queueHandler
  , queueSummaryHandler
  , machinesHandler
  , stepsHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Lucid

import Data.Text (Text)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Builds (queuedBuilds)
import HydraWeb.DB.Queue
import HydraWeb.Models.Build (Build (..))
import HydraWeb.Models.Queue (ActiveStep (..), QueueSummary (..))
import HydraWeb.Visibility (filterByProjectAccess)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Queue (queuePage, queueSummaryPage, machinesPage, stepsPage)
import HydraWeb.View.Components (showT)

-- | Render the full queue list page (GET /queue).
queueHandler :: Maybe Text -> AppM (Html ())
queueHandler mCookie = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  (builds, counts) <- liftIO $ withConn pool $ \conn -> do
    bs <- queuedBuilds conn
    visible <- filterByProjectAccess conn mUser buildProject bs
    nc <- navCounts conn
    pure (visible, nc)
  let total = length builds
      pd = PageData
        { pdTitle    = "Build Queue (" <> showT total <> ")"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = mUser
        }
  pure $ pageLayout pd $ queuePage bp builds total

-- | Render queue summary by jobset and system (GET /queue-summary).
queueSummaryHandler :: Maybe Text -> AppM (Html ())
queueSummaryHandler mCookie = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  (summary, systems, total, counts) <- liftIO $ withConn pool $ \conn -> do
    s  <- queueSummary conn
    visible <- filterByProjectAccess conn mUser qsProject s
    sy <- systemQueueSummary conn
    t  <- queueCount conn
    nc <- navCounts conn
    pure (visible, sy, t, nc)
  let pd = PageData
        { pdTitle    = "Queue Summary"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = mUser
        }
  pure $ pageLayout pd $ queueSummaryPage bp summary systems total

-- | Render the machines/active-steps page (GET /machines).
machinesHandler :: Maybe Text -> AppM (Html ())
machinesHandler mCookie = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  (steps, counts) <- liftIO $ withConn pool $ \conn -> do
    s  <- activeSteps conn
    visible <- filterByProjectAccess conn mUser asProject s
    nc <- navCounts conn
    pure (visible, nc)
  let pd = PageData
        { pdTitle    = "Machine Status"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = mUser
        }
  pure $ pageLayout pd $ machinesPage bp steps

-- | Render recent build steps page (GET /steps).
stepsHandler :: Maybe Text -> Maybe Int -> AppM (Html ())
stepsHandler mCookie mPage = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  let page    = min 10000 (max 1 (fromMaybe 1 mPage))
      perPage = 20
      offset  = (page - 1) * perPage
  (steps, counts) <- liftIO $ withConn pool $ \conn -> do
    s  <- recentSteps conn offset perPage
    nc <- navCounts conn
    pure (s, nc)
  let pd = PageData
        { pdTitle    = "Latest Build Steps"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = mUser
        }
  pure $ pageLayout pd $ stepsPage bp steps page perPage
