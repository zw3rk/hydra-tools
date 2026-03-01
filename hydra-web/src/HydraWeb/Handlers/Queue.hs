-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
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
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Builds (queuedBuilds)
import HydraWeb.DB.Queue
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Queue (queuePage, queueSummaryPage, machinesPage, stepsPage)

-- | Render the full queue list page (GET /queue).
queueHandler :: AppM (Html ())
queueHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (builds, counts) <- liftIO $ withConn pool $ \conn -> do
    bs <- queuedBuilds conn
    nc <- navCounts conn
    pure (bs, nc)
  let total = length builds
      pd = PageData
        { pdTitle    = "Build Queue (" <> showT total <> ")"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ queuePage bp builds total

-- | Render queue summary by jobset and system (GET /queue-summary).
queueSummaryHandler :: AppM (Html ())
queueSummaryHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (summary, systems, total, counts) <- liftIO $ withConn pool $ \conn -> do
    s  <- queueSummary conn
    sy <- systemQueueSummary conn
    t  <- queueCount conn
    nc <- navCounts conn
    pure (s, sy, t, nc)
  let pd = PageData
        { pdTitle    = "Queue Summary"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ queueSummaryPage bp summary systems total

-- | Render the machines/active-steps page (GET /machines).
machinesHandler :: AppM (Html ())
machinesHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (steps, counts) <- liftIO $ withConn pool $ \conn -> do
    s  <- activeSteps conn
    nc <- navCounts conn
    pure (s, nc)
  let pd = PageData
        { pdTitle    = "Machine Status"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ machinesPage bp steps

-- | Render recent build steps page (GET /steps).
stepsHandler :: Maybe Int -> AppM (Html ())
stepsHandler mPage = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  let page    = max 1 (maybe 1 id mPage)
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
        }
  pure $ pageLayout pd $ stepsPage bp steps page perPage

showT :: Int -> Text
showT = Text.pack . show
