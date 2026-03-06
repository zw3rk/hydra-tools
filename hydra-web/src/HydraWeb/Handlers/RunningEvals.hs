-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the evaluations page (GET /running-evals).
-- Shows both currently running and queued evaluations.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.RunningEvals
  ( runningEvalsHandler
  ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Lucid (Html)

import Data.Text (Text)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Evals (runningEvaluations, queuedEvaluations)
import HydraWeb.Models.Eval (RunningEval (..), QueuedEval (..))
import HydraWeb.Visibility (filterByProjectAccess)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.RunningEvals (runningEvalsPage)

-- | GET /running-evals — render running + queued evaluations.
runningEvalsHandler :: Maybe Text -> AppM (Html ())
runningEvalsHandler mCookie = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  (evals, queued) <- liftIO $ withConn pool $ \conn -> do
    es <- runningEvaluations conn
      >>= filterByProjectAccess conn mUser reProject
    qs <- queuedEvaluations conn
      >>= filterByProjectAccess conn mUser qeProject
    pure (es, qs)
  let pd = PageData
        { pdTitle    = "Evaluations"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = mUser
        }
  pure $ pageLayout pd $ runningEvalsPage bp evals queued
