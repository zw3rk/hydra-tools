-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
{-# LANGUAGE OverloadedStrings #-}

-- | Handler for the evaluations page (GET /running-evals).
-- Shows both currently running and queued evaluations.
module HydraWeb.Handlers.RunningEvals
  ( runningEvalsHandler,
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Evals (queuedEvaluations, runningEvaluations)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.Models.Eval (QueuedEval (..), RunningEval (..))
import HydraWeb.Types (App (..), AppM)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.RunningEvals (runningEvalsPage)
import HydraWeb.Visibility (filterByProjectAccess)
import Lucid (Html)

-- | GET /running-evals — render running + queued evaluations.
runningEvalsHandler :: Maybe Text -> AppM (Html ())
runningEvalsHandler mCookie = do
  pool <- asks appPool
  bp <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  (evals, queued) <- liftIO $ withConn pool $ \conn -> do
    es <-
      runningEvaluations conn
        >>= filterByProjectAccess conn mUser reProject
    qs <-
      queuedEvaluations conn
        >>= filterByProjectAccess conn mUser qeProject
    pure (es, qs)
  let pd =
        PageData
          { pdTitle = "Evaluations",
            pdBasePath = bp,
            pdCounts = counts,
            pdUser = mUser
          }
  pure $ pageLayout pd $ runningEvalsPage bp evals queued
