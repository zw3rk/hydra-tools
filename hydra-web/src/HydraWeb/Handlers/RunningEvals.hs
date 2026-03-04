-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for the running evaluations page (GET /running-evals).
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.RunningEvals
  ( runningEvalsHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Lucid (Html)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Evals (runningEvaluations)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.RunningEvals (runningEvalsPage)

-- | GET /running-evals — render the running evaluations page.
runningEvalsHandler :: AppM (Html ())
runningEvalsHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (evals, counts) <- liftIO $ withConn pool $ \conn -> do
    es <- runningEvaluations conn
    nc <- navCounts conn
    pure (es, nc)
  let pd = PageData
        { pdTitle    = "Running Evaluations"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = Nothing
        }
  pure $ pageLayout pd $ runningEvalsPage bp evals
