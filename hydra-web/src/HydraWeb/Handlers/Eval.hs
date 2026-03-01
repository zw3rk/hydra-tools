-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for eval pages (GET /eval/:id) and HTMX tab partials.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Eval
  ( evalHandler
  , evalTabHandler
  , latestEvalsHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Error.Class (throwError)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Lucid
import Servant (err404)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Evals (getEval, getEvalError, getEvalInputs, previousEval,
                           latestEvals, latestEvalsCount)
import HydraWeb.DB.Builds (buildsByEval)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.Models.Eval (JobsetEval (..))
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Eval (evalPage, evalTabContent, latestEvalsPage)
import HydraWeb.View.BuildDiff (BuildDiff, computeBuildDiff)
import HydraWeb.View.Components (showT)

-- | Load the build diff for an eval (current builds vs previous eval's builds).
loadBuildDiff :: Connection -> JobsetEval -> IO BuildDiff
loadBuildDiff conn eval = do
  curBuilds <- buildsByEval conn (evalId eval) Nothing
  mPrev     <- previousEval conn eval
  prevBuilds <- case mPrev of
    Nothing -> pure []
    Just p  -> buildsByEval conn (evalId p) Nothing
  pure $ computeBuildDiff curBuilds prevBuilds

-- | Render the eval detail page with build diff.
evalHandler :: Int -> AppM (Html ())
evalHandler eid = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  result <- liftIO $ withConn pool $ \conn -> do
    mEval <- getEval conn eid
    case mEval of
      Nothing -> pure Nothing
      Just eval -> do
        inputs  <- getEvalInputs conn eid
        evalErr <- getEvalError conn eid
        diff    <- loadBuildDiff conn eval
        nc      <- navCounts conn
        pure $ Just (eval, inputs, evalErr, diff, nc)
  case result of
    Nothing -> throwError err404
    Just (eval, inputs, evalErr, diff, counts) -> do
      let pd = PageData
            { pdTitle    = "Evaluation #" <> showT eid
            , pdBasePath = bp
            , pdCounts   = counts
            }
      pure $ pageLayout pd $ evalPage bp eval inputs evalErr diff

-- | Render an eval tab partial (HTMX response).
evalTabHandler :: Int -> Text -> AppM (Html ())
evalTabHandler eid tabName = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  result <- liftIO $ withConn pool $ \conn -> do
    mEval <- getEval conn eid
    case mEval of
      Nothing -> pure Nothing
      Just eval -> do
        diff <- loadBuildDiff conn eval
        pure $ Just diff
  case result of
    Nothing -> throwError err404
    Just diff -> pure $ evalTabContent bp tabName diff

-- | Render the latest evaluations list page (GET /evals).
latestEvalsHandler :: Maybe Int -> AppM (Html ())
latestEvalsHandler mPage = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  let page    = max 1 (maybe 1 id mPage)
      perPage = 20
      offset  = (page - 1) * perPage
  (evals, total, counts) <- liftIO $ withConn pool $ \conn -> do
    es <- latestEvals conn offset perPage
    tc <- latestEvalsCount conn
    nc <- navCounts conn
    pure (es, tc, nc)
  let pd = PageData
        { pdTitle    = "Latest Evaluations"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ latestEvalsPage bp evals total page perPage
