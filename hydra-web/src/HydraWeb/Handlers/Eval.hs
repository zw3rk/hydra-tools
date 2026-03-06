-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Lucid
import Servant (err404)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Evals (getEval, getEvalError, getEvalInputs, previousEval,
                           latestEvals, latestEvalsCount)
import HydraWeb.DB.Builds (buildsByEval)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.Models.Eval (JobsetEval (..), EvalInfo (..))
import HydraWeb.Visibility (isProjectAccessible, filterByProjectAccess)
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
-- Returns 404 if the eval's parent project is hidden and user is not super-admin.
evalHandler :: Maybe Text -> Int -> AppM (Html ())
evalHandler mCookie eid = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  result <- liftIO $ withConn pool $ \conn -> do
    mEval <- getEval conn eid
    case mEval of
      Nothing -> pure Nothing
      Just eval -> do
        accessible <- isProjectAccessible conn (evalProject eval) mUser
        if not accessible
          then pure Nothing
          else do
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
            , pdUser     = mUser
            }
      pure $ pageLayout pd $ evalPage bp eval inputs evalErr diff

-- | Render an eval tab partial (HTMX response).
-- Returns 404 if the eval's parent project is hidden and user is not super-admin.
evalTabHandler :: Maybe Text -> Int -> Text -> AppM (Html ())
evalTabHandler mCookie eid tabName = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  result <- liftIO $ withConn pool $ \conn -> do
    mEval <- getEval conn eid
    case mEval of
      Nothing -> pure Nothing
      Just eval -> do
        accessible <- isProjectAccessible conn (evalProject eval) mUser
        if not accessible
          then pure Nothing
          else do
            diff <- loadBuildDiff conn eval
            pure $ Just diff
  case result of
    Nothing -> throwError err404
    Just diff -> pure $ evalTabContent bp tabName diff

-- | Render the latest evaluations list page (GET /evals).
latestEvalsHandler :: Maybe Text -> Maybe Int -> AppM (Html ())
latestEvalsHandler mCookie mPage = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  let page    = min 10000 (max 1 (fromMaybe 1 mPage))
      perPage = 20
      offset  = (page - 1) * perPage
  mUser <- liftIO $ getOptionalUser pool mCookie
  (evals, total, counts) <- liftIO $ withConn pool $ \conn -> do
    es <- latestEvals conn offset perPage
    visible <- filterByProjectAccess conn mUser (evalProject . eiEval) es
    tc <- latestEvalsCount conn
    nc <- navCounts conn
    pure (visible, tc, nc)
  let pd = PageData
        { pdTitle    = "Latest Evaluations"
        , pdBasePath = bp
        , pdCounts   = counts
        , pdUser     = mUser
        }
  pure $ pageLayout pd $ latestEvalsPage bp evals total page perPage
