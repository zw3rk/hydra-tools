-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | SSE HTML fragment renderers. Each function queries the DB and renders
-- a partial HTML fragment as a ByteString for SSE broadcasting.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.SSE.Fragments
  ( renderNavCountsBS
  , renderQueueContentBS
  , renderMachinesContentBS
  , renderRunningEvalsBS
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (Connection)
import Lucid

import HydraWeb.DB.Queue (navCounts, activeSteps)
import HydraWeb.DB.Builds (queuedBuilds)
import HydraWeb.DB.Evals (runningEvaluations)
import HydraWeb.Models.Queue (NavCounts (..))

-- | Render nav count badges as an OOB-swappable HTML fragment.
-- Each badge has a unique ID that can be targeted by hx-swap-oob.
renderNavCountsBS :: Connection -> IO ByteString
renderNavCountsBS conn = do
  nc <- navCounts conn
  pure $ LBS.toStrict $ renderBS $ do
    -- These elements are designed for OOB swap via HTMX.
    span_ [id_ "nav-queue-count"] $ toHtml (showT (ncQueued nc))
    span_ [id_ "nav-running-count"] $ toHtml (showT (ncRunning nc))
    span_ [id_ "nav-evals-count"] $ toHtml (showT (ncRunningEvals nc))
    span_ [id_ "nav-bridges-count"] $ toHtml (showT (ncBridgePending nc))

-- | Render queue content fragment for SSE.
renderQueueContentBS :: Connection -> IO ByteString
renderQueueContentBS conn = do
  builds <- queuedBuilds conn
  let total = length builds
  pure $ LBS.toStrict $ renderBS $
    p_ $ toHtml ("Queue: " <> showT total <> " builds")

-- | Render machines/active-steps content fragment for SSE.
renderMachinesContentBS :: Connection -> IO ByteString
renderMachinesContentBS conn = do
  steps <- activeSteps conn
  let total = length steps
  pure $ LBS.toStrict $ renderBS $
    p_ $ toHtml ("Machines: " <> showT total <> " active steps")

-- | Render running evaluations content fragment for SSE.
renderRunningEvalsBS :: Connection -> IO ByteString
renderRunningEvalsBS conn = do
  evals <- runningEvaluations conn
  let total = length evals
  pure $ LBS.toStrict $ renderBS $
    p_ $ toHtml ("Evaluating: " <> showT total <> " jobsets")

-- | Show an Int as Text.
showT :: Int -> Text.Text
showT = Text.pack . show
