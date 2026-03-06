-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handlers for user-triggered actions (trigger eval, restart build).
-- All actions require authentication. Returns 303 redirect to the
-- originating page on success.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Actions
  ( triggerEvalHandler
  , restartBuildHandler
  ) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Servant (err303, err401, ServerError (..), NoContent (..))

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (requireAuth)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Actions (triggerJobsetEval, restartBuild)
import HydraWeb.DB.Projects (getProjectNameByBuild)
import HydraWeb.Visibility (isProjectAccessible)
import HydraWeb.View.Components (showT)

-- | POST /projects/:name/jobsets/:js/trigger
-- Sets triggertime on the jobset so the evaluator picks it up.
-- Requires authentication. Redirects back to the jobset page.
triggerEvalHandler :: Maybe Text -> Text -> Text -> AppM NoContent
triggerEvalHandler mCookie project jobset = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  -- Require login.
  authResult <- liftIO $ requireAuth pool mCookie
  case authResult of
    Left _ -> throwError err401 { errBody = "Login required" }
    Right user -> do
      -- Check project visibility (don't allow triggering hidden/private projects).
      ok <- liftIO $ withConn pool $ \conn -> do
        accessible <- isProjectAccessible conn project (Just user)
        if accessible
          then triggerJobsetEval conn project jobset >> pure True
          else pure False
      if ok
        then redirect303 (TE.encodeUtf8 $ bp <> "/projects/" <> project <> "/jobsets/" <> jobset)
        else throwError err401 { errBody = "Not authorized" }

-- | POST /build/:id/restart
-- Resets a finished build to queued state so the queue runner re-schedules it.
-- Requires authentication. Redirects back to the build page.
restartBuildHandler :: Maybe Text -> Int -> AppM NoContent
restartBuildHandler mCookie buildId' = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  -- Require login.
  authResult <- liftIO $ requireAuth pool mCookie
  case authResult of
    Left _ -> throwError err401 { errBody = "Login required" }
    Right user -> do
      -- Check project visibility before allowing restart.
      ok <- liftIO $ withConn pool $ \conn -> do
        mProject <- getProjectNameByBuild conn buildId'
        case mProject of
          Nothing -> pure False
          Just projName -> do
            accessible <- isProjectAccessible conn projName (Just user)
            if accessible
              then restartBuild conn buildId'
              else pure False
      if ok
        then redirect303 (TE.encodeUtf8 $ bp <> "/build/" <> showT buildId')
        else throwError err401 { errBody = "Build not found or not restartable" }

-- | Throw a 303 See Other redirect.
redirect303 :: ByteString -> AppM a
redirect303 loc = throwError err303
  { errHeaders = [("Location", loc)]
  , errBody = ""
  }

