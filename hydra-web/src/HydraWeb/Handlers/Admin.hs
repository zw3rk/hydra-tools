-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Admin dashboard handler: user management, installations, org-map.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Admin
  ( adminHandler
  , installationsHandler
  , installationsPostHandler
  , installationToggleHandler
  , installationDeleteHandler
  , orgMapHandler
  , orgMapPostHandler
  , orgMapDetectHandler
  ) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid (Html)
import Servant (ServerError (..))
import Text.Read (readMaybe)

import qualified Data.Text.Encoding as TE

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Auth (listGFUsers)
import HydraWeb.DB.Installations
  (listInstallations, insertInstallation, toggleInstallation, deleteInstallation)
import HydraWeb.DB.OrgMap
  (listMappings, upsertMapping, autoDetectMappings)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Admin (adminPage, installationsPage, orgMapPage)

-- | GET /admin — render the admin dashboard (user management).
-- TODO: requires super-admin authentication check.
adminHandler :: AppM (Html ())
adminHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (users, counts) <- liftIO $ withConn pool $ \conn -> do
    us <- listGFUsers conn
    nc <- navCounts conn
    pure (us, nc)
  let pd = PageData
        { pdTitle    = "Admin Dashboard"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ adminPage bp users

-- | GET /admin/installations — list all GitHub App installations.
installationsHandler :: AppM (Html ())
installationsHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (installs, counts) <- liftIO $ withConn pool $ \conn -> do
    is <- listInstallations conn
    nc <- navCounts conn
    pure (is, nc)
  let pd = PageData
        { pdTitle    = "GitHub Installations"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ installationsPage bp installs

-- | POST /admin/installations — add a new installation.
installationsPostHandler :: [(Text, Text)] -> AppM (Html ())
installationsPostHandler formData = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  let mOrg = lookup "org_name" formData
      mId  = lookup "installation_id" formData >>= (readMaybe . Text.unpack)
  case (mOrg, mId) of
    (Just org, Just iid) -> do
      liftIO $ withConn pool $ \conn ->
        insertInstallation conn org iid
      redirect (bp <> "/admin/installations")
    _ -> redirect (bp <> "/admin/installations")

-- | POST /admin/installations/:id/toggle — toggle enabled state.
installationToggleHandler :: Int -> AppM (Html ())
installationToggleHandler instId' = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  liftIO $ withConn pool $ \conn ->
    toggleInstallation conn instId'
  redirect (bp <> "/admin/installations")

-- | POST /admin/installations/:id/delete — delete an installation.
installationDeleteHandler :: Int -> AppM (Html ())
installationDeleteHandler instId' = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  liftIO $ withConn pool $ \conn ->
    deleteInstallation conn instId'
  redirect (bp <> "/admin/installations")

-- | GET /admin/org-map — list all org/repo mappings.
orgMapHandler :: AppM (Html ())
orgMapHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  (mappings, counts) <- liftIO $ withConn pool $ \conn -> do
    ms <- listMappings conn
    nc <- navCounts conn
    pure (ms, nc)
  let pd = PageData
        { pdTitle    = "Org/Repo Mappings"
        , pdBasePath = bp
        , pdCounts   = counts
        }
  pure $ pageLayout pd $ orgMapPage bp mappings

-- | POST /admin/org-map — add or update a mapping.
orgMapPostHandler :: [(Text, Text)] -> AppM (Html ())
orgMapPostHandler formData = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  let mProject = lookup "project_name" formData
      mOrg     = lookup "org_name" formData
      mRepo    = lookup "repo_name" formData
  case (mProject, mOrg, mRepo) of
    (Just proj, Just org, Just repo)
      | not (Text.null proj) && not (Text.null org) && not (Text.null repo) -> do
        liftIO $ withConn pool $ \conn ->
          upsertMapping conn proj org repo False
        redirect (bp <> "/admin/org-map")
    _ -> redirect (bp <> "/admin/org-map")

-- | POST /admin/org-map/detect — auto-detect mappings from flake URIs.
orgMapDetectHandler :: AppM (Html ())
orgMapDetectHandler = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  liftIO $ withConn pool autoDetectMappings
  redirect (bp <> "/admin/org-map")

-- | Issue a 303 See Other redirect (POST-redirect-GET pattern).
redirect :: Text -> AppM a
redirect target =
  throwError $ ServerError
    { errHTTPCode = 303
    , errReasonPhrase = "See Other"
    , errBody = ""
    , errHeaders = [("Location", TE.encodeUtf8 target)]
    }
