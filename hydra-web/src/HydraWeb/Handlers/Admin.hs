-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Admin dashboard handler: user management, installations, org-map.
-- All routes require super-admin authentication via session cookie.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module HydraWeb.Handlers.Admin
  ( adminServer
  ) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid (Html)
import Servant ((:<|>) (..), ServerError (..))
import Text.Read (readMaybe)

import qualified Data.Text.Encoding as TE

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (requireSuperAdmin)
import HydraWeb.Models.User (GFUser (..))
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Auth (listGFUsers)
import HydraWeb.DB.Installations
  (listInstallations, insertInstallation, toggleInstallation, deleteInstallation)
import HydraWeb.DB.OrgMap
  (listMappings, upsertMapping, autoDetectMappings)
import HydraWeb.DB.Queue (navCounts)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Admin (adminPage, installationsPage, orgMapPage)

-- | All admin routes share a cookie header for super-admin authentication.
-- The cookie is extracted once and threaded to each sub-handler.
adminServer :: Maybe Text
            -> (    AppM (Html ())
               :<|> AppM (Html ())
               :<|> ([(Text, Text)] -> AppM (Html ()))
               :<|> (Int -> AppM (Html ()))
               :<|> (Int -> AppM (Html ()))
               :<|> AppM (Html ())
               :<|> ([(Text, Text)] -> AppM (Html ()))
               :<|> AppM (Html ())
               )
adminServer cookie =
       adminHandler cookie
  :<|> installationsHandler cookie
  :<|> installationsPostHandler cookie
  :<|> installationToggleHandler cookie
  :<|> installationDeleteHandler cookie
  :<|> orgMapHandler cookie
  :<|> orgMapPostHandler cookie
  :<|> orgMapDetectHandler cookie

-- | Verify super-admin access, returning the user or throwing 401/403.
guardAdmin :: Maybe Text -> AppM GFUser
guardAdmin cookie = do
  pool <- asks appPool
  result <- liftIO $ requireSuperAdmin pool cookie
  case result of
    Left err  -> throwError err
    Right u   -> pure u

-- | GET /admin — render the admin dashboard (user management).
adminHandler :: Maybe Text -> AppM (Html ())
adminHandler cookie = do
  user <- guardAdmin cookie
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
        , pdUser     = Just user
        }
  pure $ pageLayout pd $ adminPage bp users

-- | GET /admin/installations — list all GitHub App installations.
installationsHandler :: Maybe Text -> AppM (Html ())
installationsHandler cookie = do
  user <- guardAdmin cookie
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
        , pdUser     = Just user
        }
  pure $ pageLayout pd $ installationsPage bp installs

-- | POST /admin/installations — add a new installation.
installationsPostHandler :: Maybe Text -> [(Text, Text)] -> AppM (Html ())
installationsPostHandler cookie formData = do
  _ <- guardAdmin cookie
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
installationToggleHandler :: Maybe Text -> Int -> AppM (Html ())
installationToggleHandler cookie instId' = do
  _ <- guardAdmin cookie
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  liftIO $ withConn pool $ \conn ->
    toggleInstallation conn instId'
  redirect (bp <> "/admin/installations")

-- | POST /admin/installations/:id/delete — delete an installation.
installationDeleteHandler :: Maybe Text -> Int -> AppM (Html ())
installationDeleteHandler cookie instId' = do
  _ <- guardAdmin cookie
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)
  liftIO $ withConn pool $ \conn ->
    deleteInstallation conn instId'
  redirect (bp <> "/admin/installations")

-- | GET /admin/org-map — list all org/repo mappings.
orgMapHandler :: Maybe Text -> AppM (Html ())
orgMapHandler cookie = do
  user <- guardAdmin cookie
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
        , pdUser     = Just user
        }
  pure $ pageLayout pd $ orgMapPage bp mappings

-- | POST /admin/org-map — add or update a mapping.
orgMapPostHandler :: Maybe Text -> [(Text, Text)] -> AppM (Html ())
orgMapPostHandler cookie formData = do
  _ <- guardAdmin cookie
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
orgMapDetectHandler :: Maybe Text -> AppM (Html ())
orgMapDetectHandler cookie = do
  _ <- guardAdmin cookie
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
