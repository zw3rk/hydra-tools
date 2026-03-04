-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Admin dashboard pages: user management, installations, org-map.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Admin
  ( adminPage
  , installationsPage
  , orgMapPage
  ) where

import Data.Text (Text)
import Lucid

import HydraWeb.Models.User (GFUser (..))
import HydraWeb.DB.Installations (Installation (..))
import HydraWeb.DB.OrgMap (OrgMapping (..))
import HydraWeb.View.Components (fmtTime, showT)

-- | Admin page listing all users with super-admin toggle.
adminPage :: Text -> [GFUser] -> Html ()
adminPage bp users = do
  h1_ "Admin Dashboard"

  -- Navigation tabs.
  adminNav bp "users"

  if null users
    then p_ "No registered users."
    else table_ $ do
      thead_ $ tr_ $ do
        th_ "GitHub Login"
        th_ "Display Name"
        th_ "Super Admin"
        th_ "Joined"
        th_ "Actions"
      tbody_ $ mapM_ (renderUserRow bp) users

-- | GitHub App installations management page.
installationsPage :: Text -> [Installation] -> Html ()
installationsPage bp installs = do
  h1_ "GitHub App Installations"
  adminNav bp "installations"

  -- Add form.
  h2_ "Add Installation"
  form_ [method_ "POST", action_ (bp <> "/admin/installations")] $ do
    fieldset_ $ do
      label_ [for_ "org_name"] "Organisation"
      input_ [type_ "text", name_ "org_name", id_ "org_name"
             , placeholder_ "e.g. input-output-hk", required_ ""]
      label_ [for_ "installation_id"] "Installation ID"
      input_ [type_ "number", name_ "installation_id", id_ "installation_id"
             , placeholder_ "e.g. 12345678", required_ ""]
    button_ [type_ "submit"] "Add Installation"

  -- Existing installations.
  if null installs
    then p_ "No installations configured."
    else table_ $ do
      thead_ $ tr_ $ do
        th_ "Organisation"
        th_ "Installation ID"
        th_ "Enabled"
        th_ "Actions"
      tbody_ $ mapM_ (renderInstallRow bp) installs

-- | Org/repo mapping management page.
orgMapPage :: Text -> [OrgMapping] -> Html ()
orgMapPage bp mappings = do
  h1_ "Org/Repo Mappings"
  adminNav bp "org-map"

  -- Auto-detect button.
  form_ [method_ "POST", action_ (bp <> "/admin/org-map/detect"), class_ "inline-form"] $
    button_ [type_ "submit", class_ "outline"] "Auto-detect from Flake URIs"

  -- Add/update form.
  h2_ "Add Mapping"
  form_ [method_ "POST", action_ (bp <> "/admin/org-map")] $ do
    fieldset_ $ do
      label_ [for_ "project_name"] "Project Name"
      input_ [type_ "text", name_ "project_name", id_ "project_name"
             , placeholder_ "e.g. cardano-node", required_ ""]
      label_ [for_ "org_name"] "GitHub Org"
      input_ [type_ "text", name_ "org_name", id_ "org_name"
             , placeholder_ "e.g. IntersectMBO", required_ ""]
      label_ [for_ "repo_name"] "GitHub Repo"
      input_ [type_ "text", name_ "repo_name", id_ "repo_name"
             , placeholder_ "e.g. cardano-node", required_ ""]
    button_ [type_ "submit"] "Save Mapping"

  -- Existing mappings.
  if null mappings
    then p_ "No mappings configured. Click 'Auto-detect' to scan flake URIs."
    else table_ $ do
      thead_ $ tr_ $ do
        th_ "Project"
        th_ "Org"
        th_ "Repo"
        th_ "Auto-detected"
        th_ "Created"
      tbody_ $ mapM_ renderMappingRow mappings

-- ── Internal helpers ──────────────────────────────────────────────────

-- | Admin navigation tabs.
adminNav :: Text -> Text -> Html ()
adminNav bp active = nav_ [class_ "admin-nav"] $ ul_ $ do
  tabLink bp "users" "Users" active
  tabLink bp "installations" "Installations" active
  tabLink bp "org-map" "Org Mappings" active
  where
    tabLink :: Text -> Text -> Text -> Text -> Html ()
    tabLink bp' tab label current =
      let url' = case tab of
            "users" -> bp' <> "/admin"
            _       -> bp' <> "/admin/" <> tab
          cls = if tab == current then "active" else ""
      in  li_ $ a_ [href_ url', class_ cls] (toHtml label)

renderUserRow :: Text -> GFUser -> Html ()
renderUserRow bp u = tr_ $ do
  td_ $ do
    case gfuAvatarURL u of
      Just url -> img_ [src_ (url <> "&s=24"), width_ "24", height_ "24"]
      Nothing  -> pure ()
    " "
    toHtml (gfuGitHubLogin u)
  td_ $ toHtml $ maybe "" id (gfuDisplayName u)
  td_ $ if gfuIsSuperAdmin u then "Yes" else "No"
  td_ (toHtml $ fmtTime (gfuCreatedAt u))
  td_ $ form_ [method_ "POST", action_ (bp <> "/admin/toggle-admin/" <> showT (gfuId u))] $
    button_ [type_ "submit", class_ "btn btn-sm"] $
      if gfuIsSuperAdmin u then "Revoke Admin" else "Grant Admin"

renderInstallRow :: Text -> Installation -> Html ()
renderInstallRow bp inst = tr_ $ do
  td_ $ toHtml (instOrgName inst)
  td_ $ toHtml (showT (instInstallationId inst))
  td_ $ if instEnabled inst then "Yes" else "No"
  td_ $ do
    form_ [method_ "POST", action_ (bp <> "/admin/installations/" <> showT (instId inst) <> "/toggle")
          , class_ "inline-form"] $
      button_ [type_ "submit", class_ "btn btn-sm outline"] $
        if instEnabled inst then "Disable" else "Enable"
    " "
    form_ [method_ "POST", action_ (bp <> "/admin/installations/" <> showT (instId inst) <> "/delete")
          , class_ "inline-form"] $
      button_ [type_ "submit", class_ "btn btn-sm outline secondary"] "Delete"

renderMappingRow :: OrgMapping -> Html ()
renderMappingRow m = tr_ $ do
  td_ $ toHtml (omProjectName m)
  td_ $ toHtml (omOrgName m)
  td_ $ toHtml (omRepoName m)
  td_ $ if omAutoDetected m then "auto" else "manual"
  td_ $ toHtml (fmtTime (omCreatedAt m))
