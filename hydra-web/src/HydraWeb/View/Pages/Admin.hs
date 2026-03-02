-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Admin dashboard page: user management.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Admin
  ( adminPage
  ) where

import Data.Text (Text)
import Lucid

import HydraWeb.Models.User (GFUser (..))
import HydraWeb.View.Components (fmtTime, showT)

-- | Admin page listing all users with super-admin toggle.
adminPage :: Text -> [GFUser] -> Html ()
adminPage bp users = do
  h1_ "Admin Dashboard"

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
