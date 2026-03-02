-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Profile page: user info, API token management.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Profile
  ( profilePage
  ) where

import Data.Text (Text)
import Lucid

import HydraWeb.Models.User (GFUser (..), GFAPIToken (..))
import HydraWeb.View.Components (fmtTime, showT)

-- | Profile page with user info and API token list.
profilePage :: Text -> GFUser -> [GFAPIToken] -> Maybe Text -> Html ()
profilePage bp user tokens mNewToken = do
  h1_ "Profile"

  -- User info card.
  div_ [class_ "card"] $ do
    h2_ (toHtml $ gfuGitHubLogin user)
    p_ $ do
      "GitHub ID: "
      strong_ (toHtml $ showT (gfuGitHubId user))
    case gfuDisplayName user of
      Just n  -> p_ $ "Display name: " <> toHtml n
      Nothing -> pure ()
    case gfuEmail user of
      Just e  -> p_ $ "Email: " <> toHtml e
      Nothing -> pure ()
    if gfuIsSuperAdmin user
      then p_ [class_ "badge badge-admin"] "Super Admin"
      else pure ()

  -- Flash: show new token once.
  case mNewToken of
    Just t -> div_ [class_ "alert alert-success"] $ do
      p_ $ do
        strong_ "Your new API token (shown only once):"
      p_ [class_ "token-display"] $ code_ (toHtml t)
      p_ "Save this token now — it won't be shown again."
    Nothing -> pure ()

  -- API token management.
  h2_ "API Tokens"
  form_ [method_ "POST", action_ (bp <> "/profile/token")] $ do
    input_ [type_ "text", name_ "label", placeholder_ "Token label", required_ ""]
    button_ [type_ "submit", class_ "btn"] "Create Token"

  if null tokens
    then p_ "No API tokens."
    else table_ $ do
      thead_ $ tr_ $ do
        th_ "Label"
        th_ "Prefix"
        th_ "Created"
        th_ "Last Used"
        th_ "Actions"
      tbody_ $ mapM_ (renderTokenRow bp) tokens

  -- Usage hint.
  details_ $ do
    summary_ "How to use API tokens"
    p_ $ code_ "curl -H 'Authorization: Bearer hydra_...' ..."

renderTokenRow :: Text -> GFAPIToken -> Html ()
renderTokenRow bp t = tr_ $ do
  td_ (toHtml $ gftLabel t)
  td_ (code_ $ toHtml $ gftTokenPrefix t <> "...")
  td_ (toHtml $ fmtTime (gftCreatedAt t))
  td_ $ case gftLastUsedAt t of
    Just ts -> toHtml $ fmtTime ts
    Nothing -> "Never"
  td_ $ form_ [method_ "POST", action_ (bp <> "/profile/token/" <> showT (gftId t) <> "/delete")] $
    button_ [type_ "submit", class_ "btn btn-danger btn-sm"] "Revoke"
