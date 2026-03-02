-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Login page view.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.View.Pages.Login
  ( loginPage
  ) where

import Data.Text (Text)
import Lucid

-- | Simple login page with a GitHub OAuth button.
loginPage :: Text -> Html ()
loginPage bp = do
  h1_ "Login"
  p_ "Sign in with your GitHub account to access private projects."
  a_ [ href_ (bp <> "/auth/github")
     , class_ "btn btn-primary"
     ] "Sign in with GitHub"
