-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Legacy URL 301 redirect handlers.
-- Maps old URL structure to new REST-style paths.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Redirect
  ( legacyProjectRedirect
  , legacyJobsetRedirect
  ) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Lucid (Html)
import Servant (err301, errHeaders)

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.View.Components (showT)

-- | /project/:name → 301 → /projects/:name
legacyProjectRedirect :: Text -> AppM (Html ())
legacyProjectRedirect name = do
  bp <- asks (cfgBasePath . appConfig)
  let target = bp <> "/projects/" <> name
  throwError $ err301 { errHeaders = [("Location", TE.encodeUtf8 target)] }

-- | /jobset/:p/:j → 301 → /projects/:p/jobsets/:j
legacyJobsetRedirect :: Text -> Text -> Maybe Int -> AppM (Html ())
legacyJobsetRedirect project jobset mPage = do
  bp <- asks (cfgBasePath . appConfig)
  let base = bp <> "/projects/" <> project <> "/jobsets/" <> jobset
      target = case mPage of
        Nothing -> base
        Just p  -> base <> "?page=" <> showT p
  throwError $ err301 { errHeaders = [("Location", TE.encodeUtf8 target)] }
