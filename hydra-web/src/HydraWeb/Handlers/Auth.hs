-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | HTTP handlers for GitHub OAuth login, callback, and logout.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Auth
  ( handleLogin
  , handleGitHubAuth
  , handleGitHubCallback
  , handleLogout
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Error.Class (throwError)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Lucid (Html)
import Servant (err302, err400, err500, ServerError (..))

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..), GitHubConfig (..))
import HydraWeb.Auth.GitHub
  (generateOAuthState, authorizeURL, exchangeCode, fetchGitHubUser, GitHubUser (..))
import HydraWeb.Auth.Session (createSessionForUser, sessionCookieName)
import HydraWeb.Auth.Encrypt (encrypt)
import HydraWeb.DB.Auth (upsertGFUser, upsertGitHubToken)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.View.Pages.Login (loginPage)

-- | GET /login — render the login page.
handleLogin :: AppM (Html ())
handleLogin = do
  bp <- asks (cfgBasePath . appConfig)
  pure $ loginPage bp

-- | GET /auth/github — start the OAuth flow: redirect to GitHub.
handleGitHubAuth :: AppM (Html ())
handleGitHubAuth = do
  cfg <- asks appConfig
  bp  <- asks (cfgBasePath . appConfig)
  let gh = cfgGitHub cfg
      callbackURL = cfgBaseURL cfg <> bp <> "/auth/github/callback"
  state <- liftIO generateOAuthState
  let url = authorizeURL (ghClientID gh) callbackURL state
  -- Set CSRF state cookie and redirect to GitHub.
  throwError err302
    { errHeaders =
        [ ("Location", TE.encodeUtf8 url)
        , ("Set-Cookie", "hydra_oauth_state=" <> TE.encodeUtf8 state
            <> "; Path=/; Max-Age=600; HttpOnly; SameSite=Lax")
        ]
    , errBody = ""
    }

-- | GET /auth/github/callback?code=...&state=... — OAuth callback.
handleGitHubCallback :: Maybe Text -> Maybe Text -> AppM (Html ())
handleGitHubCallback mCode mState = do
  code  <- maybe (throwError err400 { errBody = "missing code" }) pure mCode
  _state <- maybe (throwError err400 { errBody = "missing state" }) pure mState
  -- TODO: verify state matches cookie (requires access to request cookies)

  cfg <- asks appConfig
  bp  <- asks (cfgBasePath . appConfig)
  let gh = cfgGitHub cfg
  pool <- asks appPool
  mEnc <- asks appEncryptor

  -- Exchange code for access token.
  mgr <- asks appHttpManager
  mToken <- liftIO $ exchangeCode mgr (ghClientID gh) (ghClientSecret gh) code
  token <- maybe (throwError err500 { errBody = "token exchange failed" }) pure mToken

  -- Fetch GitHub user info.
  mGHUser <- liftIO $ fetchGitHubUser mgr token
  ghUser  <- maybe (throwError err500 { errBody = "user fetch failed" }) pure mGHUser

  -- Upsert user in database.
  userId <- liftIO $ withConn pool $ \conn -> do
    uid <- upsertGFUser conn (ghuId ghUser) (ghuLogin ghUser)
                        (ghuName ghUser) (ghuEmail ghUser) (ghuAvatarURL ghUser)
    -- Store encrypted GitHub token.
    case mEnc of
      Just enc -> do
        encToken <- encrypt enc (TE.encodeUtf8 token)
        upsertGitHubToken conn uid encToken
      Nothing ->
        upsertGitHubToken conn uid (TE.encodeUtf8 token)
    pure uid

  -- Create session.
  sid <- liftIO $ createSessionForUser pool userId

  -- Redirect to home with session cookie.
  throwError err302
    { errHeaders =
        [ ("Location", TE.encodeUtf8 (bp <> "/"))
        , ("Set-Cookie", sessionCookieName <> "=" <> TE.encodeUtf8 sid
            <> "; Path=/; Max-Age=604800; HttpOnly; SameSite=Strict")
        ]
    , errBody = ""
    }

-- | GET /logout — destroy session and redirect to home.
handleLogout :: AppM (Html ())
handleLogout = do
  bp   <- asks (cfgBasePath . appConfig)
  -- TODO: extract session from cookie, clear it
  -- For now, just clear the cookie and redirect.
  throwError err302
    { errHeaders =
        [ ("Location", TE.encodeUtf8 (bp <> "/"))
        , ("Set-Cookie", sessionCookieName
            <> "=; Path=/; Max-Age=0; HttpOnly; SameSite=Strict")
        ]
    , errBody = ""
    }
