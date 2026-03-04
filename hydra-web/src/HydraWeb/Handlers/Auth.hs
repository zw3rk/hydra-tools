-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | HTTP handlers for GitHub OAuth login, callback, and logout.
-- The callback validates the CSRF state cookie. Logout clears the
-- server-side session from the database.
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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Lucid (Html)
import Servant (err302, err400, err403, err500, ServerError (..))

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..), GitHubConfig (..))
import HydraWeb.Auth.GitHub
  (generateOAuthState, authorizeURL, exchangeCode, fetchGitHubUser, GitHubUser (..))
import HydraWeb.Auth.Middleware (extractSessionId)
import HydraWeb.Auth.Session (createSessionForUser, clearSession, sessionCookieName)
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
-- Validates the CSRF state parameter against the hydra_oauth_state cookie.
handleGitHubCallback :: Maybe Text -> Maybe Text -> Maybe Text -> AppM (Html ())
handleGitHubCallback mCookie mCode mState = do
  code  <- maybe (throwError err400 { errBody = "missing code" }) pure mCode
  state <- maybe (throwError err400 { errBody = "missing state" }) pure mState

  -- Validate CSRF state: the state param must match the hydra_oauth_state cookie.
  let mCookieState = mCookie >>= extractOAuthState
  case mCookieState of
    Nothing -> throwError err403
      { errBody = "CSRF validation failed: missing state cookie" }
    Just cookieState
      | cookieState /= state -> throwError err403
          { errBody = "CSRF validation failed: state mismatch" }
      | otherwise -> pure ()

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

  -- Redirect to home with session cookie. Clear the OAuth state cookie.
  throwError err302
    { errHeaders =
        [ ("Location", TE.encodeUtf8 (bp <> "/"))
        , ("Set-Cookie", sessionCookieName <> "=" <> TE.encodeUtf8 sid
            <> "; Path=/; Max-Age=604800; HttpOnly; SameSite=Strict")
        , ("Set-Cookie", "hydra_oauth_state=; Path=/; Max-Age=0; HttpOnly")
        ]
    , errBody = ""
    }

-- | GET /logout — destroy session in database and clear cookie.
handleLogout :: Maybe Text -> AppM (Html ())
handleLogout mCookie = do
  pool <- asks appPool
  bp   <- asks (cfgBasePath . appConfig)

  -- Clear the server-side session if one exists.
  case mCookie >>= extractSessionId of
    Just sid -> liftIO $ clearSession pool sid
    Nothing  -> pure ()

  -- Clear the session cookie and redirect to home.
  throwError err302
    { errHeaders =
        [ ("Location", TE.encodeUtf8 (bp <> "/"))
        , ("Set-Cookie", sessionCookieName
            <> "=; Path=/; Max-Age=0; HttpOnly; SameSite=Strict")
        ]
    , errBody = ""
    }

-- | Extract the hydra_oauth_state value from a raw Cookie header.
-- Parses "hydra_oauth_state=<value>; ..." from the cookie string.
extractOAuthState :: Text -> Maybe Text
extractOAuthState cookieStr =
  let pairs = map parsePair $ splitCookies cookieStr
  in  lookup ("hydra_oauth_state" :: Text) pairs
  where
    splitCookies = map (Text.strip) . Text.splitOn ";"
    parsePair s = case Text.breakOn "=" s of
      (name, rest) -> (name, Text.drop 1 rest)
