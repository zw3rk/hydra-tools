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
import qualified Data.ByteArray as BA
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
            <> "; Path=/; Max-Age=600; HttpOnly; Secure; SameSite=Lax")
        ]
    , errBody = ""
    }

-- | GET /auth/github/callback?code=...&state=... — OAuth callback.
-- Validates the CSRF state parameter against the hydra_oauth_state cookie.
handleGitHubCallback :: Maybe Text -> Maybe Text -> Maybe Text -> AppM (Html ())
handleGitHubCallback mCookie mCode mState = do
  code  <- maybe (throwError err400 { errBody = "authentication failed" }) pure mCode
  state <- maybe (throwError err400 { errBody = "authentication failed" }) pure mState

  -- Validate CSRF state: the state param must match the hydra_oauth_state cookie.
  -- Uses constant-time comparison to prevent timing attacks on the state value.
  let mCookieState = mCookie >>= extractOAuthState
  case mCookieState of
    Nothing -> throwError err403
      { errBody = "authentication failed" }
    Just cookieState
      | Text.null cookieState || Text.null state -> throwError err403
          { errBody = "authentication failed" }
      | not (BA.constEq (TE.encodeUtf8 cookieState) (TE.encodeUtf8 state)) ->
          throwError err403 { errBody = "authentication failed" }
      | otherwise -> pure ()

  cfg <- asks appConfig
  bp  <- asks (cfgBasePath . appConfig)
  let gh = cfgGitHub cfg
  pool <- asks appPool
  mEnc <- asks appEncryptor

  -- Exchange code for access token.
  -- The redirect_uri must match the one sent during authorization.
  let callbackURL = cfgBaseURL cfg <> bp <> "/auth/github/callback"
  mgr <- asks appHttpManager
  mToken <- liftIO $ exchangeCode mgr (ghClientID gh) (ghClientSecret gh) code callbackURL
  token <- maybe (throwError err500 { errBody = "authentication failed" }) pure mToken

  -- Fetch GitHub user info.
  mGHUser <- liftIO $ fetchGitHubUser mgr token
  ghUser  <- maybe (throwError err500 { errBody = "authentication failed" }) pure mGHUser

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
            <> "; Path=/; Max-Age=604800; HttpOnly; Secure; SameSite=Strict")
        , ("Set-Cookie", "hydra_oauth_state=; Path=/; Max-Age=0; HttpOnly; Secure")
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
            <> "=; Path=/; Max-Age=0; HttpOnly; Secure; SameSite=Strict")
        ]
    , errBody = ""
    }

-- | Extract the hydra_oauth_state value from a raw Cookie header.
-- Parses "hydra_oauth_state=<value>; ..." from the cookie string.
-- Returns Nothing if the value is missing or empty.
extractOAuthState :: Text -> Maybe Text
extractOAuthState cookieStr =
  case lookup ("hydra_oauth_state" :: Text) pairs of
    Just v | not (Text.null v) -> Just v
    _                          -> Nothing
  where
    pairs = map parsePair $ splitCookies cookieStr
    splitCookies = map Text.strip . Text.splitOn ";"
    parsePair s = case Text.breakOn "=" s of
      (name, rest) -> (name, Text.drop 1 rest)
