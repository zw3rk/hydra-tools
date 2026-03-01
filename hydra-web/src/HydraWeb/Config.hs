-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Environment-based configuration for hydra-web.
-- All settings are read from environment variables with sensible defaults,
-- matching the Go hydra-web's HYDRA_WEB_* variable names exactly.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Config
  ( Config (..)
  , GitHubConfig (..)
  , loadConfig
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Application configuration. All fields correspond to HYDRA_WEB_* env vars.
data Config = Config
  { cfgListenAddr      :: !Text          -- ^ HYDRA_WEB_LISTEN (default "127.0.0.1:4000")
  , cfgBaseURL         :: !Text          -- ^ HYDRA_WEB_BASE_URL
  , cfgBasePath        :: !Text          -- ^ HYDRA_WEB_BASE_PATH (URL prefix for reverse proxy)
  , cfgDatabaseURL     :: !Text          -- ^ HYDRA_WEB_DATABASE_URL
  , cfgHydraBackendURL :: !Text          -- ^ HYDRA_WEB_HYDRA_BACKEND (upstream Hydra for proxied writes)
  , cfgStaticDir       :: !FilePath      -- ^ HYDRA_WEB_STATIC_DIR
  , cfgSessionSecret   :: !Text          -- ^ HYDRA_WEB_SESSION_SECRET
  , cfgEncryptionKey   :: !Text          -- ^ HYDRA_WEB_ENCRYPTION_KEY (AES-256-GCM)
  , cfgSuperAdmins     :: ![Text]        -- ^ HYDRA_WEB_SUPER_ADMINS (comma-separated)
  , cfgGitHub          :: !GitHubConfig  -- ^ GitHub App/OAuth settings
  } deriving (Show)

-- | GitHub App and OAuth configuration.
data GitHubConfig = GitHubConfig
  { ghAppID           :: !Int            -- ^ HYDRA_WEB_GITHUB_APP_ID
  , ghAppKeyFile      :: !FilePath       -- ^ HYDRA_WEB_GITHUB_APP_KEY_FILE
  , ghClientID        :: !Text           -- ^ HYDRA_WEB_GITHUB_CLIENT_ID
  , ghClientSecret    :: !Text           -- ^ HYDRA_WEB_GITHUB_CLIENT_SECRET
  , ghInstallationIDs :: ![(Text, Int)]  -- ^ HYDRA_WEB_GITHUB_INSTALLATION_IDS ("org=id,...")
  } deriving (Show)

-- | Load configuration from environment variables.
loadConfig :: IO Config
loadConfig = do
  listen     <- envOr "HYDRA_WEB_LISTEN" "127.0.0.1:4000"
  baseURL    <- envOr "HYDRA_WEB_BASE_URL" "http://localhost:4000"
  basePath   <- Text.dropWhileEnd (== '/') <$> envOr "HYDRA_WEB_BASE_PATH" ""
  dbURL      <- envOr "HYDRA_WEB_DATABASE_URL" "postgres://hydra-web@/hydra?host=/run/postgresql"
  backend    <- envOr "HYDRA_WEB_HYDRA_BACKEND" "http://127.0.0.1:3000"
  staticDir  <- envOrStr "HYDRA_WEB_STATIC_DIR" "static"
  secret     <- envOr "HYDRA_WEB_SESSION_SECRET" ""
  encKey     <- envOr "HYDRA_WEB_ENCRYPTION_KEY" ""
  admins     <- parseSuperAdmins <$> envOr "HYDRA_WEB_SUPER_ADMINS" ""
  gh         <- loadGitHubConfig
  pure Config
    { cfgListenAddr      = listen
    , cfgBaseURL         = baseURL
    , cfgBasePath        = basePath
    , cfgDatabaseURL     = dbURL
    , cfgHydraBackendURL = backend
    , cfgStaticDir       = staticDir
    , cfgSessionSecret   = secret
    , cfgEncryptionKey   = encKey
    , cfgSuperAdmins     = admins
    , cfgGitHub          = gh
    }

loadGitHubConfig :: IO GitHubConfig
loadGitHubConfig = do
  appID      <- maybe 0 id . (>>= readMaybe) <$> lookupEnv "HYDRA_WEB_GITHUB_APP_ID"
  keyFile    <- envOrStr "HYDRA_WEB_GITHUB_APP_KEY_FILE" ""
  clientID   <- envOr "HYDRA_WEB_GITHUB_CLIENT_ID" ""
  clientSec  <- envOr "HYDRA_WEB_GITHUB_CLIENT_SECRET" ""
  instIDs    <- parseInstallationIDs <$> envOr "HYDRA_WEB_GITHUB_INSTALLATION_IDS" ""
  pure GitHubConfig
    { ghAppID           = appID
    , ghAppKeyFile      = keyFile
    , ghClientID        = clientID
    , ghClientSecret    = clientSec
    , ghInstallationIDs = instIDs
    }

-- | Parse comma-separated "org=id" pairs.
parseInstallationIDs :: Text -> [(Text, Int)]
parseInstallationIDs t
  | Text.null t = []
  | otherwise = concatMap parsePair $ Text.splitOn "," t
  where
    parsePair s = case Text.splitOn "=" (Text.strip s) of
      [org, idStr] | Just n <- readMaybe (Text.unpack $ Text.strip idStr) -> [(Text.strip org, n)]
      _ -> []

-- | Parse comma-separated admin usernames.
parseSuperAdmins :: Text -> [Text]
parseSuperAdmins t
  | Text.null t = []
  | otherwise = filter (not . Text.null) . map Text.strip $ Text.splitOn "," t

-- | Read a Text env var with a default.
envOr :: String -> Text -> IO Text
envOr key def = maybe def Text.pack <$> lookupEnv key

-- | Read a String env var with a default.
envOrStr :: String -> String -> IO String
envOrStr key def = fromMaybe def <$> lookupEnv key
