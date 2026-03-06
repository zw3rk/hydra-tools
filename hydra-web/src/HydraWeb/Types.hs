-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Application context and handler monad.
-- The 'App' record holds all shared resources (DB pool, config, SSE hub).
-- Handlers run in 'AppM', a ReaderT over Servant's Handler monad.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HydraWeb.Types
  ( App (..)
  , AppM (..)
  , runAppM
  ) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant (Handler)
import Servant.Server (ServerError)

import Network.HTTP.Client (Manager)

import HydraWeb.Auth.Encrypt (Encryptor)
import HydraWeb.Config (Config)
import HydraWeb.Models.Queue (NavCounts)
import HydraWeb.SSE.Hub (Hub)

-- | Shared application context, threaded through all handlers via ReaderT.
-- The only mutable state is 'appNavCounts', a TVar refreshed every ~10 seconds
-- by the background polling loop. This avoids 4 COUNT(*) queries per page load.
data App = App
  { appPool        :: !(Pool Connection)   -- ^ Database connection pool
  , appConfig      :: !Config              -- ^ Environment-based configuration
  , appSSEHub      :: !(Maybe Hub)         -- ^ SSE broadcast hub (Nothing if SSE disabled)
  , appEncryptor   :: !(Maybe Encryptor)   -- ^ AES-GCM encryption (Nothing if disabled)
  , appHttpManager :: !Manager             -- ^ HTTP client manager for GitHub API
  , appNavCounts   :: !(TVar NavCounts)    -- ^ Cached nav badge counts, refreshed by SSE poller
  }

-- | Handler monad: ReaderT App over Servant's Handler.
-- Use 'asks' to access App fields, 'liftIO' for IO actions.
newtype AppM a = AppM { unAppM :: ReaderT App Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader App,
            MonadError ServerError)

-- | Natural transformation from AppM to Handler for 'hoistServer'.
runAppM :: App -> AppM a -> Handler a
runAppM app (AppM m) = runReaderT m app
