-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Application context and handler monad.
-- The 'App' record holds all shared resources (DB pool, config, SSE hub).
-- Handlers run in 'AppM', a ReaderT over Servant's Handler monad.
module HydraWeb.Types
  ( App (..),
    AppM (..),
    runAppM,
  )
where

import Control.Concurrent.STM (TVar)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import HydraWeb.Auth.Encrypt (Encryptor)
import HydraWeb.Config (Config)
import HydraWeb.Models.Queue (NavCounts)
import HydraWeb.SSE.Hub (Hub)
import Network.HTTP.Client (Manager)
import Servant (Handler)
import Servant.Server (ServerError)

-- | Shared application context, threaded through all handlers via ReaderT.
-- The only mutable state is 'appNavCounts', a TVar refreshed every ~10 seconds
-- by the background polling loop. This avoids 4 COUNT(*) queries per page load.
data App = App
  { -- | Database connection pool
    appPool :: !(Pool Connection),
    -- | Environment-based configuration
    appConfig :: !Config,
    -- | SSE broadcast hub (Nothing if SSE disabled)
    appSSEHub :: !(Maybe Hub),
    -- | AES-GCM encryption (Nothing if disabled)
    appEncryptor :: !(Maybe Encryptor),
    -- | HTTP client manager for GitHub API
    appHttpManager :: !Manager,
    -- | Cached nav badge counts, refreshed by SSE poller
    appNavCounts :: !(TVar NavCounts)
  }

-- | Handler monad: ReaderT App over Servant's Handler.
-- Use 'asks' to access App fields, 'liftIO' for IO actions.
newtype AppM a = AppM {unAppM :: ReaderT App Handler a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader App,
      MonadError ServerError
    )

-- | Natural transformation from AppM to Handler for 'hoistServer'.
runAppM :: App -> AppM a -> Handler a
runAppM app (AppM m) = runReaderT m app
