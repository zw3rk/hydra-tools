-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
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

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant (Handler)

import HydraWeb.Config (Config)

-- | Shared application context, threaded through all handlers via ReaderT.
-- No global mutable state: everything is in this record.
data App = App
  { appPool   :: !(Pool Connection)  -- ^ Database connection pool
  , appConfig :: !Config             -- ^ Environment-based configuration
  }

-- | Handler monad: ReaderT App over Servant's Handler.
-- Use 'asks' to access App fields, 'liftIO' for IO actions.
newtype AppM a = AppM { unAppM :: ReaderT App Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader App)

-- | Natural transformation from AppM to Handler for 'hoistServer'.
runAppM :: App -> AppM a -> Handler a
runAppM app (AppM m) = runReaderT m app
