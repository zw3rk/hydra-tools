-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
--
-- | PostgreSQL connection pool creation and management.
-- Uses resource-pool for bounded connection reuse.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.DB.Pool
  ( createPool
  , withConn
  ) where

import Data.ByteString.Char8 (pack)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)

-- | Create a connection pool from a PostgreSQL connection string.
-- Pool configuration: 2 stripes, 300s idle timeout, 20 max connections.
createPool :: Text -> IO (Pool Connection)
createPool connStr =
  Pool.newPool Pool.defaultPoolConfig
    { Pool.newResource    = connectPostgreSQL (pack $ Text.unpack connStr)
    , Pool.freeResource   = close
    , Pool.poolCacheTTL   = 300  -- 5 minutes idle timeout
    , Pool.poolMaxResources = 20
    }

-- | Run an action with a connection from the pool.
withConn :: Pool Connection -> (Connection -> IO a) -> IO a
withConn = Pool.withResource
