module Lib.GitHub.TokenCache (refreshTokensAt) where

import Control.Monad (forM)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)

-- | Keep valid leases and renew expiring leases using their own origin metadata.
-- The owner key labels the cache entry; it need not belong to a webhook allowlist.
-- A failed refresh propagates without replacing the cached leases.
refreshTokensAt ::
  UTCTime ->
  NominalDiffTime ->
  (lease -> Maybe UTCTime) ->
  IORef [(owner, lease)] ->
  (lease -> IO lease) ->
  IO [(owner, lease)]
refreshTokensAt now buffer expiresAt cache refresh = do
  leases <- readIORef cache
  refreshed <- forM leases $ \(owner, lease) -> do
    renewed <- case expiresAt lease of
      Just expiry | addUTCTime buffer now < expiry -> pure lease
      _ -> refresh lease
    pure (owner, renewed)
  writeIORef cache refreshed
  pure refreshed
