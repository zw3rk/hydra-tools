module Lib.GitHub.TokenCache (refreshTokensAt, retainConfiguredOwners) where

import Control.Monad (forM)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)

-- | Keep only cache entries whose owner is configured for this bridge.
retainConfiguredOwners :: (Eq owner) => [owner] -> [(owner, value)] -> [(owner, value)]
retainConfiguredOwners configured = filter ((`elem` configured) . fst)

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
