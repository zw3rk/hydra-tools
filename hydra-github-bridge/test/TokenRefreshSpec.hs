module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Lib.GitHub.TokenCache (refreshTokensAt, retainConfiguredOwners)

data Lease = Lease
  { installationId :: Int,
    expiry :: Maybe UTCTime,
    generation :: Int
  }
  deriving (Eq, Show)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (actual == expected) $
    fail $
      label ++ ": expected " ++ show expected ++ ", got " ++ show actual

main :: IO ()
main = do
  let now = UTCTime (fromGregorian 2026 10 16) 0
      valid = Lease 1 (Just $ addUTCTime 3600 now) 0
      expired = Lease 2 (Just $ addUTCTime (-1) now) 0
      atBuffer = Lease 3 (Just $ addUTCTime 5 now) 0
      noExpiry = Lease 4 Nothing 0
      initial =
        [ ("simplex-chat", valid),
          -- Discovered App installation absent from GITHUB_APP_INSTALL_IDS.
          ("kolfadser1", expired),
          ("at-buffer", atBuffer),
          ("no-expiry", noExpiry)
        ]
      renewed lease = lease {expiry = Just $ addUTCTime 3600 now, generation = 1}
      expected = [(owner, if owner == "simplex-chat" then lease else renewed lease) | (owner, lease) <- initial]
  calls <- newIORef []
  cache <- newIORef initial
  let refresh lease = do
        modifyIORef' calls (++ [installationId lease])
        pure $ renewed lease
      runRefresh = refreshTokensAt now 5 expiry cache refresh
  result <- runRefresh
  assertEqual "all owners survive refresh" expected result
  assertEqual "renew original installation IDs only" [2, 3, 4] =<< readIORef calls
  assertEqual "cache receives renewed leases" expected =<< readIORef cache
  assertEqual "valid leases reused on next call" expected =<< runRefresh
  assertEqual "no duplicate refresh" [2, 3, 4] =<< readIORef calls

  let configuredOwners = ["simplex-chat", "at-buffer", "no-expiry"]
      configuredInitial = retainConfiguredOwners configuredOwners initial
      configuredExpected = [(owner, if owner == "simplex-chat" then lease else renewed lease) | (owner, lease) <- configuredInitial]
  configuredCalls <- newIORef []
  configuredCache <- newIORef configuredInitial
  configuredResult <- refreshTokensAt now 5 expiry configuredCache $ \lease -> do
    modifyIORef' configuredCalls (++ [installationId lease])
    pure $ renewed lease
  assertEqual "unconfigured installations are excluded before refresh" configuredExpected configuredResult
  assertEqual "configured leases refresh their original installation IDs" [3, 4] =<< readIORef configuredCalls
  assertEqual "configured token cache keeps only renewed configured owners" configuredExpected =<< readIORef configuredCache

  failingCache <- newIORef [("kolfadser1", expired)]
  failed <- try $ refreshTokensAt now 5 expiry failingCache (\_ -> ioError $ userError "refresh failed")
  case failed :: Either IOException [(String, Lease)] of
    Left _ -> pure ()
    Right value -> fail $ "refresh error swallowed: " ++ show value
  assertEqual "failed refresh preserves cache" [("kolfadser1", expired)] =<< readIORef failingCache
  putStrLn "token-refresh: PASS"
