{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception
  ( SomeException,
    catch,
    displayException,
  )
import Control.Monad (forM, forM_, forever, replicateM_, unless, void, when)
import Data.List (partition)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
    stdin,
    stdout,
  )
import System.Process
  ( readCreateProcessWithExitCode,
    shell,
  )
import Text.Read (readMaybe)

-- | Maximum number of retries before permanently deleting an entry.
-- At 1.5^10 * 10 min ≈ 9.6 hours for the last retry.  Entries beyond
-- this threshold will never succeed (the store path was garbage
-- collected from all builders).
maxRetries :: Int
maxRetries = 10

-- | Push a store path to attic.  On success the row is deleted;
-- on failure an exponential backoff is applied.
pushToAttic :: Connection -> String -> Int -> String -> IO ()
pushToAttic conn cache rowId drvPath = do
  (exitCode, _, errOutput) <- readCreateProcessWithExitCode (shell $ "attic push " ++ cache ++ " " ++ drvPath) ""
  case exitCode of
    ExitFailure code -> do
      putStrLn $ "Ran: attic push " ++ cache ++ " " ++ drvPath
      putStrLn $ "Attic push failed with exit code " ++ show code
      unless (null errOutput) $ putStrLn $ "Error output:\n" ++ errOutput
      -- Exponential backoff: 5 min * 1.5^tries, capped at 30 days.
      _ <- execute conn "UPDATE DrvpathsToUpload SET last = NOW() + (interval '5 minutes' * least(8640, 1.5 ^ tries)), tries = tries + 1 WHERE id = ?;" (Only rowId)
      pure ()
    ExitSuccess -> do
      _ <- execute conn "DELETE FROM DrvpathsToUpload WHERE id = ?;" (Only rowId)
      pure ()

-- | Take up to @batch@ due paths, newest first, and push the ones in the
-- local store with one @attic push@.
--
-- One push per path was too slow to keep up with haskell.nix: each
-- @attic push@ computes the path's closure and asks the server which of
-- its thousands of paths are missing before uploading anything, and the
-- paths a build produces share most of that closure.  A batch pays for
-- that once.  Newest first, so that the outputs current builds need reach
-- the cache rather than waiting behind a backlog.
processDrvPath :: Connection -> String -> Int -> IO ()
processDrvPath conn cache batch = do
  more <- withTransaction conn $ do
    -- Select by primary key so that DELETE/UPDATE below only touches
    -- these rows, eliminating deadlocks from duplicate drvpath entries.
    rows <-
      query
        conn
        "SELECT id, drvpath FROM DrvpathsToUpload WHERE last < NOW() AND tries < ? ORDER BY id DESC FOR UPDATE SKIP LOCKED LIMIT ?;"
        (maxRetries, batch) ::
        IO [(Int, String)]
    if null rows
      then pure False
      else do
        -- Fast local check: which of them are not in our store?  Prints
        -- the invalid paths and never consults substituters.
        (_, invalidOut, _) <- readCreateProcessWithExitCode (shell $ "nix-store --check-validity --print-invalid " ++ unwords (map snd rows) ++ " 2>/dev/null") ""
        let invalid = lines invalidOut
            (missing, local) = partition ((`elem` invalid) . snd) rows
        fetched <- fmap concat $ forM missing $ \(rowId, drvPath) -> do
          -- Path not in local store.  Try fetching from LAN peers
          -- via peernix / configured substituters before giving up.
          -- nix-store --realise checks all configured substituters
          -- and downloads if found — it does NOT rebuild.
          putStrLn $ "Path not local, fetching from peers: " ++ drvPath
          (fetchExit, _, fetchErr) <- readCreateProcessWithExitCode (shell $ "nix-store --realise " ++ drvPath ++ " >/dev/null 2>&1") ""
          case fetchExit of
            ExitSuccess -> do
              putStrLn $ "Fetched from peer: " ++ drvPath
              pure [(rowId, drvPath)]
            ExitFailure _ -> do
              -- Path truly unavailable anywhere on the network.
              -- Exponential backoff: 10 min * 1.5^tries, capped at
              -- 30 days.  These paths were likely GC'd from all
              -- builders and will never reappear.
              putStrLn $ "Path not available on any peer, deferring: " ++ drvPath
              unless (null fetchErr) $ putStrLn $ "Fetch error: " ++ fetchErr
              _ <- execute conn "UPDATE DrvpathsToUpload SET last = NOW() + (interval '10 minutes' * least(4320, 1.5 ^ tries)), tries = tries + 1 WHERE id = ?;" (Only rowId)
              pure []
        let toPush = local ++ fetched
        unless (null toPush) $ do
          (exitCode, _, _) <- readCreateProcessWithExitCode (shell $ "attic push " ++ cache ++ " " ++ unwords (map snd toPush)) ""
          case exitCode of
            ExitSuccess -> do
              _ <- execute conn "DELETE FROM DrvpathsToUpload WHERE id IN ?;" (Only (In (map fst toPush)))
              putStrLn $ "Pushed " ++ show (length toPush) ++ " paths"
            -- Push them one by one to find the failing ones (what the
            -- batch did upload is skipped as already on the server).
            ExitFailure _ -> forM_ toPush $ \(rowId, drvPath) -> pushToAttic conn cache rowId drvPath
        pure True
  when more (processDrvPath conn cache batch)

-- | Delete entries that have exhausted all retries.  These paths are
-- permanently unreachable and keeping them around just clutters the
-- queue / dashboard.  Returns the number of rows deleted.
cleanupExhausted :: Connection -> IO Int
cleanupExhausted conn = do
  n <- execute conn "DELETE FROM DrvpathsToUpload WHERE tries >= ?;" (Only maxRetries)
  pure (fromIntegral n)

-- | Periodically clean up exhausted entries.  Runs every hour.
cleanupLoop :: ConnectInfo -> IO ()
cleanupLoop connectInfo =
  withConnect connectInfo $ \conn ->
    forever $ do
      -- Sleep 1 hour between cleanup runs.
      threadDelay (60 * 60 * 1000000)
      catch
        ( do
            n <- cleanupExhausted conn
            when (n > 0) $
              putStrLn $
                "Cleanup: removed " ++ show n ++ " exhausted entries (tries >= " ++ show maxRetries ++ ")"
        )
        $ \err ->
          putStrLn $ "Cleanup error: " ++ displayException (err :: SomeException)

workerLoop :: Chan () -> ConnectInfo -> String -> Int -> IO ()
workerLoop wakeChan connectInfo cache batch =
  withConnect connectInfo $ \conn ->
    forever $ do
      -- Block until there is work to process.
      readChan wakeChan
      catch (processDrvPath conn cache batch) $ \err ->
        putStrLn $ "Worker encountered an error: " ++ displayException (err :: SomeException)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  host <- maybe mempty id <$> lookupEnv "HYDRA_HOST"
  user <- maybe mempty id <$> lookupEnv "HYDRA_USER"
  pass <- maybe mempty id <$> lookupEnv "HYDRA_PASS"
  attic <- maybe "localhost" id <$> lookupEnv "ATTIC_HOST"
  cache <- maybe mempty id <$> lookupEnv "ATTIC_CACHE"
  token <- maybe mempty id <$> lookupEnv "ATTIC_TOKEN"
  workersEnv <- lookupEnv "ATTIC_WORKERS"
  let workerCount =
        case workersEnv >>= readMaybe of
          Just n | n > 0 -> n
          -- Default to 4 workers: peer-fetch adds network I/O per item,
          -- so parallelism helps keep throughput up while waiting on
          -- nix-store --realise and attic push.
          _ -> 4
  batchEnv <- lookupEnv "ATTIC_BATCH"
  let batch =
        case batchEnv >>= readMaybe of
          Just n | n > 0 -> n
          -- Paths per `attic push`.  A build's outputs share most of
          -- their closure, so the per-push closure check is paid once.
          _ -> 50

  (exitCode, _, errOutput) <-
    readCreateProcessWithExitCode
      (shell $ "attic login local " ++ attic ++ " " ++ token)
      ""

  case exitCode of
    ExitFailure code -> do
      putStrLn $ "Ran: attic login local " ++ attic ++ " <token>"
      putStrLn $ "Login failed with exit code " ++ show code
      unless (null errOutput) $ putStrLn $ "Error output:\n" ++ errOutput
    _ -> do
      let connectInfo = ConnectInfo host 5432 user pass "hydra"
      wakeChan <- newChan
      putStrLn $ "Starting " ++ show workerCount ++ " worker thread(s)"
      replicateM_ workerCount $
        void $
          forkIO $
            workerLoop wakeChan connectInfo cache batch
      -- Background thread: clean up exhausted entries every hour.
      void $ forkIO $ cleanupLoop connectInfo
      -- Run initial cleanup at startup to clear any accumulated dead entries.
      withConnect connectInfo $ \startupConn -> do
        n <- cleanupExhausted startupConn
        when (n > 0) $
          putStrLn $
            "Startup cleanup: removed " ++ show n ++ " exhausted entries"
      -- Periodic wake-up timer: re-check deferred items whose backoff
      -- has expired.  Without this, workers sleep forever on readChan
      -- after a transient network outage defers all in-flight items.
      void $ forkIO $ forever $ do
        threadDelay (5 * 60 * 1000000) -- every 5 minutes
        replicateM_ workerCount $ writeChan wakeChan ()
      withConnect connectInfo $ \listenConn -> do
        _ <- execute_ listenConn "LISTEN step_finished" -- (build id, step id, logpath)
        -- Kick workers once to process any backlog that exists at startup.
        replicateM_ workerCount $ writeChan wakeChan ()
        -- Trigger workers whenever Hydra notifies us of new entries.
        forever $ do
          _ <- getNotification listenConn
          replicateM_ workerCount $ writeChan wakeChan ()
