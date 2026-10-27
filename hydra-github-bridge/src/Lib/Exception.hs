{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Exception
  ( tryNonAsync,
  )
where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)

-- | Like 'try' at 'SomeException', but rethrows asynchronous exceptions.
--
-- The workers in @Main@ run under 'Control.Concurrent.Async.mapConcurrently_',
-- which cancels the rest when one throws.  A worker that catches the resulting
-- 'Control.Concurrent.Async.AsyncCancelled' as an ordinary failure keeps
-- running, so the process never exits (and systemd never restarts it) while
-- the webhook server and the other workers are already gone.
tryNonAsync :: IO a -> IO (Either SomeException a)
tryNonAsync action = do
  result <- try action
  case result of
    Left e | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
    _ -> pure result
