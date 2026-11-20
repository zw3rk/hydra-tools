{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Exception
  ( tryNonAsync,
    httpStatusOf,
    briefHttpError,
  )
where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Data.ByteString.Char8 qualified as BS8
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)

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

-- | The HTTP status of a failed GitHub request, if that is what failed.
httpStatusOf :: SomeException -> Maybe Int
httpStatusOf e = case fromException e of
  Just (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) -> Just (statusCode (HTTP.responseStatus resp))
  _ -> Nothing

-- | A one-line description of a failed GitHub request.  'show' of an
-- 'HTTP.HttpException' spells out the whole request and response, a dozen
-- log lines per failure.
briefHttpError :: SomeException -> String
briefHttpError e = case fromException e of
  Just (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp body)) ->
    "HTTP " <> show (statusCode (HTTP.responseStatus resp)) <> ": " <> take 200 (BS8.unpack body)
  Just (HTTP.HttpExceptionRequest _ content) -> takeWhile (/= '\n') (show content)
  _ -> takeWhile (/= '\n') (displayException e)
