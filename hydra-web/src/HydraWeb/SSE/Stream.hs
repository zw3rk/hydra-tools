-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Unified SSE stream endpoints. Each endpoint creates a WAI Application
-- that subscribes to the Hub with appropriate topic filtering.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.SSE.Stream
  ( streamApp
  ) where

import Control.Concurrent.STM (atomically, readTBQueue)
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200, status503)
import Network.Wai (Application, Request (..), responseLBS, responseStream)

import HydraWeb.Types (App (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.SSE.Hub (Topic (..), subscribeTopics, unsubscribe)

-- | Create a WAI Application for a given stream topic string.
-- Parses the topic string to determine which Hub topics to subscribe to.
-- Unauthenticated users are excluded from TopicBridges to prevent
-- leaking private repo notification data via SSE.
streamApp :: App -> Text -> Application
streamApp app topicStr req respond = do
  case appSSEHub app of
    Nothing ->
      respond $ responseLBS status503
        [("Content-Type", "text/plain")]
        "SSE not available"
    Just hub -> do
      -- Parse session cookie to determine auth status. Unauthenticated
      -- users are stripped of TopicBridges so private repo bridge data
      -- is never sent to them.
      let cookieStr = extractCookieHeader req
      mUser <- getOptionalUser (appPool app) cookieStr
      let rawTopics = parseTopic topicStr
          topics = case mUser of
            Just _  -> rawTopics
            Nothing -> Set.delete TopicBridges rawTopics
      (sid, q) <- subscribeTopics hub topics
      respond $ responseStream status200
        [ ("Content-Type", "text/event-stream")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        , ("X-Accel-Buffering", "no")
        ]
        (\write flush -> finally
          (forever $ do
            msg <- atomically $ readTBQueue q
            write (Builder.byteString msg)
            flush
          )
          (unsubscribe hub sid)
        )

-- | Parse a topic string into a set of Hub topics.
-- TopicGlobal is always included (added by subscribeTopics).
parseTopic :: Text -> Set.Set Topic
parseTopic "global"        = Set.singleton TopicGlobal
parseTopic "bridges"       = Set.singleton TopicBridges
parseTopic "queue"         = Set.singleton TopicQueue
parseTopic "machines"      = Set.singleton TopicMachines
parseTopic "running-evals" = Set.singleton TopicRunningEvals
parseTopic t
  | "project:" `Text.isPrefixOf` t =
      Set.singleton $ TopicProject (Text.drop 8 t)
  | "jobset:" `Text.isPrefixOf` t =
      case Text.splitOn ":" (Text.drop 7 t) of
        [p, j] -> Set.singleton $ TopicJobset p j
        _      -> Set.singleton TopicGlobal
  | otherwise = Set.singleton TopicGlobal

-- | Extract the raw Cookie header value from a WAI request as Maybe Text.
-- This is what getOptionalUser expects (Servant passes the Cookie header
-- the same way).
extractCookieHeader :: Request -> Maybe Text
extractCookieHeader req =
  TE.decodeUtf8 <$> lookup "Cookie" (requestHeaders req)
