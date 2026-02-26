{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | SSE (Server-Sent Events) HTTP server for streaming Hydra build status.
--
-- Exposes two endpoints:
--   GET /status/:owner/:repo/:sha        — one-shot JSON with current statuses
--   GET /status/:owner/:repo/:sha/events — SSE stream of status updates
--
-- The SSE stream immediately replays cached status on connect (preventing races
-- where a build finishes before the client connects), then streams live updates.
--
-- Subscriber channels are tracked by unique integer IDs so that cleanup on
-- client disconnect is exact and doesn't require Eq on TChan.
--
-- Copyright (c) Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: Apache-2.0
module Lib.SSE.Server
  ( sseApp,
    runSSEServer,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Exception (finally)
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Lib.SSE.StatusCache
  ( CommitKey,
    StatusCache (..),
    StatusEntry (..),
    garbageCollect,
    lookupStatuses,
  )
import Network.HTTP.Types
  ( hContentType,
    status200,
    status404,
  )
import Network.Wai (Application, pathInfo, responseLBS, responseStream)
import Network.Wai.Handler.Warp qualified as Warp

-- | Unique subscriber ID for tracking SSE connections.
type SubscriberId = Int

-- | Per-server subscriber registry indexed by ID for clean removal.
type SubscriberMap = TVar (Map.Map CommitKey (Map.Map SubscriberId (TChan StatusEntry)))

newSubscriberMap :: IO SubscriberMap
newSubscriberMap = newTVarIO Map.empty

-- | Register a subscriber: creates a TChan, adds it to both the ID-indexed
-- map and the StatusCache's broadcast list, and returns the current cached
-- statuses for immediate replay.
addSubscriber ::
  SubscriberMap ->
  IORef SubscriberId ->
  StatusCache ->
  CommitKey ->
  IO (SubscriberId, TChan StatusEntry, Map.Map Text StatusEntry)
addSubscriber subMap idRef cache key = do
  subId <- atomicModifyIORef' idRef (\n -> (n + 1, n))
  atomically $ do
    chan <- newTChan
    -- Register in the ID-indexed map (authoritative source).
    modifyTVar' subMap $
      Map.insertWith Map.union key (Map.singleton subId chan)
    -- Also register in the StatusCache so broadcastCheckRuns sees it.
    modifyTVar' (scSubscribers cache) $
      Map.insertWith (++) key [chan]
    current <- Map.findWithDefault Map.empty key <$> readTVar (scStatuses cache)
    return (subId, chan, current)

-- | Remove a subscriber by ID, and rebuild the StatusCache's subscriber list
-- from the remaining entries in our ID-indexed map.
removeSubscriber ::
  SubscriberMap ->
  StatusCache ->
  CommitKey ->
  SubscriberId ->
  IO ()
removeSubscriber subMap cache key subId = atomically $ do
  -- Remove from ID-indexed map.
  modifyTVar' subMap $ Map.adjust (Map.delete subId) key
  -- Rebuild StatusCache subscriber list from remaining channels.
  subs <- readTVar subMap
  let remainingChans = case Map.lookup key subs of
        Just m -> Map.elems m
        Nothing -> []
  modifyTVar' (scSubscribers cache) $
    Map.insert key remainingChans

-- | WAI application implementing the SSE endpoints.
sseApp :: StatusCache -> SubscriberMap -> IORef SubscriberId -> Application
sseApp cache subMap idRef req respond = do
  case pathInfo req of
    -- GET /status/:owner/:repo/:sha/events — SSE stream
    ["status", owner, repo, sha, "events"] -> do
      let key = (owner, repo, sha)
      (subId, chan, current) <- addSubscriber subMap idRef cache key

      respond $
        responseStream status200 sseHeaders $ \write flush -> do
          ( do
              -- Replay cached statuses immediately so the client doesn't miss
              -- events that arrived before it connected.
              mapM_ (writeSSE write) (Map.elems current)
              flush

              -- Stream live updates until the client disconnects.
              let loop = do
                    entry <- atomically $ readTChan chan
                    writeSSE write entry
                    flush
                    loop
              loop
            )
            `finally` removeSubscriber subMap cache key subId

    -- GET /status/:owner/:repo/:sha — one-shot JSON snapshot
    ["status", owner, repo, sha] -> do
      let key = (owner, repo, sha)
      statuses <- lookupStatuses cache key
      if Map.null statuses
        then
          respond $
            responseLBS status404 jsonHeaders $
              Aeson.encode $
                Aeson.object ["error" Aeson..= ("no status found" :: Text)]
        else respond $ responseLBS status200 jsonHeaders $ Aeson.encode statuses

    -- GET /health — liveness probe
    ["health"] ->
      respond $
        responseLBS status200 jsonHeaders $
          Aeson.encode $
            Aeson.object ["status" Aeson..= ("ok" :: Text)]

    -- Everything else
    _ ->
      respond $
        responseLBS status404 jsonHeaders $
          Aeson.encode $
            Aeson.object ["error" Aeson..= ("not found" :: Text)]
  where
    sseHeaders =
      [ (hContentType, "text/event-stream"),
        ("Cache-Control", "no-cache"),
        ("Connection", "keep-alive"),
        ("X-Accel-Buffering", "no") -- disable nginx buffering
      ]

    jsonHeaders = [(hContentType, "application/json")]

    -- Format a StatusEntry as an SSE event.
    writeSSE write entry = do
      let json = LBS.toStrict $ Aeson.encode entry
      write $
        Builder.byteString "event: status\n"
          <> Builder.byteString "data: "
          <> Builder.byteString json
          <> Builder.byteString "\n\n"

-- | Run the SSE server on the given port with periodic garbage collection.
-- The TTL controls how long statuses are cached (default: 24 hours).
runSSEServer :: StatusCache -> Int -> NominalDiffTime -> IO ()
runSSEServer cache port ttl = do
  subMap <- newSubscriberMap
  idRef <- newIORef (0 :: SubscriberId)

  putStrLn $ "SSE server starting on port " ++ show port

  Async.mapConcurrently_
    id
    [ Warp.run port (sseApp cache subMap idRef),
      -- Periodic GC: evict stale entries every hour.
      let gcLoop = do
            threadDelay (3600 * 1000000) -- 1 hour
            n <- garbageCollect cache ttl
            putStrLn $ "SSE cache GC: evicted " ++ show n ++ " stale commits"
            gcLoop
       in gcLoop
    ]
