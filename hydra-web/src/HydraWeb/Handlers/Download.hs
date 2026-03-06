-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Download handler for build products.
-- Serves files directly from the Nix store using WAI's zero-copy sendfile.
-- Enforces visibility (hidden projects, private repos) before serving.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.Download
  ( downloadApp
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200, status404)
import Network.Mime (defaultMimeLookup)
import qualified Network.Wai as Wai
import System.Directory (doesFileExist, doesDirectoryExist)

import HydraWeb.Types (App (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Builds (getBuildProduct)
import HydraWeb.DB.Projects (getProjectNameByBuild)
import HydraWeb.Models.Build (BuildProduct (..))
import HydraWeb.Visibility (isProjectAccessible)

-- | WAI application that serves a build product file.
--
-- Route: @/build/{id}/download/{productnr}/...@
--
-- The trailing path segments (captured by Servant 'Raw') are used as a
-- sub-path when the product path points to a directory. Security: the
-- stored path must start with @/nix/store/@ and neither the stored path
-- nor sub-path segments may contain @..@.
downloadApp :: App -> Int -> Int -> Wai.Application
downloadApp app bid productNr req respond = do
  -- Extract cookie the same way SSE/Stream.hs does.
  let cookieStr = extractCookieHeader req
  mUser <- getOptionalUser (appPool app) cookieStr

  result <- withConn (appPool app) $ \conn -> do
    -- Visibility check: resolve project for this build, then check access.
    mProject <- getProjectNameByBuild conn bid
    case mProject of
      Nothing -> pure Nothing
      Just projName -> do
        accessible <- isProjectAccessible conn projName mUser
        if not accessible
          then pure Nothing
          else do
            mProduct <- getBuildProduct conn bid productNr
            pure mProduct

  case result of
    Nothing -> respond notFound
    Just prod -> serveProduct prod req respond

-- | Attempt to serve a build product file from the Nix store.
serveProduct :: BuildProduct -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
serveProduct prod req respond = do
  case bpPath prod of
    Nothing -> respond notFound
    Just storePath
      -- Path must be in /nix/store/ and contain no traversals.
      | not (isValidStorePath storePath) -> respond notFound
      | otherwise -> do
          let pathText = storePath
          -- Extra path segments from the URL after /download/{nr}/...
          -- pathInfo for a Raw handler contains the segments AFTER the
          -- matched prefix, so for /build/123/download/1/foo/bar.txt
          -- servant passes ["foo","bar.txt"] to the Raw app.
          let subSegments = filter (not . Text.null) (Wai.pathInfo req)

          -- Validate no traversals in sub-path segments.
          if any (".." `Text.isInfixOf`) subSegments
            then respond notFound
            else do
              isFile <- doesFileExist (Text.unpack pathText)
              isDir  <- doesDirectoryExist (Text.unpack pathText)
              case (isFile, isDir, subSegments) of
                -- Product path is a regular file — serve it directly.
                (True, _, _) -> serveFile pathText (bpName prod) respond
                -- Directory + sub-path from URL — serve path/sub/path.
                (_, True, (_:_)) -> do
                  let subPath = Text.intercalate "/" subSegments
                      fullPath = pathText <> "/" <> subPath
                  exists <- doesFileExist (Text.unpack fullPath)
                  if exists
                    then serveFile fullPath (lastSegment subPath) respond
                    else respond notFound
                -- Directory + no sub-path + defaultpath set — serve default.
                (_, True, []) | Just dp <- bpDefaultPath prod, not (Text.null dp) -> do
                  let fullPath = pathText <> "/" <> dp
                  exists <- doesFileExist (Text.unpack fullPath)
                  if exists
                    then serveFile fullPath (lastSegment dp) respond
                    else respond notFound
                -- Everything else — not found.
                _ -> respond notFound

-- | Serve a file with detected MIME type and Content-Disposition header.
serveFile :: Text -> Text -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
serveFile filePath fileName respond' = do
  let mime = defaultMimeLookup fileName
      headers = [ ("Content-Type", mime)
                , ("Content-Disposition", "attachment; filename=\""
                    <> TE.encodeUtf8 (sanitizeFilename fileName) <> "\"")
                ]
  respond' $ Wai.responseFile status200 headers (Text.unpack filePath) Nothing

-- | Validate that a store path starts with /nix/store/ and has no traversals.
isValidStorePath :: Text -> Bool
isValidStorePath p =
  "/nix/store/" `Text.isPrefixOf` p && not (".." `Text.isInfixOf` p)

-- | Extract the Cookie header from a WAI request (same pattern as SSE/Stream.hs).
extractCookieHeader :: Wai.Request -> Maybe Text
extractCookieHeader req =
  TE.decodeUtf8 <$> lookup "Cookie" (Wai.requestHeaders req)

-- | Get the last path segment for use as a filename.
lastSegment :: Text -> Text
lastSegment t = fromMaybe t $ safeLast (Text.splitOn "/" t)
  where
    safeLast [] = Nothing
    safeLast xs = Just (last xs)

-- | Remove characters unsafe in Content-Disposition filenames.
sanitizeFilename :: Text -> Text
sanitizeFilename = Text.filter (`notElem` ("\"\\/\r\n" :: [Char]))

-- | 404 response body.
notFound :: Wai.Response
notFound = Wai.responseLBS status404
  [("Content-Type", "text/plain")] "Not Found"
