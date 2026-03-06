-- Copyright 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group.
-- SPDX-License-Identifier: Apache-2.0
--
-- | Handler for build log viewing (GET /build/:id/nixlog/:stepnr).
-- Reads bzip2-compressed (or plain-text) build logs from the Hydra
-- data directory and renders them in a <pre> block.
{-# LANGUAGE OverloadedStrings #-}

module HydraWeb.Handlers.BuildLog
  ( buildLogHandler
  ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Error.Class (throwError)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Lucid
import Servant (err404)

import qualified Codec.Compression.BZip as BZip

import HydraWeb.Types (AppM, App (..))
import HydraWeb.Config (Config (..))
import HydraWeb.Auth.Middleware (getOptionalUser)
import HydraWeb.DB.Pool (withConn)
import HydraWeb.DB.Builds (getBuildStep)
import HydraWeb.DB.Projects (getProjectNameByBuild)
import HydraWeb.Models.Build (BuildStep (stepDrvPath))
import HydraWeb.Visibility (isProjectAccessible)
import HydraWeb.View.Layout (PageData (..), pageLayout)
import HydraWeb.View.Pages.Build (buildLogPage)
import HydraWeb.View.Components (showT)

-- | Serve the build log for a given build step.
-- Locates the log file under <hydraDataDir>/build-logs/XX/YY.bz2
-- where XX is the first 2 chars of the nix store hash and YY is the rest.
-- Returns 404 if the build's parent project is hidden and user is not super-admin.
buildLogHandler :: Maybe Text -> Int -> Int -> AppM (Html ())
buildLogHandler mCookie bid stepNr = do
  pool    <- asks appPool
  bp      <- asks (cfgBasePath . appConfig)
  dataDir <- asks (cfgHydraDataDir . appConfig)
  mUser <- liftIO $ getOptionalUser pool mCookie
  counts <- liftIO . readTVarIO =<< asks appNavCounts
  result <- liftIO $ withConn pool $ \conn -> do
    -- Check project visibility before fetching the step.
    mProjName <- getProjectNameByBuild conn bid
    accessible <- case mProjName of
      Nothing -> pure False
      Just pn -> isProjectAccessible conn pn mUser
    if not accessible
      then pure Nothing
      else do
        mStep <- getBuildStep conn bid stepNr
        pure mStep
  case result of
    Nothing -> throwError err404
    Just step -> do
      case stepDrvPath step of
        Nothing -> throwError err404
        Just drv -> do
          logText <- liftIO $ readBuildLog dataDir drv
          let pd = PageData
                { pdTitle    = "Build #" <> showT bid <> " step " <> showT stepNr <> " log"
                , pdBasePath = bp
                , pdCounts   = counts
                , pdUser     = mUser
                }
          pure $ pageLayout pd $
            buildLogPage bp bid stepNr drv logText

-- | Read a build log from the Hydra data directory.
-- Tries bzip2-compressed first, falls back to uncompressed plain text.
-- Validates the derivation path to prevent directory traversal attacks.
readBuildLog :: FilePath -> Text -> IO Text
readBuildLog dataDir drvPath
  -- Reject paths that don't start with /nix/store/ or contain traversal sequences.
  | not (Text.isPrefixOf nixStorePrefix drvPath) = pure "(invalid derivation path)"
  | Text.isInfixOf ".." storeName               = pure "(invalid derivation path)"
  | otherwise = do
      let -- First 2 chars of hash → subdirectory
          subDir    = Text.take 2 storeName
          -- Remaining chars → filename
          fileName  = Text.drop 2 storeName
          basePath  = dataDir <> "/build-logs/" <> Text.unpack subDir
                      <> "/" <> Text.unpack fileName
          bzPath    = basePath <> ".bz2"
      -- Try bzip2-compressed log first.
      result <- try (LBS.readFile bzPath) :: IO (Either SomeException LBS.ByteString)
      case result of
        Right compressed -> do
          let decompressed = BZip.decompress compressed
          pure $ decodeLog decompressed
        Left _ -> do
          -- Fall back to uncompressed log (Rust queue runner may write plain text).
          plain <- try (LBS.readFile basePath) :: IO (Either SomeException LBS.ByteString)
          case plain of
            Right content -> pure $ decodeLog content
            Left _        -> pure "(no build log available)"
  where
    nixStorePrefix = "/nix/store/"
    -- Strip /nix/store/ prefix to get "HASH-name.drv"
    storeName = Text.drop (Text.length nixStorePrefix) drvPath

-- | Decode a lazy ByteString as UTF-8 text, replacing invalid sequences.
decodeLog :: LBS.ByteString -> Text
decodeLog = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') . LBS.toStrict
