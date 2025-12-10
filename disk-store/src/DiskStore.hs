{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}

module DiskStore
  ( DiskStoreConfig (..),
    retrieveLatest,
    save,
  )
where

import Control.Exception (catch, throwIO)
import Data.List qualified as List
import Data.Ord (Down (..))
import Data.Time (UTCTime)
import Data.Time qualified as Time
import System.Directory (listDirectory, removeFile)
import System.FilePath (isExtensionOf, takeFileName, (</>))
import System.IO
  ( IOMode (WriteMode),
    hFlush,
    hPutStrLn,
    withFile,
  )
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)
import Text.Show.Pretty (ppShow)

data DiskStoreConfig = DiskStoreConfig
  { scDirectory :: FilePath, -- The directory to store the state in.
    scName :: String, -- The file name prefix.
    scKeepCount :: Int -- How many files to keep (the most recent are kept.
  }

retrieveLatest :: Read a => DiskStoreConfig -> IO (Either String a)
retrieveLatest scfg = do
  xs <- getStoreFiles scfg
  case xs of
    [] -> pure . Left $ "No matching files in " ++ show (scDirectory scfg)
    (fpath : _) -> do
      str <- readFile fpath
      pure $ maybe (Left $ "Failed to read " ++ show fpath) Right $ readMaybe str

save :: Show a => DiskStoreConfig -> a -> IO ()
save scfg a = do
  fpath <- genFilePath scfg
  -- putStrLn $ "DiskStore.save: " ++ fpath
  withFile fpath WriteMode $ \h -> do
    hPutStrLn h $ ppShow a
    hFlush h
  deleteOldStates scfg

-- -------------------------------------------------------------------------------------------------

deleteOldStates :: DiskStoreConfig -> IO ()
deleteOldStates scfg = do
  xs <- getStoreFiles scfg
  mapM_ removeFileIfExists $ take (length xs - scKeepCount scfg) xs
  where
    removeFileIfExists :: FilePath -> IO ()
    removeFileIfExists fpath =
      removeFile fpath `catch` \e ->
        if isDoesNotExistError e
          then pure ()
          else throwIO e

genFilePath :: DiskStoreConfig -> IO FilePath
genFilePath scfg =
  toFilePath <$> Time.getCurrentTime
  where
    toFilePath :: UTCTime -> FilePath
    toFilePath time =
      scDirectory scfg
        </> scName scfg
        ++ Time.formatTime Time.defaultTimeLocale "-%Y-%m-%d+%H:%M:%S%Q" time
        ++ storeExt

genFilter :: DiskStoreConfig -> (FilePath -> Bool)
genFilter scfg fpath =
  scName scfg `List.isPrefixOf` fname && storeExt `isExtensionOf` fname
  where
    fname :: String
    fname = takeFileName fpath

getStoreFiles :: DiskStoreConfig -> IO [FilePath]
getStoreFiles scfg =
  fmap (scDirectory scfg </>) . List.sortOn Down . List.filter (genFilter scfg)
    <$> listDirectory (scDirectory scfg)

storeExt :: String
storeExt = ".store"
