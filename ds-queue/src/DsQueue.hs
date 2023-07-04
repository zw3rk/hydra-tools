{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- This module is designed to be imported qualified.
module DsQueue
  ( DsQueue -- This is an opaque type.
  , currentState
  , length
  , new
  , read
  , readCount
  , write
  , writeMany
  ) where

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, readTVar, writeTVar)

import DiskStore (DiskStoreConfig (..))
import DiskStore qualified

-- Need the following to be able to provide functions that would clash with Prelude.
-- This module is intended to be imported qualified.
import Prelude hiding (length, read)
import Prelude qualified

data DsQueue a = DsQueue
  { dsqDiskStoreConfig :: Maybe DiskStoreConfig
  , dsqRead :: {-# UNPACK #-} TVar [a]
  , dsqWrite :: {-# UNPACK #-} TVar [a]
  }

currentState :: DsQueue a -> IO [a]
currentState = atomically . readState

length :: DsQueue a -> IO Int
length dsq = do
  atomically $ (+)
    <$> fmap Prelude.length (readTVar $ dsqRead dsq)
    <*> fmap Prelude.length (readTVar $ dsqWrite dsq)

new :: Read a => Maybe DiskStoreConfig -> IO (DsQueue a)
new mdsc =
    case mdsc of
      Nothing -> mkDsQueue []
      Just dsc -> do
        exs <- DiskStore.retrieveLatest dsc
        case exs of
          Left _err -> mkDsQueue []
          Right xs -> mkDsQueue xs
  where
    mkDsQueue :: [a] -> IO (DsQueue a)
    mkDsQueue xs = atomically $ DsQueue mdsc <$> newTVar xs <*> newTVar []

read :: forall a. Show a => DsQueue a -> IO a
read dsq = do
    (x, state) <- atomically $ do
                xs <- readTVar (dsqRead dsq)
                case xs of
                  [] -> continueRead
                  (r:rs) -> do
                    writeTVar (dsqRead dsq) rs
                    (r,) <$> readState dsq
    case dsqDiskStoreConfig dsq of
      Nothing -> pure ()
      Just scfg -> DiskStore.save scfg state
    pure x
  where
    continueRead :: STM (a, [a])
    continueRead = do
      xs <- readTVar (dsqWrite dsq)
      case reverse xs of
        [] -> retry
        (y:ys) -> do
          writeTVar (dsqWrite dsq) []
          writeTVar (dsqRead dsq) ys
          (y,) <$> readState dsq

-- `readCount` will block if there are not already `count` elements in the queue.
readCount :: Show a => DsQueue a -> Int -> IO [a]
readCount dsq count = mapM (const $ DsQueue.read dsq) [ 1 .. count ]


-- Write is trivial, just push the new element to the head of the write list.
write :: Show a => DsQueue a -> a -> IO ()
write dsq x = do
  state <- atomically $ do
            modifyTVar' (dsqWrite dsq) (x :)
            readState dsq
  case dsqDiskStoreConfig dsq of
    Nothing -> pure ()
    Just scfg -> DiskStore.save scfg state

writeMany :: Show a => DsQueue a -> [a] -> IO ()
writeMany dsq xs = do
  state <- atomically $ do
            modifyTVar' (dsqWrite dsq) (reverse xs ++)
            readState dsq
  case dsqDiskStoreConfig dsq of
    Nothing -> pure ()
    Just scfg -> DiskStore.save scfg state

-- -------------------------------------------------------------------------------------------------

readState :: DsQueue a -> STM [a]
readState dsq = (++) <$> readTVar (dsqRead dsq) <*> fmap reverse (readTVar $ dsqWrite dsq)
