{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Test.DsQueue
  ( tests
  , manualTest
  ) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Exception.Lifted (bracket)
import           Control.Monad            (void)
import           Control.Monad.Catch      (MonadCatch, catch)

import           DiskStore                (DiskStoreConfig (..))
import qualified DsQueue

import           Hedgehog                 (Gen, Property, PropertyT, discover,
                                           (===))
import qualified Hedgehog
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range

import           System.Directory         (removeDirectoryRecursive)
import           System.IO.Temp           (createTempDirectory)

prop_length_correct :: Property
prop_length_correct =
  Hedgehog.withTests 200 . Hedgehog.property $ do
    xs <- Hedgehog.forAll genOrderedIntList
    dsq <- Hedgehog.evalIO $ DsQueue.new Nothing
    lenZero <- Hedgehog.evalIO $ DsQueue.length dsq
    lenZero === 0
    Hedgehog.evalIO $ DsQueue.writeMany dsq xs
    count <- Hedgehog.evalIO $ DsQueue.length dsq
    count === length xs

prop_maintains_order :: Property
prop_maintains_order =
  Hedgehog.withTests 200 . Hedgehog.property $ do
    dsq <- Hedgehog.evalIO $ DsQueue.new Nothing
    original <- Hedgehog.forAll genOrderedIntList
    output <- Hedgehog.evalIO $ do
                DsQueue.writeMany dsq original
                DsQueue.readCount dsq $ length original
    original === output

-- This test does real disk IO so only run 10 tests.
-- Also a larger numbers of tests (ie 20) can fail because the temp directory is not created.
prop_stored_state_ok :: Property
prop_stored_state_ok =
  Hedgehog.withTests 10 . Hedgehog.property $
    propertyTWithTempDirectory "/tmp" "dsq-test-1" $ \ dir -> do
      let storeConfig = DiskStoreConfig { scDirectory = dir, scName = "blah", scKeepCount = 5  }
      dsq <- Hedgehog.evalIO $ DsQueue.new (Just storeConfig)
      len0 <- Hedgehog.evalIO $ DsQueue.length dsq
      len0 === 0

      xs1 <- Hedgehog.forAll $ Gen.list (Range.linear 0 10) (Gen.int $ Range.linear 0 100)
      Hedgehog.evalIO $ DsQueue.writeMany dsq xs1
      ys1 <- Hedgehog.evalIO $ DsQueue.currentState dsq
      len1 <- Hedgehog.evalIO $ DsQueue.length dsq
      len1 === length ys1

      xs2 <- Hedgehog.forAll $ Gen.list (Range.linear 5 20) (Gen.int $ Range.linear 100 200)
      Hedgehog.evalIO $ DsQueue.writeMany dsq xs2
      ys2 <- Hedgehog.evalIO $ DsQueue.currentState dsq
      xs1 ++ xs2 === ys2

      len2 <- Hedgehog.forAll $ Gen.int (Range.linear 0 (length xs1))
      void $ Hedgehog.evalIO (DsQueue.readCount dsq len2)
      drop len1 xs1 ++ xs2 === ys2

      len3 <- Hedgehog.forAll $ Gen.int (Range.linear 0 (length xs2))
      void $ Hedgehog.evalIO (DsQueue.readCount dsq len3)
      ys3 <- Hedgehog.evalIO $ DsQueue.currentState dsq
      drop (len2 + len3) (xs1 ++ xs2) === ys3

-- Make sure `DsQueue.read` blocks if the queue is empty.
prop_read_blocks_ok :: Property
prop_read_blocks_ok =
  Hedgehog.withTests 200 . Hedgehog.property $ do
    dsq <- Hedgehog.evalIO $ DsQueue.new Nothing
    val <- Hedgehog.forAll $ Gen.int (Range.linear 0 10000000)
    -- `race` will return the first result which should be a `Right` containing the value
    -- returned by `DsQueue.read`. If the read returns before the write has been done, the value
    -- is very unlikely to match. If result is `Left ()` then something is very wrong.
    result <- Hedgehog.evalIO $ race
                (threadDelay 1000 >> DsQueue.write dsq val >> threadDelay 1000)
                (DsQueue.read dsq)
    result === Right val

-- This test does real disk IO so only run 10 tests.
-- Also a larger numbers of tests (ie 20) can fail because the temp directory is not created.
prop_state_reload_ok :: Property
prop_state_reload_ok =
  Hedgehog.withShrinks 0 . Hedgehog.withTests 10 . Hedgehog.property $
    propertyTWithTempDirectory "/tmp" "dsq-test-2" $ \ dir -> do
      let storeConfig = DiskStoreConfig { scDirectory = dir, scName = "blah", scKeepCount = 5 }
      dsq1 <- Hedgehog.evalIO $ DsQueue.new (Just storeConfig)
      xs1 <- Hedgehog.forAll $ Gen.list (Range.linear 5 20) (Gen.int $ Range.linear 0 100)
      Hedgehog.evalIO $ DsQueue.writeMany dsq1 xs1

      -- If we create a new DsQueue with the same storeConfig as the previous one on intialisation,
      -- it should have the same contents as the previous one.
      dsq2 <- Hedgehog.evalIO $ DsQueue.new (Just storeConfig)
      ys2 <- Hedgehog.evalIO $ DsQueue.currentState dsq2

      xs1 === ys2

-- -----------------------------------------------------------------------------

genOrderedIntList :: Gen [Int]
genOrderedIntList = do
  start <- Gen.int (Range.linear 0 1000)
  count <- Gen.int (Range.linear 2 100)
  pure [ start .. start + count ]

-- Slightly modified version of `withTempDirectory` from the `temporary` package.
propertyTWithTempDirectory
    :: FilePath -> String -> (FilePath -> PropertyT IO a) -> PropertyT IO a
propertyTWithTempDirectory targetDir template =
    bracket
      (Hedgehog.evalIO slowCreateTempDirectory)
      (Hedgehog.evalIO . ignoringIOErrors . removeDirectoryRecursive)
  where
    ignoringIOErrors :: MonadCatch m => m () -> m ()
    ignoringIOErrors ioe = ioe `catch` (\(_ :: IOError) -> pure ())

    -- If this is inlined and/or the threadDelay is removed, it can cause a test using
    -- `propertyTWithTempDirectory` to fail because it tries to create a file in a
    -- directory that does not yet exist (due to laziness).
    slowCreateTempDirectory :: IO FilePath
    slowCreateTempDirectory = do
      dir <- createTempDirectory targetDir template
      threadDelay 10000
      pure dir

-- -----------------------------------------------------------------------------

-- A manual test.
manualTest :: IO ()
manualTest = do
  let storeConfig = DiskStoreConfig { scDirectory = "/tmp/xxxx", scName = "blah", scKeepCount = 5  }
      xs = [ 1, 35, 23, 12 :: Int ]
  dsq1 <- DsQueue.new (Just storeConfig)
  DsQueue.writeMany dsq1 xs

tests :: IO Bool
tests = Hedgehog.checkParallel $$discover
