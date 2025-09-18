{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.DiskStore
  ( tests,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List qualified as List
import DiskStore (DiskStoreConfig (..))
import DiskStore qualified
import Hedgehog (Gen, Property, discover, (===))
import Hedgehog qualified
import Hedgehog.Corpus qualified as Corpus
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (listDirectory)
import System.IO.Temp (withTempDirectory)

prop_roundtrip :: Property
prop_roundtrip =
  Hedgehog.withTests 200 . Hedgehog.property $ do
    oldTd <- Hedgehog.forAll genTestData
    newTd <- liftIO . withTempDirectory "/tmp" "disk-store-test" $ \dir -> do
      let storeConfig = DiskStoreConfig {scDirectory = dir, scName = "blah", scKeepCount = 5}
      DiskStore.save storeConfig oldTd
      DiskStore.retrieveLatest storeConfig
    Right oldTd === newTd

-- This test does real disk IO so only run 10 tests.
-- It generates a list of `TestData`, orders it (ascending) by the `tdInt` element and then writes
-- them to disk one-by-one. It then retrieves the last one and compares it to the last in the
-- initial list.
prop_retriveLatest :: Property
prop_retriveLatest =
  Hedgehog.withTests 10 . Hedgehog.property $ do
    tds <- List.sortOn tdInt <$> Hedgehog.forAll (Gen.list (Range.linear 2 10) genTestData)
    newTd <- liftIO . withTempDirectory "/tmp" "disk-store-test" $ \dir -> do
      let storeConfig = DiskStoreConfig {scDirectory = dir, scName = "blah", scKeepCount = 5}
      mapM_ (DiskStore.save storeConfig) tds
      DiskStore.retrieveLatest storeConfig
    Right (List.last tds) === newTd

-- This test does real disk IO so only run 10 tests.
prop_keep_count :: Property
prop_keep_count =
  Hedgehog.withTests 10 . Hedgehog.property $ do
    keepCount <- Hedgehog.forAll $ Gen.int (Range.linear 2 5)
    xs <- Hedgehog.forAll $ Gen.list (Range.singleton (keepCount + 4)) (Gen.int $ Range.linear 0 100)
    count <- liftIO . withTempDirectory "/tmp" "disk-store-test" $ \dir -> do
      let storeConfig = DiskStoreConfig {scDirectory = dir, scName = "blah", scKeepCount = keepCount}
      mapM_ (DiskStore.save storeConfig) xs
      length <$> listDirectory (dir)
    count === keepCount

-- -----------------------------------------------------------------------------

data TestData = TestData
  { tdInt :: Int,
    tdBool :: Bool,
    tdString :: String
  }
  deriving (Eq, Read, Show)

genTestData :: Gen TestData
genTestData =
  TestData
    <$> Gen.int (Range.linear (-10) 1000)
    <*> Gen.bool
    <*> Gen.element Corpus.simpsons

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = Hedgehog.checkParallel $$discover
