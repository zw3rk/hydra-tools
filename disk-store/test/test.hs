{-# LANGUAGE ImportQualifiedPost #-}

import           Hedgehog.Main  (defaultMain)

import qualified Test.DiskStore

main :: IO ()
main =
  defaultMain
    [ Test.DiskStore.tests
    ]
