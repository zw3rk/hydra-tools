{-# LANGUAGE ImportQualifiedPost #-}

import Hedgehog.Main (defaultMain)
import Test.DiskStore qualified

main :: IO ()
main =
  defaultMain
    [ Test.DiskStore.tests
    ]
