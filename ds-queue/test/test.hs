{-# LANGUAGE ImportQualifiedPost #-}

import Hedgehog.Main (defaultMain)
import Test.DsQueue qualified

main :: IO ()
main =
  defaultMain
    [ Test.DsQueue.tests
    ]
