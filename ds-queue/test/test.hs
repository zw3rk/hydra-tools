{-# LANGUAGE ImportQualifiedPost #-}

import           Hedgehog.Main (defaultMain)

import qualified Test.DsQueue

main :: IO ()
main =
  defaultMain
    [ Test.DsQueue.tests
    ]
