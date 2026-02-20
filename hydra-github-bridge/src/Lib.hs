module Lib
  ( binarySearch,
    binarySearchM,
  )
where

import Data.Void (absurd)

binarySearch :: Int -> Int -> (Int -> (Bool, a)) -> a
binarySearch low high find =
  either absurd id $ binarySearchM low high (Right . find)

binarySearchM :: (Monad m) => Int -> Int -> (Int -> m (Bool, a)) -> m a
binarySearchM low high find = do
  let mid = (low + high) `div` 2
  (higher, found) <- find mid
  if low == high
    then return found
    else
      if higher
        then binarySearchM (mid + 1) high find
        else binarySearchM low mid find
