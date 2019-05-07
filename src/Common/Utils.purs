module Common.Utils (contains, remove) where

import Prelude (class Eq, (>), (==), (/=), ($))
import Data.Array (length, filter)

contains :: ∀ a. Eq a => Array a -> a -> Boolean
contains xs x =
  (length $ filter (\y -> y == x) xs) > 0

remove :: ∀ a. Eq a => Array a -> a -> Array a
remove xs x =
  filter (\y -> y /= x) xs
