module Common.Utils (contains) where

import Prelude (class Eq, (>), (==), ($))
import Data.Array (length, filter)

contains :: ∀ a. Eq a => Array a -> a -> Boolean
contains xs x =
  (length $ filter (\y -> y == x) xs) > 0
