module Common.Utils (contains, remove, safeName) where

import Prelude (class Eq, (>), (==), (/=), ($))
import Data.Array (length, filter)
import Data.String (null)

-- | return true if the element is found in the array
contains :: ∀ a. Eq a => Array a -> a -> Boolean
contains xs x =
  (length $ filter (\y -> y == x) xs) > 0

-- | remove the element from the array if it is present
remove :: ∀ a. Eq a => Array a -> a -> Array a
remove xs x =
  filter (\y -> y /= x) xs

-- | build a safe name for a file name in cases where it would otherwise be empty
safeName :: String -> String
safeName s =
  if (null s) then
    "unnamed"
  else
    s
