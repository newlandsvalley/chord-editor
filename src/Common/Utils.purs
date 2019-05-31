module Common.Utils (contains, remove, safeName, jsonFileInputCtx) where

import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Array (length, filter, null)
import Data.MediaType (MediaType(..))
-- import Data.String (null)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Halogen.FileInputComponent (Context) as FIC
import Prelude (class Eq, (>), (==), (/=), ($), (&&))

-- | return true if the element is found in the array
contains :: ∀ a. Eq a => Array a -> a -> Boolean
contains xs x =
  (length $ filter (\y -> y == x) xs) > 0

-- | remove the element from the array if it is present
remove :: ∀ a. Eq a => Array a -> a -> Array a
remove xs x =
  filter (\y -> y /= x) xs

-- | build a safe name for a file name in cases where it would otherwise be empty
-- | or have invalid characters
safeName :: String -> String
safeName s =
  let
    safeChars = filter
      (\c -> c /= '?' && c /= '*' && c /= '%' && c /= '\\')
      (toCharArray s)
  in
    if (null safeChars) then
      "unnamed"
    else
      fromCharArray safeChars

-- | the context for a JSON file input field
jsonFileInputCtx :: FIC.Context
jsonFileInputCtx =
  { componentId : "jsoninput"
  , isBinary : false
  , prompt : "load"
  , accept :  mediaType (MediaType ".json")
  }
