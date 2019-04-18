module Export (exportAs) where

import Prelude (Unit)
import Graphics.Canvas (CanvasElement)
import Effect (Effect)

foreign import exportAs :: CanvasElement -> String -> String -> Effect Unit
