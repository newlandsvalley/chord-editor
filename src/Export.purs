module Export (exportAs, scaleCanvas) where

import Prelude (Unit)
import Graphics.Canvas (CanvasElement)
import Effect (Effect)

-- | export the canvas to the specified file as governed by the image MIME type
foreign import exportAs :: CanvasElement -> String -> String -> Effect Unit

-- | scale the canvas by the specified amount to produce a new (hidden) canvas
-- | as a prelude to downloading it
foreign import scaleCanvas :: CanvasElement -> Number -> Effect CanvasElement
