module Common.Export (exportAs, scaleCanvas, toMimeType) where

import Prelude (Unit)
import Graphics.Canvas (CanvasElement)
import Effect (Effect)
import Common.Types (ExportFormat(..))

-- | the MIME type for each export format
toMimeType :: ExportFormat -> String
toMimeType PNG = "image/png;base64"
toMimeType JPG = "image/jpeg"

-- | export the canvas to the specified file as governed by the image MIME type
foreign import exportAs :: CanvasElement -> String -> String -> Effect Unit

-- | scale the canvas by the specified amount to produce a new (hidden) canvas
-- | as a prelude to downloading it
foreign import scaleCanvas :: CanvasElement -> Number -> Effect CanvasElement
