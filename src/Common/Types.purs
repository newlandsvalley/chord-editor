module Common.Types where

-- | we only export to PNG and JPG at the moment
data ExportFormat =
    PNG
  | JPG

-- | the coordinates of a mouse click relative to the top left of the canvas
type MouseCoordinates =
  { x :: Number
  , y :: Number
  }

-- | the coordinates that define the entire canvas position
type CanvasPosition =
  { left :: Number
  , top  :: Number
  }

-- | a slider position represented as a percentage of the total slide
type Percentage = Int
