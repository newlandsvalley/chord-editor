module Types where

import Prelude (negate)

-- | a finger position on a string
-- | n < 0  : String is silent
-- | n == 0 : Open string is sounded
-- | N > 0  : string is fretted at this position
type FingerPosition = Int

-- | fingering is represented as an array of finger positions
type Fingering = Array FingerPosition

-- | we only export to PNG and JPG at the moment
data ExportFormat =
    PNG
  | JPG

-- | a fingered string
type FingeredString =
  { stringNumber :: Int
  , fretNumber  :: FingerPosition
  }

-- | the coordinates of a mouse click relative to the top left of the canvas
type MouseCoordinates =
  { x :: Number
  , y :: Number
  }

-- | an open string
open :: FingerPosition
open = 0

-- | a silent (unplayed) string
silent :: FingerPosition
silent = -1

-- | the MIME type for each export format
toMimeType :: ExportFormat -> String
toMimeType PNG = "image/png;base64"
toMimeType JPG = "image/jpeg"

-- | just a sample D chord
dChord :: Fingering
dChord =
  [2,0,0,2,3,2]

-- | all the open strings
openStrings :: Fingering
openStrings =
  [open,open,open,open,open,open]

openStringsChordName :: String
openStringsChordName =
  "Em7+11"
