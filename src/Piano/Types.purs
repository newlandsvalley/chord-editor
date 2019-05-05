module Piano.Types where

import Prelude (negate)

-- | a finger position on a string
-- | n < 0  : String is silent
-- | n == 0 : Open string is sounded
-- | N > 0  : string is fretted at this position
type FingerPosition = Int

-- | fingering is represented as an array of finger positions
type Fingering = Array FingerPosition


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

-- | parameters other than fingering that show up on the chord diagram
-- | and which are governed by HTML input boxes of some kind
type DiagramParameters =
  { name :: String             -- the chord name
  }

-- | an open string
open :: FingerPosition
open = 0

-- | a silent (unplayed) string
silent :: FingerPosition
silent = -1

dChord :: Fingering
dChord =
  [9,14,18]

unfingered :: Fingering
unfingered =
  []
