module Piano.Types where

-- | a finger position on a keyboard
-- | this is just an offset into the array of keys representing 2 octaves
-- | 0 <= position <= 23
type FingerPosition = Int

-- | fingering is represented as an array of finger positions
-- | this is a null array when no fingers are on the keyboard
-- | otherwise its length is the number of fingers in use.
type Fingering = Array FingerPosition

-- | parameters other than fingering that show up on the chord diagram
-- | and which are governed by HTML input boxes of some kind
type DiagramParameters =
  { name :: String             -- the chord name
  }

unfingered :: Fingering
unfingered =
  []
