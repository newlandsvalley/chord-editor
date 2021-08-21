module Piano.Types where

-- | a finger position on a keyboard
-- | this is just an offset into the array of keys representing 2 octaves
-- | 0 <= position <= 23
type FingerPosition = Int

-- | fingering is represented as an array of finger positions
-- | this is a null array when no fingers are on the keyboard
-- | otherwise its length is the number of fingers in use.
type Fingering = Array FingerPosition

-- | The editable chord shape of the piano
type ChordShape =
  { name :: String -- the chord name
  , fingering :: Fingering
  }

unfingered :: Fingering
unfingered =
  []
