module Types where

-- | a finger position on a string
-- | n < 0  : String is silent
-- | n == 0 : Open string is sounded
-- | N > 0  : string is fretted at this position
type FingerPosition = Int

-- | a fretboard is represented as an array of finger positions
type Fretboard = Array FingerPosition

-- | the number of frets to display
fretCount :: Int
fretCount = 7

-- | the number of strings to display
-- | (for a guitar, this is obviously 6
-- | but we may want, for example, to represent ukuleles)
stringCount :: Int
stringCount = 6
