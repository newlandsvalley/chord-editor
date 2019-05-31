module Bass.Types where


import Prelude (class Eq, (==))
import Bass.FingerStatus (FingerStatus(..))

{-}
-- | a finger on a bass guitar pattern can be primary (black circle) or
-- | secondary (gray square) to indicate its importance
data FingerStatus =
    Primary
  | Secondary

derive instance eqFingerStatus :: Eq FingerStatus
-}

-- | a fingered fret position on a string
-- | n == 0 : Open string is sounded
-- | N > 0  : string is fretted at this position
type FretNumber = Int

-- | a finger position combines the fret number with an indication
-- | of the importance of the fingering
-- | when the fret is 0 (open) then status is not applicable
type FingerPosition = { fret :: FretNumber, status :: FingerStatus }

-- | in a bass pattern, multiple fingers are allowed on a string
-- | an empty array represents an unplayed string
type StringPositions = Array FingerPosition

-- | overall fingering is represented as an array of string positions
type Fingering = Array StringPositions

-- | a fingered string
type FingeredString =
  { stringNumber :: Int
  , fretNumber  :: FretNumber
  }

-- | The editable chord shape of the bass
type ChordShape =
  { name :: String             -- the chord name
  , firstFretOffset :: Int     -- which fret on the guitar does fret 1 represent
  , fingering :: Fingering     -- the fretboard fingering
  }

-- | the number of frets we display
displayedFretCount :: Int
displayedFretCount = 5

-- | an open string
open :: FingerPosition
open =
  { fret : 0, status : Primary }

isOpenFret :: FingerPosition -> Boolean
isOpenFret fp =
  fp.fret == 0

-- | all the open strings
openStrings :: Fingering
openStrings =
  [[open],[open],[open],[open]]

-- | all the closed strings
closedStrings :: Fingering
closedStrings =
  [[],[],[],[]]

-- | test sample
samplePattern :: Fingering
samplePattern =
  [[open],[{ fret : 2, status: Primary}]
         ,[open, {fret: 4, status: Secondary}]
         ,[ { fret: 1, status: Primary},{fret : 3, status : Secondary}]]

-- | silent strings chord name
closedStringsChordName :: String
closedStringsChordName =
  "silent"
