module FrettedInstrument.Types where

-- | This module extracts types representing the common behaviour of fretted instruments 
-- | such as guitar, tenor guitar and ukelele where chords are played 'normally'.
-- | It does not include bass guitar where they are played as an arpeggio and where,
-- | therefore the behaviour is different.

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String.Common (toLower)
import Data.Midi.Instrument (InstrumentName)

-- | all this stuff about FrettedInstrumentExample is just to support routing
-- | where we use a URI of `FrettedInstrument` followed by the actual example
data FrettedInstrumentExample =
    Guitar
  | TenorGuitar

instance showFrettedInstrumentExample :: Show FrettedInstrumentExample where
  show Guitar = "Guitar"
  show TenorGuitar  = "TenorGuitar"

derive instance eqFrettedInstrumentExample :: Eq FrettedInstrumentExample
derive instance ordFrettedInstrumentExample :: Ord FrettedInstrumentExample

-- | 
readExample :: String -> Maybe FrettedInstrumentExample
readExample genreStr =
  case genreStr of
    "guitar"    -> Just Guitar
    "tenorguitar"  -> Just TenorGuitar
    _ -> Nothing

exampleToString :: FrettedInstrumentExample -> String
exampleToString =
  toLower <<< show

exampleFromString :: String -> Either String FrettedInstrumentExample
exampleFromString s =
  case (readExample s) of
    Just example ->
      Right example
    Nothing ->
      Left $ "Not a known fretted instrument: " <> s

-- | a finger position on a string
-- | n < 0  : String is silent
-- | n == 0 : Open string is sounded
-- | N > 0  : string is fretted at this position
type FingerPosition = Int

-- | fingering is represented as an array of finger positions,
-- | one for each of the strings up to FrettedInstrumentConfig.stringCount.
type Fingering = Array FingerPosition

-- | an open string
open :: FingerPosition
open = 0

-- | a silent (unplayed) string
silent :: FingerPosition
silent = -1

-- | the number of frets we display
displayedFretCount :: Int
displayedFretCount = 6

-- | a fingered string
type FingeredString =
  { stringNumber :: Int
  , fretNumber :: FingerPosition
  }

-- | a barré
type Barre =
  Maybe FingeredString

data MouseAction
  = Barre FingeredString
  | OneFret FingeredString
  | NoFret

-- | The editable chord shape of the guitar
type ChordShape =
  { name :: String -- the chord name
  , firstFretOffset :: Int -- which fret on the guitar does fret 1 represent
  , barre :: Barre -- a barré at this fret from this string to
  -- the final string (5)
  , fingering :: Fingering -- the fingering
  }

-- | the variable attributes of differing fretted instruments
type FrettedInstrumentConfig = 
  { name :: String                   -- the fretted instrument name
  , safeFileName :: String           -- the instrument name as used in file names
  , stringCount :: Int               -- the number of strings
  , maxFrets :: Int                  -- the maximum number of (sounded) frets
  , openStrings :: Fingering         -- fingering for the open strings (up to stringCount of open)
  , openStringsChordName :: String   -- the chord that is therefore produced on open strings
  , openStringMidiIds :: Array Int   -- the MIDI note number of each open string
  , instrumentName :: InstrumentName -- the instrument name of the MIDI SoundFont

  }

