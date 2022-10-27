module FrettedInstrument.Types where

-- | This module extracts types representing the common behaviour of fretted instruments 
-- | such as guitar, tenor guitar and ukelele where chords are played 'normally'.
-- | It does not include bass guitar where they are played as an arpeggio and where,
-- | therefore the behaviour is different.

import Prelude
import Data.Array (replicate)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String.Common (replaceAll, toLower)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Utils (filter)
import Data.Midi.Instrument (InstrumentName)

-- | An enumeration of each fretted instrument we support
-- | much of this stuff about FrettedInstrumentName is to support routing
-- | where we use a URI of `FrettedInstrument` followed by the actual name
data FrettedInstrumentName =
    Guitar
  | TenorGuitar
  | Ukulele

-- | show is used in displays of the name
instance showFrettedInstrumentName :: Show FrettedInstrumentName where
  show Guitar = "Guitar"
  show TenorGuitar  = "Tenor Guitar"
  show Ukulele  = "Ukulele"

derive instance eqFrettedInstrumentName :: Eq FrettedInstrumentName
derive instance ordFrettedInstrumentName :: Ord FrettedInstrumentName
 
-- | we represent all instrument names in URIs as lower-case names
-- | with blanks removed from the 'show' name
instrumentNameToURIString :: FrettedInstrumentName -> String
instrumentNameToURIString =
  toLower <<< (filter (\s -> s /= " ")) <<< show

-- | try to read a URI string component and create a FrettedInstrumentName
instrumentNameFromURIString :: String -> Either String FrettedInstrumentName
instrumentNameFromURIString s =
  case (readInstrumentName s) of
    Just name ->
      Right name
    Nothing ->
      Left $ "Not a known fretted instrument: " <> s

  where 
  readInstrumentName :: String -> Maybe FrettedInstrumentName
  readInstrumentName nameStr =
    case nameStr of
      "guitar"    -> Just Guitar
      "tenorguitar"  -> Just TenorGuitar
      "ukulele"  -> Just Ukulele
      _ -> Nothing


-- | we represent all instrument names in file names as lower-case names
-- | with blanks replaced by an underscore
instrumentNameToFileName :: FrettedInstrumentName -> String
instrumentNameToFileName =
  toLower <<< replaceBlanks <<< show

  where 
    replaceBlanks = replaceAll (Pattern " ") (Replacement "_")

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

-- | for each string in the instrument, mark the default finger position as open
openStrings :: Int -> Fingering
openStrings stringCount = 
  replicate stringCount open

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

-- | The editable chord shape of the fretted instrument
type ChordShape =
  { name :: String -- the chord name
  , firstFretOffset :: Int -- which fret on the instrument does fret 1 represent
  , barre :: Barre -- a barré at this fret from this string to the final string (5)
  , fingering :: Fingering -- the fingering
  }

-- | the variable attributes of differing fretted instruments
type FrettedInstrumentConfig = 
  { name :: FrettedInstrumentName    -- the fretted instrument name
  , stringCount :: Int               -- the number of strings
  , maxFrets :: Int                  -- the maximum number of (sounded) frets
  , openStringsChordName :: String   -- the chord that is therefore produced on open strings
  , openStringMidiIds :: Array Int   -- the MIDI note number of each open string
  , instrumentName :: InstrumentName -- the instrument name of the MIDI SoundFont
  }

