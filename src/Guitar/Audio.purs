module Guitar.Audio
  ( getMidiPitches
  , playChord
  ) where

-- | play the guitar chord through Web Audio by means of generating
-- | the MIDI notes

import Prelude (Unit, ($), (<), (<=), (+), map, max, void)
import Effect (Effect)
import Audio.SoundFont (Instrument, MidiNote, playNotes)
import Data.Array (index, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Guitar.Types (Fingering, FingerPosition, Barre)

guitar :: Int
guitar = 0

-- | the MIDI note number of each guitar open string
openStringMidiIds :: Array Int
openStringMidiIds =
  [ 40, 45, 50, 55, 59, 64 ]

-- | generate a MIDI note from the note fingering information
toNote :: Int -> Barre -> Int -> FingerPosition -> MidiNote
toNote firstFretOffset mBarre stringNumber fingerPosition =
  let
    openStringId :: Int
    openStringId = fromMaybe 0 $ index openStringMidiIds stringNumber
    actualFret :: Int
    actualFret = possiblyBarredFret stringNumber fingerPosition mBarre
    id =
      if (actualFret < 0) then
        0
      else
        openStringId + actualFret + firstFretOffset
  in
    { channel: guitar, id, timeOffset: 0.0, duration: 2.0, gain: 1.0 }

-- | work out the actual fretted position taking note of the fret position
-- | and any possible barré
possiblyBarredFret :: Int -> FingerPosition -> Barre -> FingerPosition
possiblyBarredFret stringNumber fingerPosition mBarre =
  case mBarre of
    Just fingeredString ->
      if (fingeredString.stringNumber <= stringNumber) then
        max fingeredString.fretNumber fingerPosition
      else
        fingerPosition
    _ ->
      fingerPosition

-- | play the guitar chord indicated by the fingering
playChord :: Fingering -> Int -> Barre -> Array Instrument -> Effect Unit
playChord fingering firstFretOffset mBarre instruments =
  do
    let
      notes = mapWithIndex (toNote firstFretOffset mBarre) fingering
    void $ playNotes instruments notes

getMidiPitches :: Fingering -> Int -> Barre -> Array Int
getMidiPitches fingering firstFretOffset mBarre =
  map _.id notes

  where
  notes = mapWithIndex (toNote firstFretOffset mBarre) fingering
