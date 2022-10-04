module TenorGuitar.Audio
  ( getMidiPitches
  , playChord
  ) where

-- | play the tenorguitar chord through Web Audio by means of generating
-- | the MIDI notes

import Prelude (Unit, ($), (<), (<=), (+), map, max, void)
import Effect (Effect)
import Audio.SoundFont (Instrument, MidiNote, playNotes)
import Data.Array (index, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import TenorGuitar.Types (Fingering, FingerPosition, Barre)

tenorGuitar :: Int
tenorGuitar = 0

-- | the MIDI note number of each tenor guitar open string: C3 G4 D4 A4
openStringMidiIds :: Array Int
openStringMidiIds =
  [ 48, 55, 62, 69 ]

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
    { channel: tenorGuitar, id, timeOffset: 0.0, duration: 2.0, gain: 1.0 }

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

-- | play the tenorguitar chord indicated by the fingering
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
