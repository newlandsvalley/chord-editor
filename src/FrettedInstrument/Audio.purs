module FrettedInstrument.Audio
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
import FrettedInstrument.Types (FrettedInstrumentConfig, Fingering, FingerPosition, Barre)

channel :: Int
channel = 0

-- | generate a MIDI note from the note fingering information
toNote :: FrettedInstrumentConfig -> Int -> Barre -> Int -> FingerPosition -> MidiNote
toNote config firstFretOffset mBarre stringNumber fingerPosition =
  let
    openStringId :: Int
    openStringId = fromMaybe 0 $ index config.openStringMidiIds stringNumber
    actualFret :: Int
    actualFret = possiblyBarredFret stringNumber fingerPosition mBarre
    id =
      if (actualFret < 0) then
        0
      else
        openStringId + actualFret + firstFretOffset
  in
    { channel, id, timeOffset: 0.0, duration: 2.0, gain: 1.0 }

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

-- | play the fretted instrument chord indicated by the fingering
playChord :: FrettedInstrumentConfig -> Fingering -> Int -> Barre -> Array Instrument -> Effect Unit
playChord config fingering firstFretOffset mBarre instruments =
  do
    let
      notes = mapWithIndex (toNote config firstFretOffset mBarre) fingering
    void $ playNotes instruments notes

getMidiPitches :: FrettedInstrumentConfig -> Fingering -> Int -> Barre -> Array Int
getMidiPitches config fingering firstFretOffset mBarre =
  map _.id notes

  where
  notes = mapWithIndex (toNote config firstFretOffset mBarre) fingering
