module Bass.Audio (playChord) where

-- | play the guitar chord through Web Audio by means of generating
-- | the MIDI notes

import Prelude (Unit, ($), (+), (*), (>), join, map, void)
import Effect (Effect)
import Audio.SoundFont (Instrument, MidiNote, playNotes)
import Data.Array (filter, index, mapWithIndex)
import Data.Maybe (fromMaybe)
import Data.Int (toNumber)
import Bass.Types (Fingering, FingerPosition, StringPositions)

bass :: Int
bass = 0

-- | the MIDI note number of each bass guitar open string
openStringMidiIds :: Array Int
openStringMidiIds =
  [ 28, 33, 38, 43 ]

-- | generate a MIDI note from the note fingering information
toNote :: Int -> Int -> FingerPosition -> MidiNote
toNote firstFretOffset stringNumber fingerPosition =
  let
    openStringId :: Int
    openStringId =
      fromMaybe 0 $ index openStringMidiIds stringNumber
    id =
      openStringId + fingerPosition.fret + firstFretOffset
  in
    { channel: bass, id, timeOffset: 0.0, duration: 0.8, gain: 1.0 }

-- | filter any notes that are not to be played
-- | shouldn't be necessary because we shouldn't be able to provide a string
-- | number out of range
clean :: Array MidiNote -> Array MidiNote
clean notes =
  filter (\note -> note.id > 0) notes

-- | generate all the notes for one string (to be played at the same time)
toStringNotes :: Int -> Int -> StringPositions -> Array MidiNote
toStringNotes firstFretOffset stringNumber stringPositions =
  clean $ map (toNote firstFretOffset stringNumber) stringPositions

-- | arpeggiate the notes - with a delay of 0.7 seconds between the start of each
arpeggiate :: Array MidiNote -> Array MidiNote
arpeggiate notes =
  let
    f :: Int -> MidiNote -> MidiNote
    f ix note =
      note { timeOffset = toNumber ix * 0.7 }
  in
    mapWithIndex f notes

-- | play the bass guitar chord pattern indicated by the fingering
playChord :: Fingering -> Int -> Array Instrument -> Effect Unit
playChord fingering firstFretOffset instruments =
  do
    let
      notes = arpeggiate $ join $ mapWithIndex (toStringNotes firstFretOffset) fingering
    void $ playNotes instruments notes
