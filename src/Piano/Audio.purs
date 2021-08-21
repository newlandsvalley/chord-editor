module Piano.Audio (playChord) where

import Prelude (Unit, ($), (+), map, void)
import Effect (Effect)
import Audio.SoundFont (Instrument, MidiNote, playNotes)
import Piano.Types (Fingering)

piano :: Int
piano = 0

toNote :: Int -> MidiNote
toNote keyPosition =
  let
    id = 60 + keyPosition
  in
    { channel: piano, id, timeOffset: 0.0, duration: 1.5, gain: 1.0 }

playChord :: Fingering -> Array Instrument -> Effect Unit
playChord fingering instruments =
  do
    let
      notes = map toNote fingering
    void $ playNotes instruments notes
