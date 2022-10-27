module FrettedInstrument.Guitar.Config where

import FrettedInstrument.Types (FrettedInstrumentConfig, FrettedInstrumentName(Guitar))
import Data.Midi.Instrument (InstrumentName(AcousticGuitarSteel))

config :: FrettedInstrumentConfig 
config = 
  { name: Guitar 
  , stringCount: 6
  , maxFrets: 27
  , openStringsChordName: "Em7+11"
  , openStringMidiIds:  [ 40, 45, 50, 55, 59, 64 ]
  , instrumentName: AcousticGuitarSteel
  }
