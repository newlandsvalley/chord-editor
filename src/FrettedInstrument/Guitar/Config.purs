module FrettedInstrument.Guitar.Config where

import FrettedInstrument.Types (FrettedInstrumentConfig, open)
import Data.Midi.Instrument (InstrumentName(AcousticGuitarSteel))

config :: FrettedInstrumentConfig 
config = 
  { name: "Guitar"    
  , safeFileName: "guitar"
  , stringCount: 6
  , maxFrets: 27
  , openStrings:  [ open, open, open, open, open, open ]
  , openStringsChordName: "Em7+11"
  , openStringMidiIds:  [ 40, 45, 50, 55, 59, 64 ]
  , instrumentName: AcousticGuitarSteel
  }
