module FrettedInstrument.TenorGuitar.Config where

import FrettedInstrument.Types (FrettedInstrumentConfig, open)
import Data.Midi.Instrument (InstrumentName(AcousticGuitarSteel))

config :: FrettedInstrumentConfig 
config = 
  { name: "Tenor Guitar"    
  , safeFileName: "tenor_guitar"
  , stringCount: 4
  , maxFrets: 20
  , openStrings: [ open, open, open, open ]
  , openStringsChordName: "C6/9"
  , openStringMidiIds: [ 48, 55, 62, 69 ]
  , instrumentName: AcousticGuitarSteel
  }
