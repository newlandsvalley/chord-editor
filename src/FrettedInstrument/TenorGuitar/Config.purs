module FrettedInstrument.TenorGuitar.Config where

import FrettedInstrument.Types (FrettedInstrumentConfig, FrettedInstrumentName(TenorGuitar))
import Data.Midi.Instrument (InstrumentName(AcousticGuitarSteel))

config :: FrettedInstrumentConfig 
config = 
  { name: TenorGuitar  
  , stringCount: 4
  , maxFrets: 20
  , openStringsChordName: "C6/9"
  , openStringMidiIds: [ 48, 55, 62, 69 ]
  , instrumentName: AcousticGuitarSteel
  }
