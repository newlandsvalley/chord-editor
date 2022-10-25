module FrettedInstrument.Ukulele.Config where

import FrettedInstrument.Types (FrettedInstrumentConfig, FrettedInstrumentName(Ukulele), open)
import Data.Midi.Instrument (InstrumentName(AcousticGuitarNylon))

config :: FrettedInstrumentConfig 
config = 
  { name: Ukulele 
  , stringCount: 4
  , maxFrets: 15
  , openStrings:  [ open, open, open, open ]
  , openStringsChordName: "C6"
  , openStringMidiIds:  [ 67, 60, 64, 69 ]
  , instrumentName: AcousticGuitarNylon
  }
