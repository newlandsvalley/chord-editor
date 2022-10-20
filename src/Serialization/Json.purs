module Serialization.Json where

import Data.Either (Either)
import Foreign (MultipleErrors)
import Piano.Types as Piano
import FrettedInstrument.Types as FrettedInstrument
import Bass.Types as Bass
import Yoga.JSON as JSON

writePiano :: Piano.ChordShape -> String
writePiano =
  JSON.writeJSON

readPiano :: String -> Either MultipleErrors Piano.ChordShape
readPiano =
  JSON.readJSON
  
writeFrettedInstrument :: FrettedInstrument.ChordShape -> String
writeFrettedInstrument =
  JSON.writeJSON

readFrettedInstrument :: String -> Either MultipleErrors FrettedInstrument.ChordShape
readFrettedInstrument =
  JSON.readJSON

writeBass :: Bass.ChordShape -> String
writeBass =
  JSON.writeJSON

readBass :: String -> Either MultipleErrors Bass.ChordShape
readBass =
  JSON.readJSON
