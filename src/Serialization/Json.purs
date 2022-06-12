module Serialization.Json where

import Data.Either (Either)
import Foreign (MultipleErrors)
import Piano.Types as Piano
import Guitar.Types as Guitar
import Bass.Types as Bass
import Yoga.JSON as JSON

writePiano :: Piano.ChordShape -> String
writePiano =
  JSON.writeJSON

readPiano :: String -> Either MultipleErrors Piano.ChordShape
readPiano =
  JSON.readJSON

writeGuitar :: Guitar.ChordShape -> String
writeGuitar =
  JSON.writeJSON

readGuitar :: String -> Either MultipleErrors Guitar.ChordShape
readGuitar =
  JSON.readJSON

writeBass :: Bass.ChordShape -> String
writeBass =
  JSON.writeJSON

readBass :: String -> Either MultipleErrors Bass.ChordShape
readBass =
  JSON.readJSON
