module Piano.Validation (validate, validateJson) where

-- | Validate a Piano chord pattern when loaded from a JSOM file  

import Prelude (($), (>), (<), (>=), (||), (<>), (<$>), const, map, pure, show)
import Data.Array (filter, length)
import Data.Foldable (intercalate)
import Data.Either (either)
import Data.Validation.Semigroup
import Piano.Types (ChordShape, FingerPosition, Fingering)
import Common.Types (Validated)
import Serialization.Json (readPiano)

-- we display 24 piano keys
maxKeys :: Int
maxKeys = 24

-- | validate a prospective piano chord JSON String
validateJson :: String -> Validated ChordShape
validateJson json =
  either
    (const $ invalid $ pure "Not a recognisable piano chord format.")
    validate
    (readPiano json)

-- | validate a prospective piano chord
validate :: ChordShape -> Validated ChordShape
validate chordShape =
  { name: chordShape.name, fingering: _ }
    <$> validateFingering chordShape.fingering

validateFingering :: Fingering -> Validated Fingering
validateFingering fingering =
  if (length fingering > 10) then
    invalid $ pure "Too many fingers."
  else
    validateFingerPositions fingering

validateFingerPositions :: Fingering -> Validated Fingering
validateFingerPositions fingering =
  let
    fingerOutOfRange :: FingerPosition -> Boolean
    fingerOutOfRange fingerPosition =
      fingerPosition < 0 || fingerPosition >= maxKeys
  in
    case (filter fingerOutOfRange fingering) of
      [] ->
        pure fingering
      [ x ] ->
        invalid $ pure $ "Finger position " <> show x <> " is out of range."
      y ->
        let
          fingers =
            intercalate ", " $ map show y
        in
          invalid $ pure $ "Finger positions " <> fingers <> " are out of range."
