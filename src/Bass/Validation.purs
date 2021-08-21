module Bass.Validation (validate, validateJson) where

-- | Validate a Bass chord pattern when loaded from a JSOM file

import Prelude (($), (>), (<), (||), (<>), (<$>), (<*>), (/=), const, map, join, negate, pure, show)
import Data.Array (filter, length)
import Data.Either (either)
import Data.Foldable (intercalate)
import Data.Validation.Semigroup
import Bass.Types (ChordShape, FingerPosition, Fingering, displayedFretCount)
import Common.Types (Validated)
import Serialization.Json (readBass)

maxFrets :: Int
maxFrets = 36

-- | validate a prospective bass chord JSON String
validateJson :: String -> Validated ChordShape
validateJson json =
  either
    (const $ invalid $ pure "Not a recognisable bass chord format.")
    validate
    (readBass json)

-- | validate a prospective bass guitar chord pattern
validate :: ChordShape -> Validated ChordShape
validate chordShape =
  { name: chordShape.name, firstFretOffset: _, fingering: _ }
    <$> validateFirstFretOffset chordShape.firstFretOffset
    <*> validateFingering chordShape.fingering

validateFirstFretOffset :: Int -> Validated Int
validateFirstFretOffset offset =
  if (offset < 0) || (offset > maxFrets) then
    invalid $ pure ("First fret offset should be between 0 and " <> show maxFrets <> ".")
  else
    pure offset

validateFingering :: Fingering -> Validated Fingering
validateFingering fingering =
  if (length fingering /= 4) then
    invalid $ pure "Fingering for all 4 strings is required."
  else
    validateFingerPositions fingering

validateFingerPositions :: Fingering -> Validated Fingering
validateFingerPositions fingering =
  let
    fingerOutOfRange :: FingerPosition -> Boolean
    fingerOutOfRange fingerPosition =
      fingerPosition.fret < (-1) || fingerPosition.fret > displayedFretCount
  in
    case (filter fingerOutOfRange (join fingering)) of
      [] ->
        pure fingering
      [ f ] ->
        invalid $ pure $ "Finger position " <> show f.fret <> " is out of range."
      y ->
        let
          fingers =
            intercalate ", " $ map (\fp -> show fp.fret) y
        in
          invalid $ pure $ "Finger positions " <> fingers <> " are out of range."
