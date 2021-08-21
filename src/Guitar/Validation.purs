module Guitar.Validation (validate, validateJson) where

-- | Validate a Guitar chord pattern when loaded from a JSOM file

import Prelude (($), (>), (<), (>=), (<=), (||), (&&), (<>), (<$>), (<*>), (/=), (-), const, map, negate, pure, show)
import Data.Array (filter, length, mapWithIndex)
import Data.Foldable (intercalate)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup
import Guitar.Types
  ( ChordShape
  , Barre
  , FingeredString
  , FingerPosition
  , Fingering
  , displayedFretCount
  )
import Common.Types (Validated)
import Serialization.Json (readGuitar)

-- seems like 27 is about the maximum possible
maxFrets :: Int
maxFrets = 27

-- | validate a prospective guitar chord JSON String
validateJson :: String -> Validated ChordShape
validateJson json =
  either
    (const $ invalid $ pure "Not a recognisable guitar chord format.")
    validate
    (readGuitar json)

-- | validate a prospective guitar chord
validate :: ChordShape -> Validated ChordShape
validate chordShape =
  ( { name: chordShape.name, firstFretOffset: _, barre: _, fingering: _ }
      <$> validateFirstFretOffset chordShape.firstFretOffset
      <*> validateBarre chordShape.barre
      <*> validateFingering chordShape.fingering
  ) `andThen` checkHiddenByBarre

validateFirstFretOffset :: Int -> Validated Int
validateFirstFretOffset offset =
  if (offset < 0) || (offset > maxFrets) then
    invalid $ pure ("First fret offset should be between 0 and " <> show maxFrets <> ".")
  else
    pure offset

validateBarre :: Barre -> Validated Barre
validateBarre mbarre =
  case mbarre of
    Just barre ->
      if (barre.stringNumber < 0 || barre.stringNumber >= 6) then
        invalid $ pure
          ( "Invalid string number of "
              <> show barre.stringNumber
              <> " in the barré."
          )
      else if (barre.fretNumber < 1 || barre.fretNumber >= displayedFretCount) then
        invalid $ pure
          ( "Invalid fret number of "
              <> show barre.fretNumber
              <> " in the barré which should be between 1 and "
              <> show (displayedFretCount - 1)
              <> "."
          )
      else
        pure mbarre
    _ ->
      pure mbarre

validateFingering :: Fingering -> Validated Fingering
validateFingering fingering =
  if (length fingering /= 6) then
    invalid $ pure "Fingering for all 6 strings is required."
  else
    validateFingerPositions fingering

validateFingerPositions :: Fingering -> Validated Fingering
validateFingerPositions fingering =
  let
    fingerOutOfRange :: FingerPosition -> Boolean
    fingerOutOfRange fingerPosition =
      fingerPosition < -1 || fingerPosition >= maxFrets
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

checkHiddenByBarre :: ChordShape -> Validated ChordShape
checkHiddenByBarre chordShape =
  case chordShape.barre of
    Just _ -> -- barre
      let
        f :: Int -> FingerPosition -> Int
        f stringNumber fretNumber =
          if
            ( hiddenByBarre chordShape.barre
                { stringNumber: stringNumber, fretNumber: fretNumber }
            ) then
            stringNumber
          else
            -1
        hiddenStrings = filter (\s -> s >= 0) $ mapWithIndex f chordShape.fingering
      in
        case hiddenStrings of
          [] ->
            pure chordShape
          [ x ] ->
            invalid $ pure $ "Fingering for string " <> show x <> " is hidden by the barré."
          y ->
            let
              fingers =
                intercalate ", " $ map show y
            in
              invalid $ pure $ "Fingering for strings " <> fingers <> " is hidden by the barré."
    _ ->
      pure chordShape

-- | return true if the string at this fingering is hidden by the barré
hiddenByBarre :: Barre -> FingeredString -> Boolean
hiddenByBarre mBarre fs =
  case mBarre of
    Just barre ->
      fs.fretNumber > 0
        && barre.stringNumber <= fs.stringNumber
        &&
          barre.fretNumber >= fs.fretNumber
    _ ->
      false
