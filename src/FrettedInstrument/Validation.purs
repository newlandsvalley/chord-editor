module FrettedInstrument.Validation (validate, validateJson) where

-- | Validate a Fretted Instrument chord pattern when loaded from a JSON file

import Prelude (($), (>), (<), (>=), (<=), (||), (&&), (<>), (<$>), (<*>), (/=), (-), const, map, negate, pure, show)
import Data.Array (filter, length, mapWithIndex)
import Data.Foldable (intercalate)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import Data.Validation.Semigroup
import FrettedInstrument.Types
  ( ChordShape
  , Barre
  , FingeredString
  , FingerPosition
  , Fingering
  , FrettedInstrumentConfig
  , displayedFretCount
  )
import Common.Types (Validated)
import Serialization.Json (readFrettedInstrument)

-- | validate a prospective fretted instrument chord JSON String
validateJson ::  FrettedInstrumentConfig -> String -> Validated ChordShape
validateJson config json =
  either
    (const $ invalid $ pure message)
    (validate config)
    (readFrettedInstrument json)
  where
    message = "Not a recognisable " <> (toLower config.name) <> " chord format."

-- | validate a prospective fretted instrument chord
validate :: FrettedInstrumentConfig -> ChordShape -> Validated ChordShape
validate config chordShape =
  ( { name: chordShape.name, firstFretOffset: _, barre: _, fingering: _ }
      <$> validateFirstFretOffset config chordShape.firstFretOffset
      <*> validateBarre chordShape.barre
      <*> validateFingering config chordShape.fingering
  ) `andThen` checkHiddenByBarre

validateFirstFretOffset :: FrettedInstrumentConfig -> Int -> Validated Int
validateFirstFretOffset config offset =
  if (offset < 0) || (offset > config.maxFrets) then
    invalid $ pure ("First fret offset should be between 0 and " <> show config.maxFrets <> ".")
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

validateFingering :: FrettedInstrumentConfig -> Fingering -> Validated Fingering
validateFingering config fingering =
  if (length fingering /= config.stringCount) then
    invalid $ pure message
  else
    validateFingerPositions config fingering
  where 
      message = "Fingering for all " <> show config.stringCount <> " strings is required."


validateFingerPositions :: FrettedInstrumentConfig -> Fingering -> Validated Fingering
validateFingerPositions config fingering =
  let
    fingerOutOfRange :: FingerPosition -> Boolean
    fingerOutOfRange fingerPosition =
      fingerPosition < -1 || fingerPosition >= config.maxFrets
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
