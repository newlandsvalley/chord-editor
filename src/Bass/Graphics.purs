-- | draw the fretboard
module Bass.Graphics
  ( canvasHeight
  , canvasWidth
  , titleDepth
  , displayChord
  , fingeredString) where

import Prelude

import Color (black, white, graytone)
import Data.Array (mapWithIndex, null, range)
import Data.Foldable (foldl)
import Data.Int (floor, round, toNumber)
import Data.String.CodeUnits (dropRight, length)
import Graphics.Drawing (Drawing, circle, rectangle, filled, fillColor, text)
import Graphics.Drawing.Font (bold, light, font, sansSerif)
import Bass.Types (ChordShape, Fingering, FingeredString, FingerPosition,
           FretNumber, StringPositions, open, displayedFretCount)
import Bass.FingerStatus (FingerStatus(..))
import Common.Types (MouseCoordinates)

-- | a private manufactured position representing a silent (unplayed) string
-- | unavailable as a finger position to the rest of the app
silent :: FingerPosition
silent =
  { fret : -1, status : Primary }

canvasWidth :: Int
canvasWidth =
  round $ neckWidth + (2.0 * nutxOffset)

canvasHeight :: Int
canvasHeight =
  round $ nutDepth + nutyOffset + stringLength + cellSize

-- | this is the basic size of a cell bounded by 2 frets and 2 strings
-- | and thus represents a unit of scalability
cellSize :: Number
cellSize =
  45.0

neckWidth :: Number
neckWidth =
  stringSeparation * toNumber (stringCount -1)

nutDepth :: Number
nutDepth =
  cellSize / 3.0

titleDepth :: Number
titleDepth =
  35.0

nutyOffset:: Number
nutyOffset =
  70.0

nutxOffset:: Number
nutxOffset =
  cellSize * 1.5

fretDepth :: Number
fretDepth =
  cellSize

fretWidth :: Number
fretWidth =
  2.0

stringCount :: Int
stringCount =
  4

stringSeparation :: Number
stringSeparation =
  cellSize

stringLength :: Number
stringLength =
  (toNumber displayedFretCount) * fretDepth

stringWidth :: Number
stringWidth =
  2.0

nut :: Drawing
nut =
  filled
    (fillColor $ graytone 0.8)
    (rectangle nutxOffset nutyOffset (neckWidth + stringWidth) nutDepth)

fret :: Int -> Drawing
fret n =
  let
    fretyOffset =  toNumber n  * fretDepth
  in
    filled
      (fillColor black)
      (rectangle nutxOffset (nutDepth + nutyOffset + fretyOffset) neckWidth fretWidth)

-- draw thw frets
frets :: Drawing
frets =
  let
    fretNums = range 1 displayedFretCount
    f :: Drawing -> Int -> Drawing
    f acc n = acc <> (fret n)
  in
    foldl f mempty fretNums

aString :: Int -> Drawing
aString n =
  let
    xOffset =  nutxOffset + toNumber n  * stringSeparation
    yOffset = nutDepth + nutyOffset
  in
    filled
      (fillColor black)
      (rectangle xOffset (nutDepth + nutyOffset) stringWidth stringLength)

-- | draw the strings
strings :: Drawing
strings =
  let
    stringNums = range 0 (stringCount -1)
    f :: Drawing -> Int -> Drawing
    f acc n = acc <> (aString n)
  in
    foldl f mempty stringNums

-- | an open circle above the nut represents an open string
openString :: Int -> Drawing
openString stringNum =
  let
    outerRadius = 0.4 * fretDepth / 2.0
    innerRadius = outerRadius - 2.0
    xpos = nutxOffset + (toNumber stringNum * stringSeparation)
    ypos = nutyOffset - (outerRadius + 4.0)
  in
    filled
      (fillColor black)
      (circle xpos ypos outerRadius)
    <>
      filled
        (fillColor white)
        (circle xpos ypos innerRadius)

-- | a cross above the nut indicates a string which should not be played
silentString :: Int -> Drawing
silentString stringNum =
  let
    barLength = 0.25 * stringSeparation
    theFont = font sansSerif 26 bold
    xpos = nutxOffset +
           (toNumber stringNum * stringSeparation)
           - barLength
    ypos = nutyOffset - (nutDepth * 0.5)
  in
    text theFont xpos ypos (fillColor black) "x"

-- | draw all the fingers on a single string
fingers :: Int -> StringPositions -> Drawing
fingers stringNum stringPositions =
  let
    f :: Drawing -> FingerPosition -> Drawing
    f acc fretNum =
      acc <> finger stringNum fretNum
  in
    if (null stringPositions) then
      finger stringNum silent
    else
      foldl f mempty stringPositions

-- | draw a single finger on a string.
finger :: Int -> FingerPosition -> Drawing
finger stringNum fingerPosition  =
  if
    (fingerPosition.fret > displayedFretCount) ||
    (stringNum < 0) || (stringNum >= stringCount)
  then
    mempty
  else if (fingerPosition.fret == open.fret) then
    openString stringNum
  else if (fingerPosition.fret == silent.fret) then
    silentString stringNum
  else
    case fingerPosition.status of
      Primary ->
        primaryFinger stringNum fingerPosition.fret
      Secondary ->
        secondaryFinger stringNum fingerPosition.fret

-- | a primary finger is the prominent note on the bass and is the first
-- | string fingered
primaryFinger :: Int -> FretNumber -> Drawing
primaryFinger stringNum fretNumber =
  let
    radius = 0.6 * fretDepth / 2.0
    ypos = nutDepth + nutyOffset + (toNumber fretNumber * fretDepth) - (radius + 2.0)
    xpos = nutxOffset + (toNumber stringNum * stringSeparation)
  in
    filled
      (fillColor black)
      (circle xpos ypos radius)

-- | a secondary finger is a subsidiary string fingered after the primary one
secondaryFinger :: Int -> FretNumber -> Drawing
secondaryFinger stringNum fretNumber =
  let
    side = fretDepth / 2.0
    ypos = nutDepth + nutyOffset + (toNumber fretNumber * fretDepth) - (side + 4.0)
    xpos = nutxOffset + (toNumber stringNum * stringSeparation) - (side / 2.0)
  in
    filled
      (fillColor $ graytone 0.5)
      (rectangle xpos ypos side side)

-- | draw the complete fingering
fingering :: Fingering -> Drawing
fingering fingerSpec =
  foldl (<>) mempty $ mapWithIndex fingers fingerSpec

-- | work out a fingered string from the mouse click coordinates
fingeredString :: MouseCoordinates -> FingeredString
fingeredString coords  =
  let
    stringNumber =
      if coords.x < (nutxOffset / 2.0) then
        0
      else
        floor $ (coords.x - (nutxOffset / 2.0)) / stringSeparation

    fretNumber =
      if coords.y < (nutDepth + nutyOffset) then
        0
      else
        (floor $ (coords.y - (nutDepth + nutyOffset)) / fretDepth) + 1
    in
      { stringNumber : min stringNumber (stringCount - 1)
      , fretNumber  : min fretNumber displayedFretCount
      }

-- | display the chord diagram title, but constrain it to live within
-- | the width of the nut, roughly centered
title :: String -> Drawing
title name =
  let
    theFont = font sansSerif 35 bold
    -- we seem to have space for at most 9 characters so truncate the name
    -- where necessary
    displayName =
      if (length name > 9) then
        dropRight (length name - 9) name
      else
        name
    -- rough heuristic for the width in pixels
    textWidth = (cellSize / 1.8) * (toNumber $ length displayName)
    -- work out the title x offset
    unsafeTitlexOffset = nutxOffset + (neckWidth / 2.0) - (textWidth / 2.0)
    -- but restrict it to start at the nut start for long strings
    titlexOffset = max unsafeTitlexOffset nutxOffset
  in
    text theFont titlexOffset titleDepth (fillColor black) displayName

-- | display the label of the first fret
firstFretLabel :: Int -> Drawing
firstFretLabel fretNo =
  if
    (fretNo < 1  || fretNo >= 10) then
      mempty
  else
    let
      theFont = font sansSerif 20 light
      displayNumber = show fretNo
      xpos = nutxOffset * 0.6
      ypos = nutDepth + nutyOffset + (fretDepth * 0.6)
  in
    text theFont xpos ypos (fillColor black) displayNumber


-- | display the enire choords hape described by the fingering
displayChord :: ChordShape -> Drawing
displayChord chord  =
  title chord.name <>
        nut <>
        frets <>
        strings <>
        (fingering chord.fingering) <>
        firstFretLabel chord.firstFretOffset
