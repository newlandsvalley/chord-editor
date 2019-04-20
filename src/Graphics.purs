-- | draw the fretboard
module Graphics
  ( canvasHeight
  , canvasWidth
  , displayChord
  , fingeredString) where

import Prelude

import Color (Color, rgb, black, white)
import Data.Array (mapWithIndex, range)
import Data.Foldable (foldl)
import Data.Int (floor, round, toNumber)
import Math (pi)
import Graphics.Drawing (Drawing, circle, rectangle, filled, fillColor)
import Types (Fingering, FingeredString, MouseCoordinates)

gray :: Color
gray = rgb 160 160 160

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
  36.0

neckWidth :: Number
neckWidth =
  stringSeparation * toNumber (stringCount -1)

nutDepth :: Number
nutDepth =
  cellSize / 3.0

nutyOffset:: Number
nutyOffset =
  50.0

nutxOffset:: Number
nutxOffset =
  cellSize

fretDepth :: Number
fretDepth =
  cellSize

fretWidth :: Number
fretWidth =
  2.0

fretCount :: Int
fretCount =
  6

stringCount :: Int
stringCount =
  6

stringSeparation :: Number
stringSeparation =
  cellSize

stringLength :: Number
stringLength =
  (toNumber fretCount) * fretDepth

stringWidth :: Number
stringWidth =
  2.0

nut :: Drawing
nut =
  filled
    (fillColor gray)
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
    fretNums = range 1 fretCount
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
    outerRadius = 0.5 * fretDepth / 2.0
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

{-}
-- | a cross above the nut indicates a string which should not be played
deadString :: Int -> Drawing
deadString stringNum =
  let
    barLength = 0.25 * stringSeparation
    barWidth = barLength / 3.0
    outerRadius = 0.5 * fretDepth / 2.0
    xpos = nutxOffset +
           (toNumber stringNum * stringSeparation)
           - barLength
    ypos = nutyOffset - (outerRadius + 4.0)
  in
      filled
        (fillColor black)
        (rectangle xpos ypos (2.0 * barLength) barWidth)
      <>
        filled
          (fillColor black)
          (rectangle (xpos - 2.0 + barLength)
                     (ypos + 2.0 - barLength)
                     barWidth
                     (2.0 * barLength))
-}

-- | draw a single finger on a string
finger :: Int -> Int -> Drawing
finger stringNum fretNum  =
  let
    radius = 0.7 * fretDepth / 2.0
    xpos = nutxOffset + (toNumber stringNum * stringSeparation)
    ypos = nutDepth + nutyOffset + (toNumber fretNum * fretDepth) - (radius + 2.0)
  in
    if
      (fretNum < 0) || (fretNum >= fretCount) ||
      (stringNum < 0) || (stringNum >= stringCount)
    then
      mempty
    else if (fretNum == 0) then
      openString stringNum
    else
        filled
          (fillColor black)
          (circle xpos ypos radius)

-- | draw the complete fingering
fingering :: Array Int -> Drawing
fingering fingerSpec =
  foldl (<>) mempty $ mapWithIndex finger fingerSpec


-- | work out a fingered string from the mouse click coordinates
fingeredString :: MouseCoordinates -> FingeredString
fingeredString coords =
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
      , fretNumber  : min fretNumber fretCount
      }


-- | display the enire choords hape described by the fingering
displayChord :: Fingering -> Drawing
displayChord chord =
  nut <> frets <> strings <> (fingering chord)
