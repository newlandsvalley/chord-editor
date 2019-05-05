-- | draw the fretboard
module Piano.Graphics
  ( canvasHeight
  , canvasWidth
  , displayChord
  , fingeredKey
  ) where

import Prelude

import Color (black, white)
import Data.Array (index, range)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)
import Data.Int (floor, round, toNumber)
import Data.String.CodeUnits (length)
import Graphics.Drawing (Drawing, circle, rectangle, filled, fillColor, text)
import Graphics.Drawing.Font (bold, font, sansSerif)
import Piano.Types (DiagramParameters, Fingering, MouseCoordinates)

-- | the index positions of the black keys in the double octave
blackKeyPositions :: Array Int
blackKeyPositions = [1,3,6,8,10,13,15,18,20,22]

-- | the index positions of the white keys in the double octave
whiteKeyPositions :: Array Int
whiteKeyPositions = [0,2,4,5,7,9,11,12,14,16,17,19,21,23]

-- | the actual x offsets from the start of the first ket for each black note
-- | n.b. This is not completely regular because 7 white notes  and 12 black or
-- | white notes have no real common multiple, so we massage the black keys at
-- | positions 6 and 18 a little to make it look OK.
blackKeyOffsets :: Array Number
blackKeyOffsets =
  let
    f :: Int -> Number
    f pos =
      if (pos == 6 || pos == 18 ) then
        toNumber pos * blackKeyWidth + 6.0
      else
        toNumber pos * blackKeyWidth
  in
    map f blackKeyPositions

canvasWidth :: Int
canvasWidth =
  round $ (2.0 * keyboardxOffset) + (toNumber whiteNoteCount * whiteKeyWidth)

canvasHeight :: Int
canvasHeight =
  round $ keyboardyOffset + whiteKeyLength + cellSize

-- | this is the basic size of a cell representing s single semitone
cellSize :: Number
cellSize =
  24.0

titleDepth :: Number
titleDepth =
  35.0

keyboardyOffset:: Number
keyboardyOffset =
  3.0 * cellSize

keyboardxOffset:: Number
keyboardxOffset =
  cellSize * 1.5

whiteNoteCount :: Int
whiteNoteCount =
  14

-- | width of a black key and also the portion of a white key at the top of
-- | the keyboard
blackKeyWidth :: Number
blackKeyWidth =
  cellSize

blackKeyLength  :: Number
blackKeyLength =
  whiteKeyLength * 0.66

keyboardWidth :: Number
keyboardWidth =
  toNumber whiteNoteCount * whiteKeyWidth

-- | width of a white key at the base of the keyboard
-- | 7 white keys occupy the same space as 12 black/white tops
whiteKeyWidth  :: Number
whiteKeyWidth =
  cellSize * 12.0 / 7.0

-- | the full length of a white key
whiteKeyLength  :: Number
whiteKeyLength =
  cellSize * 9.0

whiteKey :: Int -> Drawing
whiteKey n =
  let
    xOffset =  keyboardxOffset + toNumber n  * whiteKeyWidth
    yOffset = keyboardyOffset
  in
    filled
      (fillColor black)
      (rectangle xOffset yOffset whiteKeyWidth whiteKeyLength)
    <>
      filled
        (fillColor white)
        (rectangle (xOffset + 2.0) (yOffset + 2.0) (whiteKeyWidth -4.0) (whiteKeyLength -4.0))

blackKey :: Number -> Drawing
blackKey keyOffset =
  let
    xOffset =  keyboardxOffset + keyOffset
    yOffset = keyboardyOffset
  in
    filled
      (fillColor black)
      (rectangle xOffset yOffset blackKeyWidth blackKeyLength)

-- | draw the white keys
whiteKeys :: Drawing
whiteKeys =
  let
    keys = range 0 (whiteNoteCount -1)
    f :: Drawing -> Int -> Drawing
    f acc n = acc <> (whiteKey n)
  in
    foldl f mempty keys

-- | draw the black keys
blackKeys :: Drawing
blackKeys =
  let
    f :: Drawing -> Number -> Drawing
    f acc n = acc <> (blackKey n)
  in
    foldl f mempty blackKeyOffsets

-- | display the chord diagram title, but constrain it to live within
-- | the width of the nut, roughly centered
title :: String -> Drawing
title name =
  let
    theFont = font sansSerif 35 bold
    -- rough heuristic for the width in pixels
    textWidth = (cellSize / 1.5) * (toNumber $ length name)
    -- roughly center
    titlexOffset = keyboardxOffset + ((keyboardWidth - textWidth) / 2.0)
  in
    text theFont titlexOffset titleDepth (fillColor black) name

-- | an circle above the key shows it to be selected in a chord
selectedKey :: Int -> Drawing
selectedKey keyNum =
  let
    radius = 0.3 * cellSize
    xpos = keyboardxOffset + (toNumber keyNum * blackKeyWidth) + (blackKeyWidth / 2.0)
    ypos = keyboardyOffset - (radius + 4.0)
  in
    filled
      (fillColor black)
      (circle xpos ypos radius)

-- | draw the circles above the selected keys
selectedKeys :: Array Int -> Drawing
selectedKeys fingers =
  let
    f :: Drawing -> Int -> Drawing
    f acc n = acc <> (selectedKey n)
  in
    foldl f mempty fingers

-- | work out a newly fingered Key from the mouse click coordinates
fingeredKey :: MouseCoordinates -> Maybe Int
fingeredKey coords =
  -- don't respond if we're outside the keyboard area
  if (coords.x < keyboardxOffset) ||
     (coords.x > keyboardxOffset + keyboardWidth) ||
     (coords.y < keyboardyOffset - cellSize / 2.0) ||
     (coords.y > keyboardyOffset +  whiteKeyLength ) then
       Nothing
  else
    if (coords.y < keyboardyOffset + blackKeyLength) then
      Just $ floor $ (coords.x - keyboardxOffset) / blackKeyWidth
    else
      let
        keyPos = floor $ (coords.x - keyboardxOffset) / whiteKeyWidth
      in
        index whiteKeyPositions keyPos

-- | display the entire chord shape described by the fingering
displayChord :: Fingering -> DiagramParameters -> Drawing
displayChord chord params =
  title params.name <>
    whiteKeys <>
    blackKeys <>
    selectedKeys chord
