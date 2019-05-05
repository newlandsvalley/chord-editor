module Guitar.Page where

import Guitar.Types
import Prelude

import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array (index, mapWithIndex, updateAt)
import Data.Int (toNumber, fromString)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (Context2D, CanvasElement, clearRect, getCanvasElementById, getContext2D)
import Graphics.Drawing (render) as Drawing
import Guitar.Export (exportAs, scaleCanvas)
import Guitar.Graphics (canvasHeight, canvasWidth, displayChord, fingeredString,
          titleDepth)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (offsetTop, offsetLeft)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)

type Slot = H.Slot Query Void

-- import Debug.Trace (spy)
type Percentage = Int

type CanvasPosition =
  { left :: Number
  , top  :: Number
  }

type State =
  { -- mAudioContext :: Maybe AudioContext
    mGraphicsContext :: Maybe Context2D
  , mCanvas :: Maybe CanvasElement
  , canvasPosition :: CanvasPosition
  , mouseDownFinger :: Maybe FingeredString
  , mouseUpFinger   :: Maybe FingeredString
  , fingering :: Fingering
  , diagramParameters :: DiagramParameters
  , exportScale :: Percentage
  }

data Action =
    Init
  | MouseDown Int Int
  | MouseUp Int Int
  | ClearFingering
  | GetChordName String
  | GetFirstFretNumber String
  | GetImageScale Percentage
  | Export ExportFormat

data Query a =
    GetCanvasOffset a
  | EditFingering a
  | DisplayFingering a

component :: ∀ i o m. MonadAff m => H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , finalize = Nothing
        }
    }
  where

  openStringParameters :: DiagramParameters
  openStringParameters =
      { name : openStringsChordName
      , firstFretOffset : 0
      , barre : Nothing
      }

  initialState :: i -> State
  initialState _ =
    { -- mAudioContext : Nothing
      mGraphicsContext : Nothing
    , mCanvas : Nothing
    , canvasPosition : { left : 0.0, top : 0.0 }
    , mouseDownFinger : Nothing
    , mouseUpFinger : Nothing
    , fingering : openStrings
    , diagramParameters : openStringParameters
    , exportScale : 100
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Guitar Chord Editor" ]
      , HH.canvas
         [ HP.id_ "canvas"
         -- , HE.onClick canvasClickHandler
         , HE.onMouseDown canvasMouseDownHandler
         , HE.onMouseUp canvasMouseUpHandler
         , HP.height canvasHeight
         , HP.width  canvasWidth
         ]
      , renderChordNameInput state
      , renderFirstFretNoInput state
      , HH.div_
        [ renderImageScaleSlider state
        , HH.text (show $ toNumber state.exportScale / 100.0)
        ]
      , HH.div_
        [ renderClearFingeringButton state
        , renderExportPNGButton state
        ]
      ]

  renderClearFingeringButton :: State -> H.ComponentHTML Action () m
  renderClearFingeringButton state =
    HH.button
      [ HE.onClick \_ -> Just ClearFingering
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "clear fingering" ]

  renderExportPNGButton :: State -> H.ComponentHTML Action () m
  renderExportPNGButton state =
    HH.button
      [ HE.onClick \_ -> Just (Export PNG)
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "download PNG" ]

  renderChordNameInput :: State -> H.ComponentHTML Action () m
  renderChordNameInput state =
    HH.div
      [ HP.id_ "chord-name-div" ]
      [ HH.label
        [ HP.id_ "chord-name-label" ]
        [ HH.text "chord name:" ]
      , HH.input
          [ HE.onValueInput  (Just <<< GetChordName)
          , HP.value state.diagramParameters.name
          , HP.type_ HP.InputText
          , HP.id_  "chord-name-edit"
          , HP.class_ $ ClassName "text-input"
          ]
      ]

  renderFirstFretNoInput :: State -> H.ComponentHTML Action () m
  renderFirstFretNoInput state =
    HH.div
      [ HP.id_ "fret-number-div" ]
      [ HH.label
        [ HP.id_ "fret-number-label" ]
        [ HH.text "first fret number:" ]
      , HH.input
          [ HE.onValueInput  (Just <<< GetFirstFretNumber)
          , HP.value (show state.diagramParameters.firstFretOffset)
          , HP.type_ HP.InputNumber
          , HP.min 0.0
          , HP.max 9.0
          , HP.id_  "fret-number-edit"
          , HP.class_ $ ClassName "text-input"
          ]
      ]

  renderImageScaleSlider :: State -> H.ComponentHTML Action () m
  renderImageScaleSlider state =
    let
      toScale :: String -> Percentage
      toScale s =
        fromMaybe 100 $ fromString s
    in
      HH.div
        [ HP.class_ (H.ClassName "leftPanelComponent")]
        [ HH.label
           [ HP.class_ (H.ClassName "labelAlignment") ]
           [ HH.text "scale download:" ]
        , HH.input
            [ HE.onValueInput  (Just <<< GetImageScale <<< toScale )
            , HP.type_ HP.InputRange
            , HP.id_ "scale-slider"
            , HP.class_ (H.ClassName "scaling-slider")
            , HP.min 25.0
            , HP.max 1000.0
            , HP.step (Step 25.0)
            , HP.value (show state.exportScale)
            ]
        ]

  {-
  renderDebug :: State -> H.ComponentHTML Action () Aff
  renderDebug state =
    let
      mouseDown =
        maybe "nothing" showFinger state.mouseDownFinger
      mouseUp =
        maybe "nothing" showFinger state.mouseUpFinger
    in
      HH.text ("mouse down: " <> mouseDown <> " mouse up: " <> mouseUp)
  -}

  showFinger :: FingeredString -> String
  showFinger fs =
    (show fs.stringNumber <>
     "-" <>
     show fs.fretNumber)

  handleAction ∷ Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Init -> do
      -- audioCtx <- H.liftEffect newAudioContext
      state <- H.get
      mCanvas <- H.liftEffect $ getCanvasElementById "canvas"
      let
        canvas = unsafePartial (fromJust mCanvas)
        -- audioCtx = unsafePartial (fromJust state.mAudioContext)
      graphicsCtx <- H.liftEffect  $ getContext2D canvas
      -- _ <- H.liftEffect $ Drawing.render graphicsCtx chordDisplay
      _ <- H.modify (\st -> st { mGraphicsContext = Just graphicsCtx
                               , mCanvas = mCanvas })
      _ <- handleQuery (GetCanvasOffset unit)
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    MouseDown cx cy -> do
      state <- H.get
      let
        x = toNumber cx - state.canvasPosition.left
        y = toNumber cy - state.canvasPosition.top
      if (y > titleDepth)
        then do
          let
            fstring = fingeredString {x,y}
          _ <- H.modify (\st -> st { mouseDownFinger = Just fstring })
          pure unit
        else do
          _ <- H.modify (\st -> st { mouseDownFinger = Nothing })
          pure unit
    MouseUp cx cy -> do
      state <- H.get
      let
        x = toNumber cx - state.canvasPosition.left
        y = toNumber cy - state.canvasPosition.top
      if (y > titleDepth)
        then do
          let
            fstring = fingeredString {x,y}
          _ <- H.modify (\st -> st { mouseUpFinger = Just fstring })
          _ <- handleQuery (EditFingering unit)
          pure unit
        else do
          _ <- H.modify (\st -> st { mouseUpFinger = Nothing })
          pure unit
    GetChordName name -> do
      state <- H.get
      let
        newParams = state.diagramParameters { name = name }
        newState = state { diagramParameters = newParams }
      _ <- H.put newState
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    GetFirstFretNumber numStr -> do
      state <- H.get
      let
        fret = fromMaybe 0 $ fromString numStr
        newParams = state.diagramParameters { firstFretOffset = fret }
        newState = state { diagramParameters = newParams }
      _ <- H.put newState
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    ClearFingering -> do
      _ <- H.modify (\st -> st { fingering = openStrings
                                , diagramParameters = openStringParameters })
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    GetImageScale scale -> do
      _ <- H.modify (\st -> st { exportScale = scale })
      pure unit
    Export format -> do
      state <- H.get
      let
        originalCanvas = unsafePartial (fromJust state.mCanvas)
        mimeType = toMimeType format
        scaleFactor = toNumber state.exportScale / 100.0
      canvas <- H.liftEffect $ scaleCanvas originalCanvas scaleFactor
      _ <- H.liftEffect $ exportAs canvas state.diagramParameters.name mimeType
      pure unit

  handleQuery :: ∀ o a. Query a -> H.HalogenM State Action () o m (Maybe a)
  handleQuery = case _ of
    -- get the coordinates of the upper left hand corner of the canvas we've
    -- just built.  We need this to find accurate mouse click references relative
    -- to the canvas itsef (not the entire screen). I think it is OK to use DOM
    -- here because we only call this once immediately after initialising the canvas
    GetCanvasOffset next -> do
      mCanvasElement <- H.liftAff $ HA.selectElement (QuerySelector "#canvas")
      let
        canvasElement = unsafePartial (fromJust mCanvasElement)
      left <- H.liftEffect $ offsetLeft canvasElement
      top <- H.liftEffect $ offsetTop canvasElement
      {-}
      let
        foo = spy "Left:" left
        bar = spy "Top:" top
      -}
      _ <- H.modify (\st -> st { canvasPosition  = { left, top } })
      pure (Just next)
    EditFingering next -> do
      state <- H.get
      let
        action = mouseAction state.mouseDownFinger state.mouseUpFinger
      case action of
        OneFret fstring -> do
          let
            newFingering = alterFingering fstring state.fingering state.diagramParameters.barre
          _ <- H.modify (\st -> st { fingering = newFingering })
          _ <- handleQuery (DisplayFingering unit)
          pure (Just next)
        Barre fstring -> do
          let
            -- set up the barre
            newParams = state.diagramParameters { barre = Just fstring }
            -- remove any fingering hidden by the barre
            newFingering = removeHiddenFingering (Just fstring) state.fingering
            newState = state { fingering = newFingering
                             , diagramParameters = newParams }
          _ <- H.put newState
          _ <- handleQuery (DisplayFingering unit)
          pure (Just next)
        _ -> do
          pure (Just next)
    DisplayFingering next -> do
      state <- H.get
      let
        graphicsCtx = unsafePartial (fromJust state.mGraphicsContext)

      _ <- H.liftEffect do
        clearCanvas state
        Drawing.render graphicsCtx
                  $ displayChord state.fingering state.diagramParameters
      pure (Just next)

  -- recognize a mouse down click event on the canvas
  canvasMouseDownHandler :: MouseEvent -> Maybe Action
  canvasMouseDownHandler me =
    Just $ MouseDown (clientX me) (clientY me)

  -- recognize a mouse up click event on the canvas
  canvasMouseUpHandler :: MouseEvent -> Maybe Action
  canvasMouseUpHandler me =
    Just $ MouseUp (clientX me) (clientY me)

  clearCanvas :: State -> Effect Unit
  clearCanvas state = do
    let
      graphicsContext = unsafePartial (fromJust state.mGraphicsContext)
    clearRect graphicsContext { x: 0.0
                              , y: 0.0
                              , width : toNumber canvasWidth
                              , height : toNumber canvasHeight
                              }

  -- | alter the fingering as a response to the last MouseUp event
  alterFingering :: FingeredString -> Fingering -> Maybe FingeredString -> Fingering
  alterFingering fingeredString fingering mBarre =
    let
      currentFret = unsafePartial $ fromJust $
                      index fingering (fingeredString.stringNumber)
      newFret =
        -- if we're unfretted (fret 0) then toggle between open and silent
        if (fingeredString.fretNumber == 0) then
          if (currentFret == open) then
            silent
          else
            open
        else
          -- if we're hidden by a barre, don't change the fingering
          if (hiddenByBarre mBarre fingeredString) then
            currentFret
          -- if we're at a real fret, and it's occupied already
          -- then remove it and set to open
          else if (fingeredString.fretNumber == currentFret) then
            open
          -- else populate the new fret
          else
            fingeredString.fretNumber

      mNewFingering =
        updateAt fingeredString.stringNumber newFret fingering
    in
      fromMaybe fingering mNewFingering


  -- | work out the type of mouse action to discriminate between setting
  -- | individual either strings or else barrés (or nothing discernible)
  mouseAction :: Maybe FingeredString -> Maybe FingeredString -> MouseAction
  mouseAction mDownFinger mUpFinger =
    case Tuple mDownFinger mUpFinger of
      Tuple (Just downFinger) (Just upFinger) ->
        -- if up and down are at the same fret and finger, it's individual
        if (downFinger.fretNumber == upFinger.fretNumber) &&
            (downFinger.stringNumber == upFinger.stringNumber) then
              OneFret downFinger
        -- if up and down are at the same fret but finger position increases
        -- (i.e. currently a left-to-right sweep) then a barré is indicated
        -- but this is only possible at actual frets (fretNumber > 0)
        else if (downFinger.fretNumber == upFinger.fretNumber) &&
            (downFinger.stringNumber < upFinger.stringNumber) &&
            (downFinger.fretNumber > 0) then
              Barre downFinger
        else
          NoFret
      _ ->
        NoFret

  -- | return true if the string at this fingering is hidden by the barré
  hiddenByBarre :: Maybe FingeredString -> FingeredString -> Boolean
  hiddenByBarre mBarre fs =
    case mBarre of
      Just barre ->
        barre.stringNumber <= fs.stringNumber  &&
        barre.fretNumber >= fs.fretNumber
      _ ->
        false

  -- | remove from the fingering any finger position that is hidden by the
  -- | (new) barré (i.e. fingered at an identical or lower fret)
  removeHiddenFingering :: Maybe FingeredString -> Fingering -> Fingering
  removeHiddenFingering mBarre fingering =
    case mBarre of
      Just barre ->
        let
          f :: Int -> FingerPosition -> FingerPosition
          f stringNumber fretNumber =
            if (hiddenByBarre mBarre
                 {stringNumber: stringNumber, fretNumber: fretNumber}) then
              open
            else
              fretNumber
        in
          mapWithIndex f fingering
      _ ->
        fingering
