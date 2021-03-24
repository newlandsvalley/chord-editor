module Guitar.Page where

import Guitar.Types
import Prelude

import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array (index, length, mapWithIndex, updateAt)
import Data.Int (toNumber, fromString)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Tuple (Tuple(..))
import Data.Foldable (all, foldl)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (Context2D, CanvasElement, clearRect, getCanvasElementById, getContext2D)
import Graphics.Drawing (render) as Drawing
import Common.Export (exportAs, scaleCanvas, toMimeType)
import Common.Utils (safeName, jsonFileInputCtx)
import Common.Types (ExportFormat(..), CanvasPosition, Percentage)
import Guitar.Graphics (canvasHeight, canvasWidth, displayChord, fingeredString,
          titleDepth)
import Guitar.Audio (playChord)
import Guitar.Validation (validateJson)
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
import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Data.Midi.Instrument (InstrumentName(AcousticGuitarSteel))
import Serialization.Json (writeGuitar)
import JS.FileIO (saveTextFile)
import Halogen.FileInputComponent as FIC
import Data.Validation.Semigroup (validation)
import Type.Proxy (Proxy(..))


type Slot = H.Slot Query Void

-- import Debug.Trace (spy)

type State =
  {
    mGraphicsContext :: Maybe Context2D
  , mCanvas :: Maybe CanvasElement
  , canvasPosition :: CanvasPosition
  , mouseDownFinger :: Maybe FingeredString
  , mouseUpFinger   :: Maybe FingeredString
  , chordShape :: ChordShape
  , exportScale :: Percentage
  , instruments :: Array Instrument
  , errorText :: String
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
  | Load FIC.Message
  | Save
  | PlayChord

data Query a =
    GetCanvasOffset a
  | LoadInstruments a
  | EditFingering a
  | DisplayFingering a

type ChildSlots =
  ( loadfile :: FIC.Slot Unit )

-- _loadfile = SProxy :: SProxy "loadfile"
_loadfile = Proxy :: Proxy "loadfile"

component :: ∀ i o m. MonadAff m => H.Component Query i o m
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

  openStringChordShape :: ChordShape
  openStringChordShape =
      { name : openStringsChordName
      , firstFretOffset : 0
      , barre : Nothing
      , fingering : openStrings
      }

  initialState :: i -> State
  initialState _ =
    {
      mGraphicsContext : Nothing
    , mCanvas : Nothing
    , canvasPosition : { left : 0.0, top : 0.0 }
    , mouseDownFinger : Nothing
    , mouseUpFinger : Nothing
    , chordShape : openStringChordShape
    , exportScale : 100
    , instruments : []
    , errorText : ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Guitar Chord Editor" ]
      , HH.canvas
         [ HP.id "canvas"
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
      , HH.div_
        [ renderLoadButton state
        , renderSaveButton state
        ]
      , renderPlayButton state
      , HH.text state.errorText
      ]

  renderClearFingeringButton :: State -> H.ComponentHTML Action ChildSlots m
  renderClearFingeringButton state =
    HH.button
      [ HE.onClick \_ -> ClearFingering
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "clear fingering" ]


  renderLoadButton :: State -> H.ComponentHTML Action ChildSlots m
  renderLoadButton state =
    HH.slot _loadfile unit (FIC.component jsonFileInputCtx) unit Load

  renderSaveButton :: State -> H.ComponentHTML Action ChildSlots m
  renderSaveButton state =
    HH.button
      [ HE.onClick \_ -> Save
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "save" ]

  renderExportPNGButton :: State -> H.ComponentHTML Action ChildSlots m
  renderExportPNGButton state =
    HH.button
      [ HE.onClick \_ -> Export PNG
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "download PNG" ]

  renderChordNameInput :: State -> H.ComponentHTML Action ChildSlots m
  renderChordNameInput state =
    HH.div
      [ HP.id "chord-name-div" ]
      [ HH.label
        [ HP.id "chord-name-label" ]
        [ HH.text "chord name:" ]
      , HH.input
          [ HE.onValueInput GetChordName
          , HP.value state.chordShape.name
          , HP.type_ HP.InputText
          , HP.id  "chord-name-edit"
          , HP.class_ $ ClassName "text-input"
          ]
      ]

  renderFirstFretNoInput :: State -> H.ComponentHTML Action ChildSlots m
  renderFirstFretNoInput state =
    HH.div
      [ HP.id "fret-number-div" ]
      [ HH.label
        [ HP.id "fret-number-label" ]
        [ HH.text "first fret number:" ]
      , HH.input
          [ HE.onValueInput GetFirstFretNumber
          , HP.value (show state.chordShape.firstFretOffset)
          , HP.type_ HP.InputNumber
          , HP.min 0.0
          , HP.max 9.0
          , HP.id  "fret-number-edit"
          , HP.class_ $ ClassName "text-input"
          ]
      ]

  renderImageScaleSlider :: State -> H.ComponentHTML Action ChildSlots m
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
            [ HE.onValueInput  (GetImageScale <<< toScale )
            , HP.type_ HP.InputRange
            , HP.id "scale-slider"
            , HP.class_ (H.ClassName "scaling-slider")
            , HP.min 25.0
            , HP.max 1000.0
            , HP.step (Step 25.0)
            , HP.value (show state.exportScale)
            ]
        ]

  renderPlayButton :: State -> H.ComponentHTML Action ChildSlots m
  renderPlayButton state =
    let
      enabled =
        (length state.instruments > 0) &&
        (not $ silentChord state.chordShape.fingering state.chordShape.barre )
      className =
        if enabled then "hoverable" else "unhoverable"
    in
      HH.div_
        [ HH.button
            [ HE.onClick \_ -> PlayChord
            , HP.class_ $ ClassName className
            , HP.enabled enabled
            ]
            [ HH.text "play" ]
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

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Init -> do
      state <- H.get
      mCanvas <- H.liftEffect $ getCanvasElementById "canvas"
      let
        canvas = unsafePartial (fromJust mCanvas)
        -- audioCtx = unsafePartial (fromJust state.mAudioContext)
      graphicsCtx <- H.liftEffect  $ getContext2D canvas
      -- _ <- H.liftEffect $ Drawing.render graphicsCtx chordDisplay
      _ <- H.modify (\st -> st { mGraphicsContext = Just graphicsCtx
                               , mCanvas = mCanvas
                               })
      _ <- handleQuery (GetCanvasOffset unit)
      _ <- handleQuery (DisplayFingering unit)
      _ <- handleQuery (LoadInstruments unit)
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
          _ <- H.modify (\st -> st { mouseDownFinger = Just fstring, errorText = "" })
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
          _ <- H.modify (\st -> st { mouseUpFinger = Just fstring, errorText = "" })
          _ <- handleQuery (EditFingering unit)
          pure unit
        else do
          _ <- H.modify (\st -> st { mouseUpFinger = Nothing })
          pure unit
    GetChordName name -> do
      state <- H.get
      let
        newShape = state.chordShape { name = name }
        newState = state { chordShape = newShape, errorText = "" }
      _ <- H.put newState
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    GetFirstFretNumber numStr -> do
      state <- H.get
      let
        fret = fromMaybe 0 $ fromString numStr
        newShape = state.chordShape { firstFretOffset = fret }
        newState = state { chordShape = newShape, errorText = "" }
      _ <- H.put newState
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    ClearFingering -> do
      _ <- H.modify (\st -> st { chordShape = openStringChordShape, errorText = "" })
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
        fileName = (safeName state.chordShape.name) <> "_guitar"
      canvas <- H.liftEffect $ scaleCanvas originalCanvas scaleFactor
      _ <- H.liftEffect $ exportAs canvas fileName mimeType
      pure unit
    Load (FIC.FileLoaded filespec) -> do
      state <- H.get
      let
        validated = validateJson filespec.contents
        newState = validation
                    (\errs -> state { errorText = foldl (<>) "" errs})
                    (\chordShape -> state {chordShape = chordShape, errorText = ""} )
                    validated
      _ <- H.put newState
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    Save -> do
      state <- H.get
      let
        name = (safeName state.chordShape.name) <> "_guitar" <> ".json"
        contents = writeGuitar state.chordShape
      _ <- H.liftEffect $ saveTextFile { name, contents }
      pure unit
    PlayChord -> do
      state <- H.get
      H.liftEffect $ playChord
        state.chordShape.fingering
        state.chordShape.firstFretOffset
        state.chordShape.barre
        state.instruments

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
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
    LoadInstruments next -> do
      instruments <- H.liftAff $  loadRemoteSoundFonts  [AcousticGuitarSteel]
      _ <- H.modify (\st -> st { instruments = instruments })
      pure (Just next)
    EditFingering next -> do
      state <- H.get
      let
        action = mouseAction state.mouseDownFinger state.mouseUpFinger
      case action of
        OneFret fstring -> do
          let
            newFingering = alterFingering fstring state.chordShape.fingering state.chordShape.barre
            newChord = state.chordShape { fingering = newFingering }
          _ <- H.modify (\st -> st { chordShape = newChord })
          _ <- handleQuery (DisplayFingering unit)
          pure (Just next)
        Barre fstring -> do
          let
            -- remove any fingering hidden by the barre
            newFingering = removeHiddenFingering (Just fstring) state.chordShape.fingering
            -- set up the barre
            newShape = state.chordShape { barre = Just fstring, fingering = newFingering }
            newState = state { chordShape = newShape }
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
                  $ displayChord state.chordShape
      pure (Just next)

  -- recognize a mouse down click event on the canvas
  canvasMouseDownHandler :: MouseEvent -> Action
  canvasMouseDownHandler me =
    MouseDown (clientX me) (clientY me)

  -- recognize a mouse up click event on the canvas
  canvasMouseUpHandler :: MouseEvent -> Action
  canvasMouseUpHandler me =
    MouseUp (clientX me) (clientY me)

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
  alterFingering :: FingeredString -> Fingering -> Barre -> Fingering
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


  -- | work out the type of mouse action to discriminate between either
  -- | setting individual strings or else barrés (or nothing discernible)
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
  hiddenByBarre :: Barre -> FingeredString -> Boolean
  hiddenByBarre mBarre fs =
    case mBarre of
      Just barre ->
        barre.stringNumber <= fs.stringNumber  &&
        barre.fretNumber >= fs.fretNumber
      _ ->
        false

  -- | remove from the fingering any finger position that is hidden by the
  -- | (new) barré (i.e. fingered at an identical or lower fret)
  removeHiddenFingering :: Barre -> Fingering -> Fingering
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

  silentChord :: Fingering -> Barre -> Boolean
  silentChord fingering mBarre =
    isNothing mBarre &&
    all (\fret -> fret == silent) fingering
