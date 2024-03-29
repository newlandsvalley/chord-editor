module Piano.Page where

import Prelude
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen.Aff as HA
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)
import Web.HTML.HTMLElement (offsetTop, offsetLeft)
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Web.DOM.ParentNode (QuerySelector(..))
import Graphics.Canvas
  ( Context2D
  , CanvasElement
  , clearRect
  , getCanvasElementById
  , getContext2D
  )
import Graphics.Drawing (render) as Drawing
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Array (cons, filter, length)
import Data.Foldable (foldl)
import Partial.Unsafe (unsafePartial)
import Data.Int (toNumber, fromString)
import Piano.Graphics (canvasHeight, canvasWidth, displayChord, fingeredKey)
import Piano.Types (ChordShape, Fingering, unfingered)
import Piano.Audio (playChord)
import Piano.Validation (validateJson)
import Common.Types (ExportFormat(..), CanvasPosition, Percentage)
import Common.Export (exportAs, scaleCanvas, toMimeType)
import Common.Utils (contains, safeName, jsonFileInputCtx)
import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Data.Midi.Instrument (InstrumentName(AcousticGrandPiano))
import Serialization.Json (writePiano)
import JS.FileIO (saveTextFile)
import Halogen.FileInputComponent as FIC
import Data.Validation.Semigroup (validation)
import Type.Proxy (Proxy(..))

type Slot = H.Slot Query Void

-- import Debug.Trace (spy)

type State =
  { mGraphicsContext :: Maybe Context2D
  , mCanvas :: Maybe CanvasElement
  , canvasPosition :: CanvasPosition
  , chordShape :: ChordShape
  , exportScale :: Percentage
  , instruments :: Array Instrument
  , errorText :: String
  }

data Action
  = Init
  | EditFingering Int Int
  | ClearFingering
  | GetChordName String
  | GetImageScale Percentage
  | Export ExportFormat
  | Load FIC.Message
  | Save
  | PlayChord

data Query a
  = GetCanvasOffset a
  | LoadInstruments a
  | DisplayFingering a

type ChildSlots =
  (loadfile :: FIC.Slot Unit)

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

  initialChordShape :: ChordShape
  initialChordShape =
    { name: "silent"
    , fingering: unfingered
    }

  initialState :: i -> State
  initialState _ =
    { -- mAudioContext : Nothing
      mGraphicsContext: Nothing
    , mCanvas: Nothing
    , canvasPosition: { left: 0.0, top: 0.0 }
    , chordShape: initialChordShape
    , exportScale: 100
    , instruments: []
    , errorText: ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.h1
          [ HP.class_ (H.ClassName "center") ]
          [ HH.text "Piano Chord Editor" ]
      , HH.canvas
          [ HP.id "canvas"
          , HE.onClick canvasClickHandler
          , HP.height canvasHeight
          , HP.width canvasWidth
          ]
      , renderChordNameInput state
      , HH.div_
          [ renderImageScaleSlider state
          , HH.text (show $ toNumber state.exportScale / 100.0)
          ]
      , HH.div_
          [ renderClearFingeringButton
          , renderExportPNGButton
          ]
      , HH.div_
          [ renderLoadButton
          , renderSaveButton
          ]
      , renderPlayButton state
      , HH.text state.errorText
      ]

  renderClearFingeringButton :: H.ComponentHTML Action ChildSlots m
  renderClearFingeringButton =
    HH.button
      [ HE.onClick \_ -> ClearFingering
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "clear fingering" ]

  renderExportPNGButton :: H.ComponentHTML Action ChildSlots m
  renderExportPNGButton =
    HH.button
      [ HE.onClick \_ -> Export PNG
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "download PNG" ]

  renderLoadButton :: H.ComponentHTML Action ChildSlots m
  renderLoadButton =
    HH.slot _loadfile unit (FIC.component jsonFileInputCtx) unit Load

  renderSaveButton :: H.ComponentHTML Action ChildSlots m
  renderSaveButton =
    HH.button
      [ HE.onClick \_ -> Save
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "save" ]

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
          , HP.id "chord-name-edit"
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
        [ HP.class_ (H.ClassName "leftPanelComponent") ]
        [ HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "scale download:" ]
        , HH.input
            [ HE.onValueInput (GetImageScale <<< toScale)
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
          (length state.chordShape.fingering > 0)
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

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Init -> do
      -- state <- H.get
      mCanvas <- H.liftEffect $ getCanvasElementById "canvas"
      let
        canvas = unsafePartial (fromJust mCanvas)
      -- audioCtx = unsafePartial (fromJust state.mAudioContext)
      graphicsCtx <- H.liftEffect $ getContext2D canvas
      -- _ <- H.liftEffect $ Drawing.render graphicsCtx chordDisplay
      _ <- H.modify
        ( \st -> st
            { mGraphicsContext = Just graphicsCtx
            , mCanvas = mCanvas
            }
        )
      _ <- handleQuery (GetCanvasOffset unit)
      _ <- handleQuery (DisplayFingering unit)
      _ <- handleQuery (LoadInstruments unit)
      pure unit
    EditFingering cx cy -> do
      state <- H.get
      let
        x = toNumber cx - state.canvasPosition.left
        y = toNumber cy - state.canvasPosition.top
        mKey = fingeredKey { x, y }
      if (isJust mKey) then do
        let
          {-}
          foo = spy "X:" x
          bar = spy "Y:" y
          -}
          key = unsafePartial (fromJust mKey)
          {-}
          foo = spy "string:" fstring.stringNumber
          bar = spy "fret:" fstring.fretNumber
          -}
          newFingering = alterFingering key state.chordShape.fingering
          newChordShape = state.chordShape { fingering = newFingering }
        _ <- H.modify (\st -> st { chordShape = newChordShape, errorText = "" })
        _ <- handleQuery (DisplayFingering unit)
        pure unit
      else do
        pure unit
    GetChordName name -> do
      state <- H.get
      let
        newShape = state.chordShape { name = name }
        newState = state { chordShape = newShape, errorText = "" }
      _ <- H.put newState
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    ClearFingering -> do
      _ <- H.modify (\st -> st { chordShape = initialChordShape, errorText = "" })
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
        fileName = (safeName state.chordShape.name) <> "_piano"
      canvas <- H.liftEffect $ scaleCanvas originalCanvas scaleFactor
      _ <- H.liftEffect $ exportAs canvas fileName mimeType
      pure unit
    Load (FIC.FileLoaded filespec) -> do
      state <- H.get
      let
        validated = validateJson filespec.contents
        newState = validation
          (\errs -> state { errorText = foldl (<>) "" errs })
          (\chordShape -> state { chordShape = chordShape, errorText = "" })
          validated
      _ <- H.put newState
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    Save -> do
      state <- H.get
      let
        name = (safeName state.chordShape.name) <> "_piano" <> ".json"
        contents = writePiano state.chordShape
      _ <- H.liftEffect $ saveTextFile { name, contents }
      pure unit
    PlayChord -> do
      state <- H.get
      H.liftEffect $ playChord state.chordShape.fingering state.instruments

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
      _ <- H.modify (\st -> st { canvasPosition = { left, top } })
      pure (Just next)
    LoadInstruments next -> do
      instruments <- H.liftAff $ loadRemoteSoundFonts [ AcousticGrandPiano ]
      _ <- H.modify (\st -> st { instruments = instruments })
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

  canvasClickHandler :: MouseEvent -> Action
  canvasClickHandler me =
    EditFingering (clientX me) (clientY me)

  clearCanvas :: State -> Effect Unit
  clearCanvas state = do
    let
      graphicsContext = unsafePartial (fromJust state.mGraphicsContext)
    clearRect graphicsContext
      { x: 0.0
      , y: 0.0
      , width: toNumber canvasWidth
      , height: toNumber canvasHeight
      }

  alterFingering :: Int -> Fingering -> Fingering
  alterFingering fingeredKey fingering =
    if contains fingering fingeredKey then
      filter (\x -> x /= fingeredKey) fingering
    else
      cons fingeredKey fingering
