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
import Graphics.Canvas (Context2D, CanvasElement,
         clearRect, getCanvasElementById, getContext2D)
import Graphics.Drawing (render) as Drawing
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Array (cons, filter, length)
import Partial.Unsafe (unsafePartial)
import Data.Int (toNumber, fromString)
import Piano.Graphics (canvasHeight, canvasWidth, displayChord, fingeredKey)
import Piano.Types (DiagramParameters, Fingering, unfingered)
import Piano.Audio (playChord)
import Common.Types (ExportFormat(..), CanvasPosition, Percentage)
import Common.Export (exportAs, scaleCanvas, toMimeType)
import Common.Utils (contains)
import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Data.Midi.Instrument (InstrumentName(..))


type Slot = H.Slot Query Void

-- import Debug.Trace (spy)

type State =
  { -- mAudioContext :: Maybe AudioContext
    mGraphicsContext :: Maybe Context2D
  , mCanvas :: Maybe CanvasElement
  , canvasPosition :: CanvasPosition
  , fingering :: Fingering
  , diagramParameters :: DiagramParameters
  , exportScale :: Percentage
  , instruments :: Array Instrument
  }

data Action =
    Init
  | EditFingering Int Int
  | ClearFingering
  | GetChordName String
  | GetImageScale Percentage
  | Export ExportFormat
  | PlayChord

data Query a =
    GetCanvasOffset a
  | LoadInstruments a
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

  initialParameters :: DiagramParameters
  initialParameters =
      { name : "silent"
      }

  initialState :: i -> State
  initialState _ =
    { -- mAudioContext : Nothing
      mGraphicsContext : Nothing
    , mCanvas : Nothing
    , canvasPosition : { left : 0.0, top : 0.0 }
    , fingering : unfingered
    , diagramParameters : initialParameters
    , exportScale : 100
    , instruments : []
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Piano Chord Editor" ]
      , HH.canvas
         [ HP.id_ "canvas"
         , HE.onClick canvasClickHandler
         , HP.height canvasHeight
         , HP.width  canvasWidth
         ]
      , renderChordNameInput state
      , HH.div_
        [ renderImageScaleSlider state
        , HH.text (show $ toNumber state.exportScale / 100.0)
        ]
      , HH.div_
        [ renderClearFingeringButton state
        , renderExportPNGButton state
        ]
      , renderPlayButton state
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

  renderPlayButton :: State -> H.ComponentHTML Action () m
  renderPlayButton state =
    let
      enabled =
        (length state.instruments > 0) &&
        (length state.fingering > 0)
      className =
        if enabled then "hoverable" else "unhoverable"
    in
      HH.div_
        [ HH.button
          [ HE.onClick \_ -> Just PlayChord
          , HP.class_ $ ClassName className
          , HP.enabled enabled
          ]
          [ HH.text "play" ]
        ]

  handleAction ∷ Action → H.HalogenM State Action () o m Unit
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
                               , mCanvas = mCanvas
                               })
      _ <- handleQuery (GetCanvasOffset unit)
      _ <- handleQuery (DisplayFingering unit)
      _ <- handleQuery (LoadInstruments unit)
      pure unit
    EditFingering cx cy -> do
      state <- H.get
      let
        x = toNumber cx - state.canvasPosition.left
        y = toNumber cy - state.canvasPosition.top
        mKey = fingeredKey {x,y}
      if (isJust mKey)
        then do
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
            newFingering = alterFingering key state.fingering
          _ <- H.modify (\st -> st { fingering = newFingering })
          _ <- handleQuery (DisplayFingering unit)
          pure unit
        else do
          pure unit
    GetChordName name -> do
      state <- H.get
      let
        newParams = state.diagramParameters { name = name }
        newState = state { diagramParameters = newParams }
      _ <- H.put newState
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    ClearFingering -> do
      _ <- H.modify (\st -> st { fingering = unfingered
                                , diagramParameters = initialParameters })
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
        fileName = state.diagramParameters.name <> "_piano"
      canvas <- H.liftEffect $ scaleCanvas originalCanvas scaleFactor
      _ <- H.liftEffect $ exportAs canvas fileName mimeType
      pure unit
    PlayChord -> do
      state <- H.get
      H.liftEffect $ playChord state.fingering state.instruments

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action () o m (Maybe a)
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
      instruments <- H.liftAff $  loadRemoteSoundFonts  [AcousticGrandPiano]
      _ <- H.modify (\st -> st { instruments = instruments })
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

  canvasClickHandler :: MouseEvent -> Maybe Action
  canvasClickHandler me =
    Just $ EditFingering (clientX me) (clientY me)

  clearCanvas :: State -> Effect Unit
  clearCanvas state = do
    let
      graphicsContext = unsafePartial (fromJust state.mGraphicsContext)
    clearRect graphicsContext { x: 0.0
                              , y: 0.0
                              , width : toNumber canvasWidth
                              , height : toNumber canvasHeight
                              }

  alterFingering :: Int -> Fingering -> Fingering
  alterFingering fingeredKey fingering =
    if contains fingering fingeredKey then
      filter (\x -> x /= fingeredKey) fingering
    else
      cons fingeredKey fingering

{-}
  contains :: ∀ a. Eq a => Array a -> a -> Boolean
  contains xs x =
    (length $ filter (\y -> y == x) xs) > 0
-}
