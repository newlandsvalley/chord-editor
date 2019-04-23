module Container where

import Prelude
import Effect.Aff (Aff)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)
import Web.HTML.HTMLElement (offsetTop, offsetLeft)
import Web.DOM.ParentNode (QuerySelector(..))
import Graphics.Canvas (Context2D, CanvasElement,
         clearRect, getCanvasElementById, getContext2D)
import Graphics.Drawing (render) as Drawing
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Array (index, updateAt)
import Partial.Unsafe (unsafePartial)
import Data.Int (toNumber, fromString)
import Graphics (canvasHeight, canvasWidth, displayChord, fingeredString, titleDepth)
import Export (exportAs)
import Types

-- import Debug.Trace (spy)

type CanvasPosition =
  { left :: Number
  , top  :: Number
  }

type State =
  { -- mAudioContext :: Maybe AudioContext
    mGraphicsContext :: Maybe Context2D
  , mCanvas :: Maybe CanvasElement
  , canvasPosition :: CanvasPosition
  , fingering :: Fingering
  , diagramParameters :: DiagramParameters
  }

data Action =
    Init
  | EditFingering Int Int
  | ClearFingering
  | GetChordName String
  | GetFirstFretNumber String
  | Export ExportFormat

data Query a =
    GetCanvasOffset a
  | DisplayFingering a

component :: ∀ i o. H.Component HH.HTML Query i o Aff
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
      { name : "Em7+11"
      , firstFretOffset : 0
      }

  initialState :: i -> State
  initialState _ =
    { -- mAudioContext : Nothing
      mGraphicsContext : Nothing
    , mCanvas : Nothing
    , canvasPosition : { left : 0.0, top : 0.0 }
    , fingering : openStrings
    , diagramParameters : openStringParameters
    }

  render :: State -> H.ComponentHTML Action () Aff
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Guitar Chord Editor" ]
      , HH.canvas
         [ HP.id_ "canvas"
         , HE.onClick canvasClickHandler
         , HP.height canvasHeight
         , HP.width  canvasWidth
         ]
      , renderChordNameInput state
      , renderFirstFretNoInput state
      , HH.div_
        [ renderClearFingeringButton state
        , renderExportPNGButton state
        ]
      ]

  renderClearFingeringButton :: State -> H.ComponentHTML Action () Aff
  renderClearFingeringButton state =
    HH.button
      [ HE.onClick \_ -> Just ClearFingering
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "clear fingering" ]

  renderExportPNGButton :: State -> H.ComponentHTML Action () Aff
  renderExportPNGButton state =
    HH.button
      [ HE.onClick \_ -> Just (Export PNG)
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]
      [ HH.text "export as PNG" ]

  renderChordNameInput :: State -> H.ComponentHTML Action () Aff
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

  renderFirstFretNoInput :: State -> H.ComponentHTML Action () Aff
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

  handleAction ∷ Action → H.HalogenM State Action () o Aff Unit
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
    EditFingering cx cy -> do
      state <- H.get
      let
        x = toNumber cx - state.canvasPosition.left
        y = toNumber cy - state.canvasPosition.top
      if (y > titleDepth)
        then do
          let
            {-}
            foo = spy "X:" x
            bar = spy "Y:" y
            -}
            fstring = fingeredString {x,y}
            {-}
            foo = spy "string:" fstring.stringNumber
            bar = spy "fret:" fstring.fretNumber
            -}
            newFingering = alterFingering fstring state.fingering
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
    Export format -> do
      state <- H.get
      let
        canvas = unsafePartial (fromJust state.mCanvas)
        mimeType = toMimeType format
      _ <- H.liftEffect $ exportAs canvas state.diagramParameters.name mimeType
      pure unit

  handleQuery :: ∀ o a. Query a -> H.HalogenM State Action () o Aff (Maybe a)
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

  alterFingering :: FingeredString -> Fingering -> Fingering
  alterFingering fingeredString fingering =
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
          -- if we're at a real fret, make open if it's occupied already
          -- else populate the new fret
          if (fingeredString.fretNumber == currentFret) then
            open
          else
            fingeredString.fretNumber

      mNewFingering =
        updateAt fingeredString.stringNumber newFret fingering
    in
      fromMaybe fingering mNewFingering
