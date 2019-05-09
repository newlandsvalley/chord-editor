module Bass.Page where

import Bass.Types
import Prelude

import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array (filter, head, index, null, updateAt, snoc)
import Data.Int (toNumber, fromString)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (Context2D, CanvasElement, clearRect, getCanvasElementById, getContext2D)
import Graphics.Drawing (render) as Drawing
import Common.Export (exportAs, scaleCanvas, toMimeType)
import Common.Types (CanvasPosition, ExportFormat(..), Percentage)
import Bass.Graphics (canvasHeight, canvasWidth, displayChord, fingeredString, titleDepth)
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

type State =
  {
    mGraphicsContext :: Maybe Context2D
  , mCanvas :: Maybe CanvasElement
  , canvasPosition :: CanvasPosition
  , fingering :: Fingering
  , diagramParameters :: DiagramParameters
  , exportScale :: Percentage
  }

data Action =
    Init
  | EditFingering Int Int
  | ClearFingering
  | GetChordName String
  | GetFirstFretNumber String
  | GetImageScale Percentage
  | Export ExportFormat

data Query a =
    GetCanvasOffset a
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

  closedStringParameters :: DiagramParameters
  closedStringParameters =
      { name : closedStringsChordName
      , firstFretOffset : 0
      }

  initialState :: i -> State
  initialState _ =
    { -- mAudioContext : Nothing
      mGraphicsContext : Nothing
    , mCanvas : Nothing
    , canvasPosition : { left : 0.0, top : 0.0 }
    , fingering : closedStrings
    , diagramParameters : closedStringParameters
    , exportScale : 100
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Bass Pattern Editor" ]
      , HH.canvas
         [ HP.id_ "canvas"
         , HE.onClick canvasClickHandler
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
      _ <- H.modify (\st -> st { fingering = closedStrings
                                , diagramParameters = closedStringParameters })
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
        fileName = state.diagramParameters.name <> "_bass"
      canvas <- H.liftEffect $ scaleCanvas originalCanvas scaleFactor
      _ <- H.liftEffect $ exportAs canvas fileName mimeType
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
      currentStringPositions = unsafePartial $ fromJust $
                        index fingering (fingeredString.stringNumber)
      newStringPositions =
        -- silent string
        if (null currentStringPositions) then
          [{ fret: fingeredString.fretNumber
           , status: Primary
           }]
        else
          let
            maybeFingering =
              extract currentStringPositions fingeredString.fretNumber
          in
            case maybeFingering of
              Just afingering ->
                -- we've selected an existing open string to remove it
                if (isOpenFret afingering) then
                  remove currentStringPositions fingeredString.fretNumber
                else
                  case afingering.status of
                    -- we've selected an existing primary fret so
                    -- cycle the status to Secondary
                    Primary ->
                      setSecondaryStatus currentStringPositions fingeredString.fretNumber
                    -- we've selected an existing secondary fret to remove it
                    Secondary ->
                      remove currentStringPositions fingeredString.fretNumber
              _ ->
                -- add a completely new fretted position
                snoc currentStringPositions
                   { fret: fingeredString.fretNumber
                   , status : Primary
                   }

      mNewFingering =
         updateAt fingeredString.stringNumber newStringPositions fingering
    in
      fromMaybe fingering mNewFingering


-- | extract, if we can, any finger position on the string in question at the
-- | current fret
extract :: StringPositions -> FretNumber -> Maybe FingerPosition
extract stringPositions fretNumber =
  head $ filter (\{fret, status} -> fret == fretNumber) stringPositions

-- | remove, if we can, any finger position on the string in question at the
-- | current fret
remove :: StringPositions -> FretNumber -> StringPositions
remove stringPositions fretNumber =
  filter (\{fret, status} -> fret /= fretNumber) stringPositions

-- | set the status of the fingering at the required fret to 'Secondary'
setSecondaryStatus :: StringPositions -> FretNumber -> StringPositions
setSecondaryStatus stringPositions fretNumber =
  snoc (remove stringPositions fretNumber)
     { fret: fretNumber
     , status : Secondary
     }
