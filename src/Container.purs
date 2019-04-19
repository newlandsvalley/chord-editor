module Container where

import Prelude
import Effect.Aff (Aff)
import Halogen.Aff as HA
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)
import Web.HTML.HTMLElement (offsetTop, offsetLeft)
import Web.DOM.ParentNode (QuerySelector(..))
import Graphics.Canvas (Context2D, CanvasElement, getCanvasElementById, getContext2D)
import Graphics.Drawing (render) as Drawing
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Int (toNumber)
import Graphics (canvasHeight, canvasWidth, chordDisplay)
import Export (exportAs)
import Types (ExportFormat(..), toMimeType)

import Debug.Trace (spy)

type CanvasPosition =
  { left :: Number
  , top  :: Number
  }

type State =
  { -- mAudioContext :: Maybe AudioContext
    mGraphicsContext :: Maybe Context2D
  , mCanvas :: Maybe CanvasElement
  , canvasPosition :: CanvasPosition
  }

data Action =
    Init
  | EditFingering Int Int
  | Export ExportFormat

data Query a =
  GetCanvasOffset a

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

  initialState :: i -> State
  initialState _ =
    { -- mAudioContext : Nothing
      mGraphicsContext : Nothing
    , mCanvas : Nothing
    , canvasPosition : { left : 0.0, top : 0.0 }
    }

  render :: State -> H.ComponentHTML Action () Aff
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Guitar Chord Builder" ]
      , HH.canvas
         [ HP.id_ "canvas"
         , HE.onClick canvasClickHandler
         , HP.height canvasHeight --300
         , HP.width  canvasWidth -- 300
         ]
      , renderExportPNGButton state
      ]

  renderExportPNGButton :: State -> H.ComponentHTML Action () Aff
  renderExportPNGButton state =
    let
      enabled =
        true
        -- either (\_ -> false) (\_ -> true) state.tuneResult
      className =
        if enabled then "hoverable" else "unhoverable"
    in
      HH.button
        [ HE.onClick \_ -> Just (Export PNG)
        , HP.class_ $ ClassName className
        , HP.enabled enabled
        ]
        [ HH.text "export as PNG" ]

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
      _ <- H.liftEffect $ Drawing.render graphicsCtx chordDisplay
      -- _ <- H.liftEffect $ Export.canvasToDataURL canvas "image/png"
      _ <- H.modify (\st -> st { mGraphicsContext = Just graphicsCtx
                               , mCanvas = mCanvas })
      _ <- handleQuery (GetCanvasOffset unit)
      pure unit
    EditFingering cx cy -> do
      state <- H.get
      let
        x = toNumber cx - state.canvasPosition.left
        y = toNumber cy - state.canvasPosition.top
        foo = spy "X:" x
        bar = spy "Y:" y
      pure unit
    Export format -> do
      state <- H.get
      let
        canvas = unsafePartial (fromJust state.mCanvas)
        mimeType = toMimeType format
      _ <- H.liftEffect $ exportAs canvas "sampleexport" mimeType
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
      let
        foo = spy "Left:" left
        bar = spy "Top:" top
      _ <- H.modify (\st -> st { canvasPosition  = { left, top } })
      pure (Just next)

  canvasClickHandler :: MouseEvent -> Maybe Action
  canvasClickHandler me =
    Just $ EditFingering (clientX me) (clientY me)
