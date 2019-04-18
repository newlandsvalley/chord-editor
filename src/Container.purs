module Container where

import Prelude
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Graphics.Canvas (Context2D, CanvasElement, getCanvasElementById, getContext2D)
import Graphics.Drawing (render) as Drawing
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Graphics (canvasHeight, canvasWidth, chordDisplay)
import Export (exportAs)
import Types (ExportFormat(..), toMimeType)


type State =
  { -- mAudioContext :: Maybe AudioContext
    mGraphicsContext :: Maybe Context2D
  , mCanvas :: Maybe CanvasElement
  }

data Action =
    Init
  | Export ExportFormat

component :: ∀ i o q. H.Component HH.HTML q i o Aff
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
    }

  render :: State -> H.ComponentHTML Action () Aff
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Guitar Chord Builder" ]
      , HH.canvas
         [ HP.id_ "canvas"
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
      pure unit
    Export format -> do
      state <- H.get
      let
        canvas = unsafePartial (fromJust state.mCanvas)
        mimeType = toMimeType format
      _ <- H.liftEffect $ exportAs canvas "sampleexport" mimeType
      pure unit
