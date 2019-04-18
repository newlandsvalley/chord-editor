module Container where

import Prelude
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Graphics.Drawing (render) as Drawing
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Graphics (chordDisplay)


type State =
  { -- mAudioContext :: Maybe AudioContext
    mGraphicsContext :: Maybe Context2D
  }

data Action =
    Init

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
    }

  render :: State -> H.ComponentHTML Action () Aff
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Guitar Chord Builder" ]
      , HH.canvas
         [ HP.id_ "canvas"
         , HP.height 350
         , HP.width  800
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
      _ <- H.liftEffect $ Drawing.render graphicsCtx chordDisplay
      pure unit
