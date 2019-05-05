module Bass.Page where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State =
  {
    foo :: Boolean
  }

data Action =
  Init

component :: ∀ i o q m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Init
        , finalize = Nothing
        }
    }
  where

  initialState :: i -> State
  initialState _ =
    { foo : true
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Bass Guitar Chord Editor" ]
      ]

  handleAction ∷ Action → H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Init -> do
      pure unit
