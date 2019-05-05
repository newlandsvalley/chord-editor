module Home.Page where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State =
  {}

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
    {}

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Chord Editor - Home Page" ]
      , HH.ul
         [HP.class_ (H.ClassName "menu") ]
         [ HH.li_
             [ HH.a
                [HP.href "#/guitar" ]
                [ HH.text "Guitar"  ]
             ]
         , HH.li_
             [ HH.a
                [HP.href "#/bass" ]
                [ HH.text "Bass"  ]
             ]
         , HH.li_
             [ HH.a
                [HP.href "#/piano" ]
                [ HH.text "Piano"  ]
             ]
         ]
      ]

  handleAction ∷ Action → H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Init -> do
      pure unit
