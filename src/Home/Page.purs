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

component :: ∀ i o q m. MonadAff m => H.Component q i o m
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
  render _ =
    HH.div_
      [ HH.h1
          [ HP.class_ (H.ClassName "center") ]
          [ HH.text "Chord Editor" ]
      , HH.ul
          [ HP.class_ (H.ClassName "menu") ]
          [ HH.li_
              [ HH.a
                  [ HP.href "#/frettedInstrument/guitar" ]
                  [ HH.text "Guitar" ]
              ]
          , HH.li_
              [ HH.a
                  [ HP.href "#/frettedInstrument/tenorguitar" ]
                  [ HH.text "Tenor Guitar" ]
              ]
          , HH.li_
              [ HH.a
                  [ HP.href "#/frettedInstrument/ukulele" ]
                  [ HH.text "Ukulele" ]
              ]
          , HH.li_
              [ HH.a
                  [ HP.href "#/bass" ]
                  [ HH.text "Bass" ]
              ]
          , HH.li_
              [ HH.a
                  [ HP.href "#/piano" ]
                  [ HH.text "Piano" ]
              ]
          ]
      ]

