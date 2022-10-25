module Navigation.Router where

-- | The Router Halogen Component

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (hush)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Navigation.Route (Route(..), routeCodec)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Navigation.Navigate (class Navigate, navigate)
import Bass.Page as Bass
import Piano.Page as Piano
import FrettedInstrument.Types (FrettedInstrumentConfig, FrettedInstrumentName(..))
import FrettedInstrument.Page as FrettedInstrument
import FrettedInstrument.Guitar.Config (config) as Guitar
import FrettedInstrument.TenorGuitar.Config (config) as TenorGuitar
import Home.Page as Home
import Type.Proxy (Proxy(..))

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the component
-- | is by sending input.
type OpaqueSlot = H.Slot (Const Void) Void

type State =
  { route :: Maybe Route }

data Query a
  = Navigate Route a

data Action
  = Initialize

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , frettedInstrument :: FrettedInstrument.Slot Unit
  , bass :: Bass.Slot Unit
  , piano :: Piano.Slot Unit
  )

component :: ∀ m. MonadAff m => Navigate m => H.Component Query Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> { route: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
      -- and, finally, we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Home ->
        HH.slot_ (Proxy :: _ "home") unit Home.component unit
      FrettedInstrument example -> 
        let 
          config = getFrettedInstrumentConfig example
        in 
          HH.slot_ (Proxy :: _ "frettedInstrument") unit FrettedInstrument.component { config }
      Piano ->
        HH.slot_ (Proxy :: _ "piano") unit Piano.component unit
      Bass ->
        HH.slot_ (Proxy :: _ "bass") unit Bass.component unit

    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]

-- | get the configuration of a prticular fretted instrument example
getFrettedInstrumentConfig :: FrettedInstrumentName -> FrettedInstrumentConfig
getFrettedInstrumentConfig name = 
  case name of 
    Guitar -> 
      Guitar.config 
    TenorGuitar -> 
      TenorGuitar.config 

    