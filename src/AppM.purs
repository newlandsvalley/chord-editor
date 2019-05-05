module AppM (AppM, toAff) where

-- | our monad is just Aff with Navigation attached to it

import Prelude
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Navigation.Route (routeCodec)
import Navigation.Navigate (class Navigate)

newtype AppM a = AppM (Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print routeCodec

toAff :: AppM ~> Aff
toAff (AppM m) =
  m
