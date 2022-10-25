module Navigation.Route where

-- | Routes - URLs which are allowable and provide a round trip between
-- | each Route and a stringified version suitable for a URL

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import FrettedInstrument.Types (FrettedInstrumentName, instrumentNameFromURIString, instrumentNameToURIString)

frettedInstrumentName :: RouteDuplex' String -> RouteDuplex' FrettedInstrumentName
frettedInstrumentName = as instrumentNameToURIString instrumentNameFromURIString 

data Route
  = Home
  | FrettedInstrument FrettedInstrumentName
  | Bass
  | Piano

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "FrettedInstrument": "frettedInstrument" /  (frettedInstrumentName segment)
  , "Bass": "bass" / noArgs
  , "Piano": "piano" / noArgs
  }
