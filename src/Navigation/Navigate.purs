module Navigation.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Navigation.Route (Route)
import Halogen (HalogenM)

-- | This type class represents the ability to move around the application. The `navigate` function
-- | should change the browser location, which will then notify our routing component.
class Monad m <= Navigate m where
  navigate :: Route -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
