module Main where

import Prelude
import Effect (Effect)
import Data.Foldable (traverse_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

import Container as Container

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  traverse_ (runUI Container.component unit) =<< HA.selectElement  (QuerySelector "#embed-ps-div")
