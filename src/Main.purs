module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Guitar.Page as Guitar

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Guitar.component unit body
