
module Main where

import Prelude
import Operators

import Halogen.Aff as HA

import Effect              (Effect)
import Halogen.VDom.Driver (runUI)
import Routing.Hash

import Component

main ∷ Effect Unit
main = getHash ≫= \hash → HA.runHalogenAff 
     $ HA.awaitBody ≫= runUI (component hash) unit
