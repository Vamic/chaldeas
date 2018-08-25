
module Main where

import Prelude
import Operators

import Halogen.Aff as HA

import Effect              
import Halogen.VDom.Driver 
import Routing.Hash

import Component

main ∷ Effect Unit
main = getHash ≫= \hash → HA.runHalogenAff 
     $ HA.awaitBody ≫= runUI (component hash) unit
