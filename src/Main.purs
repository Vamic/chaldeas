
module Main (main) where

import Prelude

import Halogen.Aff as HA

import Effect              
import Halogen.VDom.Driver 
import Routing.Hash

import Component

main ∷ Effect Unit
main = getHash >>= \hash -> HA.runHalogenAff 
     $ HA.awaitBody >>= runUI (component hash) unit
