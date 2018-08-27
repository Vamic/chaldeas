
module Main (main) where

import Prelude

import Halogen.Aff as HA

import Effect              
import Halogen.VDom.Driver 
import Routing.Hash

import Component
import Preferences

main ∷ Effect Unit
main = do
    hash <- getHash
    prefs <- getPreferences
    HA.runHalogenAff $ 
    HA.awaitBody >>= runUI (component hash prefs) unit
