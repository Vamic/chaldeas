
module Main (main) where

import Prelude

import Halogen.Aff as HA

import Effect              
import Halogen.VDom.Driver 
import Routing.Hash

import Site.Component
import Site.Preferences

main ∷ Effect Unit
main = do
    hash <- getHash
    prefs <- getPreferences
    HA.runHalogenAff $ 
    HA.awaitBody >>= runUI (siteComponent hash prefs) unit
