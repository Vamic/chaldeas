
module Main (main) where

import Prelude

import Halogen.Aff as HA

import Effect (Effect)
import Halogen.VDom.Driver (runUI)
import Routing.Hash (getHash)

import Site.Component (siteComponent)
import Site.Preferences (getPreferences)

main ∷ Effect Unit
main = do
    hash <- getHash
    prefs <- getPreferences
    HA.runHalogenAff $
    HA.awaitBody >>= runUI (siteComponent hash prefs) unit
