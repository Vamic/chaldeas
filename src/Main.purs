
module Main (main) where

import Prelude

import Halogen.Aff as HA

import Effect
import Halogen.VDom.Driver
import Routing.Hash

import Site.Common
import Site.Component
import Site.Preferences

main ∷ Effect Unit
main = do
    hash  <- getHash
    prefs <- getPreferences
    today <- getDate
    team  <- getTeam
    HA.runHalogenAff $
    HA.awaitBody >>= runUI (comp hash prefs today team) unit
