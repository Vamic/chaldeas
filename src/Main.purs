module Main (main) where

import StandardLibrary

import Halogen.Aff as HalogenAff
import Halogen.VDom.Driver as Driver
import Routing.Hash as Hash

import Site.Common
import Site.Component
import Site.Preferences

-- | Runs the website interface.
main :: Effect Unit
main = do
    hash  <- Hash.getHash
    prefs <- getPreferences
    today <- getDate
    team  <- getTeam
    HalogenAff.runHalogenAff $
    HalogenAff.awaitBody >>= Driver.runUI (comp hash prefs today team) unit
