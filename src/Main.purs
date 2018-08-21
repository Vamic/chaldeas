
module Main where

import Prelude

import Halogen.Aff as HA

import Effect              (Effect)
import Halogen.VDom.Driver (runUI)

import Component.Site

main ∷ Effect Unit
main = HA.runHalogenAff do
    body ← HA.awaitBody
    runUI component unit body
