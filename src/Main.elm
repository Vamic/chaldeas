port module Main exposing (main, store)

import Browser
import Date exposing (Date)
import Time exposing (Month(..))

import Site.Application exposing (app)
import StandardLibrary exposing (..)

port store : (String, Value) -> Cmd msg

uncurryStore : String -> Value -> Cmd msg
uncurryStore k v = store (k, v)

main = Browser.application <| app uncurryStore uncurryStore

