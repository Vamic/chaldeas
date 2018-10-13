port module Main exposing (main, store)

import Browser
import Date exposing (Date)
import Time exposing (Month(..))
import Json.Encode as E

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.CraftEssence exposing (..)
import Site.Application      exposing (app)
import Database.Export as Export

port analytics : String -> Cmd msg
port store     : (String, Value) -> Cmd msg
port export    : (String, Value) -> Cmd msg

uncurryStore : String -> Value -> Cmd msg
uncurryStore k v = store (k, v)

uncurryExport : String -> Value -> Cmd msg
uncurryExport k v = export (k, v)

runExports : Cmd msg
runExports = 
    Cmd.batch 
    [ uncurryExport "servants"      <| E.list Export.servant servants
    , uncurryExport "craftEssences" <| E.list Export.craftEssence craftEssences
    ]

main = Browser.application <| 
       app runExports analytics analytics analytics uncurryStore uncurryStore

