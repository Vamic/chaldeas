port module Main exposing (main)

import Browser
import Json.Encode as E

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.CraftEssence exposing (..)
import Site.Application      exposing (app)
import Class.ToJSON as Export

{-| Updates the analytics webtracker with a new pageview on URL change. -}
port analytics : String -> Cmd msg
{-| Saves data to LocalStorage. -}
port store     : (String, Value) -> Cmd msg
{-| Exports data to a global JavaScript object. -}
port export    : (String, Value) -> Cmd msg
{-| Sets the page title. -}
port title     : String -> Cmd msg

{-| Exports `servants` to `Export.servants`
and `craftEssences` to `Export.craftEssences`. -}
runExports : Cmd msg
runExports =
    Cmd.batch
    [ export ("servants",      E.list Export.servant servants)
    , export ("craftEssences", E.list Export.craftEssence craftEssences)
    ]

{-| Runs the website interface. -}
main = Browser.application <| app runExports analytics title (curry store)
