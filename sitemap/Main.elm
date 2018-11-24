port module Main exposing (main)

import Browser

import Html as H exposing (Html)
import Platform exposing (worker)

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.CraftEssence exposing (..)
import Printing              exposing (..)

output : String
output =
    String.join "\n" <<
    List.sort <<
    List.map ((++) "https://chaldeas.surge.sh/") <| 
    ["Servants", "CraftEssences"]
    ++ List.map (.name >> urlName >> ((++) "Servants/")) servants
    ++ List.map (.name >> urlName >> ((++) "CraftEssences/")) craftEssences

port print : String -> Cmd msg

main : Program () () Never
main = worker
    { init = \_ -> ((), print output)
    , update = \_ _ -> pure ()
    , subscriptions = \_ -> Sub.none
    }
