port module Main exposing (main)

import Browser

import Maybe.Extra as Maybe
import Platform exposing (worker)

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.CraftEssence exposing (..)

import Site.CraftEssence.Filters as CraftEssences
import Site.Servant.Filters      as Servants

missingBond : CraftEssence -> Bool
missingBond ce = case ce.bond of
  Nothing -> False
  Just s  -> not << List.member s <| List.map .name servants

errors : List String
errors =
  let
    missingBonds =
        craftEssences
        |> List.filter missingBond
        >> List.map .name
  in
    suite "Invalid CE Bond" missingBonds
    ++ CraftEssences.errors
    ++ Servants.errors

port print : String -> Cmd msg

output : ()
output = case errors of
  [] -> ()
  _  -> Debug.todo <| String.join "\n" errors

main : Program () () Never
main = worker
    { init = \_ -> pure output
    , update = \_ _ -> pure ()
    , subscriptions = \_ -> Sub.none
    }
