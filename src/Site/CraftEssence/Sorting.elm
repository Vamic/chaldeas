module Site.CraftEssence.Sorting exposing (getSort)

import Dict exposing (Dict)

import StandardLibrary       exposing (..)
import Database.CraftEssence exposing (..)
import Printing              exposing (..)
import Sorting               exposing (..)

toSort : SortBy -> CraftEssence -> Float
toSort a ce = case a of
  ID     -> toFloat (-ce.id)
  Rarity -> toFloat ce.rarity
  ATK    -> toFloat ce.stats.max.atk
  HP     -> toFloat ce.stats.max.hp
  _      -> 1/0

doSort : SortBy -> List CraftEssence -> List (String, CraftEssence)
doSort a = case a of
  Rarity -> 
    let
      sorter ce   = String.fromInt (5 - ce.rarity) ++ ce.name
      showSort ce = (Maybe.withDefault "" ce.bond, ce)
    in
      List.sortWith (on compare sorter) 
      >> List.map showSort
  _ ->
    let
      sorter      = toSort a
      showSort ce = 
          ( ce 
            |> sorter 
            >> abs 
            >> formatSort a
          , ce
          )
    in
      List.sortWith (on compare sorter) 
      >> List.map showSort

sorted : Dict OrdSortBy (List (String, CraftEssence))
sorted = dict enumSortBy <| \sorter -> 
    (ordSortBy sorter, doSort sorter craftEssences)

getSort : SortBy -> List (String, CraftEssence)
getSort sorter = Maybe.withDefault [] <| Dict.get (ordSortBy sorter) sorted
