module Site.Servant.Sorting exposing (getSort)

import Dict exposing (Dict)

import StandardLibrary exposing  (..)
import MyServant       exposing (..)
import Sorting         exposing (..)

getSort : Bool -> SortBy -> List MyServant -> List (String, MyServant)
getSort addSkills a = case a of
  Rarity ->
    let
      sorter {base} = String.fromInt (5 - base.rarity) ++ base.name
    in
      List.sortWith (on compare sorter)
      >> List.map (\x -> ("", x))
  _ ->
    let
      sorter ms =
          Dict.get (ordSortBy a) ms.sorted
          |> Maybe.withDefault (1/0, 1/0)
          >> if addSkills then Tuple.first else Tuple.second
      showSort ms =
          ( ms
            |> sorter
            >> abs
            >> formatSort a
            >> if not <| List.member a [NPDmg, NPDmgOver, NPSpec, NPSpecOver]
               then identity else
               (++) <| "NP" ++ String.fromInt ms.npLvl ++ ": "
          , ms
          )
    in
      List.sortWith (on compare sorter)
      >> List.map showSort
