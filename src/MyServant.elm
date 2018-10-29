module MyServant exposing
  ( MyServant
  , recalc
  , unowned, newServant, owned
  )
{-| CHALDEAS has a "My Servants" feature for users to keep track of levels
and stats for particular Servants. This module defines the container for
such information, which is a `Database.Servant` wrapper with additional
user info such as Fou stats and skill levels. -}

import List.Extra  as List
import Dict exposing (Dict)

import StandardLibrary        exposing (..)
import Database               exposing (..)
import Database.Base          exposing (..)
import Database.Calculator    exposing (..)
import Database.Skill         exposing (..)
import Database.Servant       exposing (..)
import Sorting                exposing (..)
import MyServant.GrowthCurves exposing (..)

type alias MyServant =
    { level   : Int
    , fou     : Stat
    , skills  : List Int
    , npLvl   : Int
    , ascent  : Int
    , base    : Servant
    , servant : Servant
    , sorted  : Dict OrdSortBy (Float, Float)
    }

eqMyServant : MyServant -> MyServant -> Bool
eqMyServant = on eqServant .base

recalc : MyServant -> MyServant
recalc ms =
  let
    s         = ms.base
    stats     = s.stats
    calcStats = addStats ms.fou <| lvlStats s ms.level
    calcNP minAmount maxAmount =
        Flat <| minAmount + (maxAmount - minAmount) * case ms.npLvl of
          1 -> 0
          2 -> 0.5
          3 -> 0.75
          4 -> 0.875
          _ -> 1
    calcOver minAmount maxAmount = case ms.npLvl of
      1 -> Flat minAmount
      _ -> Range minAmount <| minAmount
                            + (maxAmount - minAmount)
                            * (toFloat ms.npLvl - 1)
                            / 4
    calcActives lvl skill =
      let
        calc minAmount maxAmount = case lvl of
          10 -> Flat maxAmount
          _  -> Flat <| minAmount
                      + (maxAmount - minAmount)
                      * (toFloat lvl - 1)
                      / 10
      in
        { skill
        | effect = List.map (mapAmount calc) skill.effect
        , cd     = skill.cd - (max 2 lvl - 2) // 4
        }
  in
    mapSort
    { ms
    | servant =
      { s
      | stats    = let x = s.stats
                   in { x | base = calcStats, max = calcStats }
      , phantasm = let x = s.phantasm
                   in { x
                      | effect = List.map (mapAmount calcNP) s.phantasm.effect
                      , over   = case ms.level of
                          0 -> s.phantasm.over
                          _ -> List.map (mapAmount calcOver) s.phantasm.over
                      }
      , skills   = List.map2 calcActives ms.skills s.skills
      }
    }
toSort : Bool -> SortBy -> Servant -> Float
toSort addSkills sortBy s = case sortBy of
  ID         -> toFloat <| -1 * s.id
  Rarity     -> toFloat s.rarity
  ATK        -> toFloat s.stats.max.atk
  HP         -> toFloat s.stats.max.hp
  StarWeight -> toFloat s.gen.starWeight
  NPArts     -> npPer s Arts
  NPDeck     -> getDeck s |> List.map (npPer s) >> List.sum
  StarQuick  -> starsPer s Quick
  StarDeck   -> getDeck s |> List.map (starsPer s) >> List.sum
  NPDmg      -> npDamage addSkills False False s
  NPDmgOver  -> npDamage addSkills False True s
  NPSpec     -> npDamage addSkills True False s
  NPSpecOver -> npDamage addSkills True True s

mapSort : MyServant -> MyServant
mapSort ms =
  let
    go sorter =
      let
        doSort addSkills = toSort addSkills sorter ms.servant
      in
        (ordSortBy sorter, (doSort True, doSort False))
  in
    { ms | sorted = dict enumSortBy go }

makeUnowned : Servant -> MyServant
makeUnowned s = mapSort <|
    { servant = s
    , base    = s
    , level   = 0
    , fou     = { atk = 990, hp = 990 }
    , skills  = [10, 10, 10]
    , npLvl   = if s.free || (s.rarity <= 3 && s.rarity > 0) then 5 else 1
    , ascent  = 1
    , sorted  = Dict.empty
    }

unowneds : Dict OrdServant MyServant
unowneds = dict servants <| \x -> (ordServant x, makeUnowned x)

unowned : Servant -> MyServant
unowned s = case Dict.get (ordServant s) unowneds of
    Just ms -> ms
    Nothing -> makeUnowned s -- But if this actually happens, something is weird

newServant : Servant -> MyServant
newServant s =
    { servant = s
    , base    = s
    , level   = 1
    , fou     = { atk = 0, hp = 0 }
    , skills  = [1, 1, 1]
    , npLvl   = 1
    , ascent  = 1
    , sorted  = Dict.empty
    }

owned : Dict OrdServant MyServant -> Servant -> MyServant
owned team s = case Dict.get (ordServant s) team of
  Just ms -> ms
  Nothing -> unowned s
