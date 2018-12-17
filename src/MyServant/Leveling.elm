module MyServant.Leveling exposing
  ( maxLevel
  , ascendCost, skillCost
  , ascendWishlist, skillWishlist
  )

{-| Fixed rates for leveling `Servant`s based on their rarity. -}

import Array

import StandardLibrary  exposing (..)
import Database.Base    exposing (..)
import Database.Servant exposing (..)
import MyServant        exposing (..)

maxLevel : Servant -> Int
maxLevel {rarity} = case rarity of
  1 -> 60
  3 -> 70
  4 -> 80
  5 -> 90
  _ -> 65

ascendCost : Servant -> Int -> Int
ascendCost {name, rarity} =
  let
    costs = Array.fromList <| case rarity of
      1 -> [ 10,  30,   90,  300]
      3 -> [ 30, 100,  300,  900]
      4 -> [ 50, 150,  500, 1500]
      5 -> [100, 300, 1000, 3000]
      _ -> [ 15,  45,  150,  450]
  in
    case name of
      "Mash Kyrielight" -> always 0
      _ ->
          flip Array.get costs
          >> Maybe.withDefault 0
          >> (*) 1000

atAscension : MyServant -> Int
atAscension {level, base, ascent} = 
  let
    tier = (-) level <| case base.rarity of
      5 -> 50
      4 -> 40
      3 -> 30
      1 -> 20
      _ -> 25
    ascension = tier // 10
  in min 4 << max 0 <|
    if remainderBy 10 tier == 0 && ascent > ascension then 
      ascension + 1
    else 
      ascension

skillWishlist : List MyServant -> List (Material, Int)
skillWishlist xs =
  let
    bind = flip List.concatMap
  in
    reduceMats <<
    bind xs <| \ms ->
    let reinforce = getReinforcements ms.base in
    bind ms.skills <| \skillLvl ->
    List.drop (skillLvl - 1) reinforce

ascendWishlist : List MyServant -> List (Material, Int)
ascendWishlist xs = reduceMats <<
    flip List.concatMap xs <| \ms ->
    List.drop (atAscension ms) <| getAscensions ms.base

skillCost : Servant -> Int -> Int
skillCost {rarity} =
  let
    costs = Array.fromList <| case rarity of
      1 -> [ 10,  20,   60,   80,  200,  250,   500,   600,  1000]
      3 -> [ 50, 100,  300,  400, 1000, 1250,  2500,  3000,  5000]
      4 -> [100, 200,  600,  800, 2000, 2500,  5000,  6000, 10000]
      5 -> [200, 400, 1200, 1600, 4000, 5000, 10000, 12000, 20000]
      _ -> [ 20,  40,  120,  160,  400,  500,  1000,  1200,  2000]
  in
    flip Array.get costs
    >> Maybe.withDefault 0
    >> (*) 1000
