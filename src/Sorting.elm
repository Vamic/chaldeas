module Sorting exposing 
  ( SortBy(..), showSortBy, enumSortBy, OrdSortBy, ordSortBy
  , formatSort
  )

import List.Extra as List

import StandardLibrary exposing (..)
import Printing        exposing (..)

type SortBy
    = Rarity
    | ID
    | ATK
    | HP
    | StarWeight
    | NPArts
    | NPDeck
    | StarQuick
    | StarDeck
    | NPDmg
    | NPDmgOver
    | NPSpec
    | NPSpecOver

enumSortBy : List SortBy
enumSortBy = 
    [ Rarity 
    , ID
    , ATK
    , HP
    , StarWeight
    , NPArts
    , NPDeck
    , StarQuick
    , StarDeck
    , NPDmg
    , NPDmgOver
    , NPSpec
    , NPSpecOver
    ]

type alias OrdSortBy = Int

ordSortBy : SortBy -> OrdSortBy
ordSortBy = enumToOrd enumSortBy

showSortBy : SortBy -> String
showSortBy a = case a of
  NPArts     -> "NP Gain per Arts card"
  NPDeck     -> "NP Gain per full deck"
  StarQuick  -> "Stars per Quick card"
  StarDeck   -> "Stars per full deck"
  NPDmg      -> "NP Damage"
  NPDmgOver  -> "NP Damage + Overcharge"
  NPSpec     -> "NP Special Damage"
  NPSpecOver -> "NP Special + Overcharge"
  _          -> unCamel <| Debug.toString a

formatSort : SortBy -> Float -> String
formatSort a = case a of
  NPDmg      -> commas
  NPDmgOver  -> commas
  NPSpec     -> commas
  NPSpecOver -> commas
  NPArts     -> places 2 >> flip (++) "%"
  NPDeck     -> places 2 >> flip (++) "%"
  StarQuick  -> places 2
  StarDeck   -> places 2
  _          -> places 0
