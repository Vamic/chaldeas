module Sorting (SortBy(..), formatSort) where

import StandardLibrary
import Generic as G

import Printing

data SortBy
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

instance _a_ :: Show SortBy where
    show NPArts     = "NP Gain per Arts card"
    show NPDeck     = "NP Gain per full deck"
    show StarQuick  = "Stars per Quick card"
    show StarDeck   = "Stars per full deck"
    show NPDmg      = "NP Damage"
    show NPDmgOver  = "NP Damage + Overcharge"
    show NPSpec     = "NP Special Damage"
    show NPSpecOver = "NP Special + Overcharge"
    show x          = unCamel $ G.genericShow x

formatSort :: SortBy -> Number -> String
formatSort NPArts    = flip append "%" <<< places 2
formatSort NPDeck    = flip append "%" <<< places 2
formatSort StarQuick = places 2
formatSort StarDeck  = places 2
formatSort _         = places 0

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _7_ :: G.Generic SortBy _
derive instance _8_ :: Eq SortBy
derive instance _9_ :: Ord SortBy
instance _11_ :: G.Enum SortBy where
    succ = G.genericSucc
    pred = G.genericPred
instance _12_ :: G.Bounded SortBy where
    top = G.genericTop
    bottom = G.genericBottom
instance _13_ :: G.BoundedEnum SortBy where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum
