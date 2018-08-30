module Sorting where

import Prelude
import Operators

import Data.Array
import Data.Enum
import Data.Formatter.Number
import Data.Generic.Rep
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum
import Data.Generic.Rep.Show
import Data.Int
import Data.Profunctor.Strong
import Data.Tuple

import Database

data SortBy = Rarity
            | ATK
            | HP
            | StarRate
            | Hits
            | NPGain
            | NPDamage

print' ∷ Number -> String
print' = format $ Formatter { comma: true
                            , before: 0
                            , after: 0
                            , abbreviations: false
                            , sign: false
                            } 
print ∷ Int -> String
print = print' ∘ toNumber

toSort ∷ SortBy -> Servant -> Number
toSort Rarity {rarity}           = toNumber rarity
toSort NPDamage s                = npDamage s
toSort NPGain {gen:{npAtk}}      = npAtk
toSort ATK {stats:{max:{atk}}}   = toNumber atk
toSort HP {stats:{max:{hp}}}     = toNumber hp
toSort StarRate {gen:{starRate}} = starRate
toSort Hits {hits:{arts,buster,quick,ex}} 
    = toNumber $ arts + buster + quick + ex

doSort ∷ SortBy -> Array Servant -> Array (Tuple String Servant)
doSort Rarity = map (Tuple "") ∘ sortWith \s -> show (5 - s.rarity) ++ s.name
doSort a = map (uncurry Tuple ∘ showSort) ∘ sortWith sorter
  where
    sorter   = toSort a
    showSort = output ∘ sorter &&& identity
    output = case a of
        NPGain   -> (_ ++ "%") ∘ show
        StarRate -> (_ ++ "%") ∘ show
        _        -> print'

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _7_ ∷ Generic SortBy _
derive instance _8_ ∷ Eq SortBy
derive instance _9_ ∷ Ord SortBy
instance _10_ ∷ Show SortBy where
  show StarRate = "Star Rate"
  show NPDamage = "NP Damage"
  show NPGain   = "NP Gain/Hit"
  show a = genericShow a
instance _11_ ∷ Enum SortBy where
  succ = genericSucc
  pred = genericPred
instance _12_ ∷ Bounded SortBy where
  top = genericTop
  bottom = genericBottom
instance _13_ ∷ BoundedEnum SortBy where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
