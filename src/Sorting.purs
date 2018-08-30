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
import Data.Ord
import Data.Profunctor.Strong
import Data.Tuple

import Database

data SortBy = ID
            | Rarity
            | ATK
            | HP
            | StarRate
            | Hits
            | NPGain
            | NPArts
            | GrailATK
            | GrailHP
            | NPDamage

instance _a_ ∷ Show SortBy where
  show StarRate   = "Star Rate"
  show NPDamage   = "NP Damage"
  show NPGain     = "NP Gain/Hit"
  show NPArts     = "NP Gain/Arts card"
  show GrailATK   = "Grail ATK"
  show GrailHP    = "Grail HP"
  show a = genericShow a

print ∷ Int -> Number -> String
print places = format $ Formatter { comma: true
                                  , before: 0
                                  , after: places
                                  , abbreviations: false
                                  , sign: false
                                  }
print' ∷ Int -> String
print' = print 0 ∘ toNumber

toSort ∷ SortBy -> Servant -> Number
toSort ID {id} = -1.0 * toNumber id
toSort Rarity {rarity} = toNumber rarity
toSort NPDamage s = npDamage s
toSort NPGain {gen:{npAtk}} = npAtk
toSort ATK {stats:{max:{atk}}} = toNumber atk
toSort HP {stats:{max:{hp}}} = toNumber hp
toSort GrailATK {stats:{grail:{atk}}} = toNumber atk
toSort GrailHP {stats:{grail:{hp}}} = toNumber hp
toSort StarRate {gen:{starRate}} = starRate
toSort NPArts {gen:{npAtk}, hits:{arts}} = 2.0 * npAtk * toNumber arts
toSort Hits {hits:{arts,buster,quick,ex}} 
    = toNumber $ arts + buster + quick + ex

doSort ∷ SortBy -> Array Servant -> Array (Tuple String Servant)
doSort Rarity = map (Tuple "") ∘ sortWith \s -> show (5 - s.rarity) ++ s.name
doSort a = map (uncurry Tuple ∘ showSort) ∘ sortWith sorter
  where
    sorter   = toSort a
    showSort = output ∘ abs ∘ sorter &&& identity
    output = case a of
        NPGain   -> (_ ++ "%") ∘ print 2
        StarRate -> (_ ++ "%") ∘ print 2
        NPArts   -> (_ ++ "%") ∘ print 2
        _        -> print 0

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _7_ ∷ Generic SortBy _
derive instance _8_ ∷ Eq SortBy
derive instance _9_ ∷ Ord SortBy
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
