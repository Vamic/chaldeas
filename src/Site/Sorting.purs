module Site.Sorting 
  ( SortBy(..)
  , getSort
  )where

import Prelude
import Operators

import Data.Map as M

import Data.Array
import Data.Enum
import Data.Formatter.Number
import Data.Generic.Rep
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum
import Data.Generic.Rep.Show
import Data.Int
import Data.Maybe
import Data.Map (Map)
import Data.Ord
import Data.Profunctor.Strong
import Data.Tuple

import Database

data SortBy = Rarity
            | ID
            | ATK
            | HP
            | GrailATK
            | GrailHP
            | NPGain
            | StarRate
            | NPArts
            | StarQuick
            | NPDamage
            | SpecialNP

instance _a_ ∷ Show SortBy where
  show StarRate   = "Star Rate"
  show NPDamage   = "NP Damage"
  show SpecialNP  = "NP Special Damage"
  show NPGain     = "NP Gain/Hit"
  show NPArts     = "NP Gain/Arts card"
  show StarQuick  = "Stars/Quick card"
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

toSort ∷ SortBy -> Servant -> Number
toSort ID        = (-1.0 * _) ∘ toNumber ∘ _.id
toSort Rarity    = toNumber ∘ _.rarity
toSort NPDamage  = npDamage false 
toSort SpecialNP = npDamage true
toSort ATK       = toNumber ∘ _.stats.max.atk
toSort HP        = toNumber ∘ _.stats.max.hp
toSort GrailATK  = toNumber ∘ _.stats.grail.atk
toSort GrailHP   = toNumber ∘ _.stats.grail.hp
toSort NPGain    = _.gen.npAtk
toSort StarRate  = _.gen.starRate 
toSort NPArts    = npPerArts
toSort StarQuick = starsPerQuick

doSort ∷ SortBy -> Array Servant -> Array (Tuple String Servant)
doSort Rarity = map (Tuple "") ∘ sortWith \s -> show (5 - s.rarity) ++ s.name
doSort a = map showSort ∘ sortWith sorter
  where
    sorter   = toSort a
    showSort 
      | a == SpecialNP || a == NPDamage = \s -> (flip Tuple) s 
          $ (if s.free || s.rarity < 4 then "NP5: " else "NP1: ") 
         ++ (output ∘ abs $ sorter s)
      | otherwise = uncurry Tuple ∘ (output ∘ abs ∘ sorter &&& identity)
    output = case a of
        NPGain    -> (_ ++ "%") ∘ print 2
        StarRate  -> (_ ++ "%") ∘ print 2
        NPArts    -> (_ ++ "%") ∘ print 2
        StarQuick -> print 2
        _         -> print 0

sorted ∷ Map SortBy (Array (Tuple String Servant))
sorted = M.fromFoldable $ go <$> enumArray
  where
    go sorter = Tuple sorter $ doSort sorter servants

getSort ∷ SortBy -> Array (Tuple String Servant)
getSort sorter = fromMaybe [] $ M.lookup sorter sorted

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
