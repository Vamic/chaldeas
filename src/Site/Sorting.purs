module Site.Sorting
  ( SortBy(..)
  , getSort
  )where

import Prelude
import Operators (enumArray, unCamel)
import Generic as G
import Data.Map as M

import Data.Array (elem, sortWith)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Ord (abs)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..), uncurry)

import Database (Servant, npDamage, npPerArts, servants, starsPerQuick)
import Printing (print)

data SortBy
    = Rarity
    | ID
    | ATK
    | HP
    -- | GrailATK
    -- | GrailHP
    -- | NPGain
    | StarWeight
    -- | StarRate
    | NPArts
    | StarQuick
    | NPDmg
    | NPDmgOver
    | NPSpec
    | NPSpecOver

instance _a_ ∷ Show SortBy where
  -- show NPGain     = "NP Gain/Hit"
  -- show StarRate   = "Star Rate"
  show NPArts     = "NP Gain per Arts card"
  show StarQuick  = "Stars per Quick card"
  -- show GrailATK   = "Grail ATK"
  -- show GrailHP    = "Grail HP"
  show NPDmg      = "NP Damage"
  show NPDmgOver  = "NP Damage + Overcharge"
  show NPSpec     = "NP Special Damage"
  show NPSpecOver = "NP Special + Overcharge"
  show a = unCamel $ G.genericShow a

toSort ∷ SortBy -> Servant -> Number
toSort ID         = (-1.0 * _) <<< toNumber <<< _.id
toSort Rarity     = toNumber <<< _.rarity
toSort ATK        = toNumber <<< _.stats.max.atk
toSort HP         = toNumber <<< _.stats.max.hp
-- toSort GrailATK   = toNumber <<< _.stats.grail.atk
-- toSort GrailHP    = toNumber <<< _.stats.grail.hp
-- toSort NPGain     = _.gen.npAtk
toSort StarWeight = toNumber <<< _.gen.starWeight
-- toSort StarRate   = _.gen.starRate
toSort NPArts     = npPerArts
toSort StarQuick  = starsPerQuick
toSort NPDmg      = npDamage false false
toSort NPDmgOver  = npDamage false true
toSort NPSpec     = npDamage true false
toSort NPSpecOver = npDamage true true

doSort ∷ SortBy -> Array Servant -> Array (Tuple String Servant)
doSort Rarity = map (Tuple "") <<< sortWith \s -> show (5 - s.rarity) <> s.name
doSort a = map showSort <<< sortWith sorter
  where
    sorter   = toSort a
    showSort
      | a `elem` [NPDmg, NPDmgOver, NPSpec, NPSpecOver] = \s -> (flip Tuple) s
          $ (if s.free || s.rarity < 4 then "NP5: " else "NP1: ")
         <> (output <<< abs $ sorter s)
      | otherwise = uncurry Tuple <<< (output <<< abs <<< sorter &&& identity)
    output = case a of
        -- NPGain    -> (_ <> "%") <<< print 2
        -- StarRate  -> (_ <> "%") <<< print 2
        NPArts    -> (_ <> "%") <<< print 2
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

derive instance _7_ ∷ G.Generic SortBy _
derive instance _8_ ∷ Eq SortBy
derive instance _9_ ∷ Ord SortBy
instance _11_ ∷ G.Enum SortBy where
  succ = G.genericSucc
  pred = G.genericPred
instance _12_ ∷ G.Bounded SortBy where
  top = G.genericTop
  bottom = G.genericBottom
instance _13_ ∷ G.BoundedEnum SortBy where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum
