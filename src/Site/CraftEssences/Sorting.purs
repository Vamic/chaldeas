module Site.CraftEssences.Sorting
  ( SortBy(..)
  , getSort
  )where

import Prelude
import Operators
import Generic as G
import Data.Map as M

import Data.Array
import Data.Int
import Data.Maybe
import Data.Map
import Data.Ord
import Data.Profunctor.Strong
import Data.Tuple

import Database
import Printing

data SortBy
    = Rarity
    | ID
    | ATK
    | HP

instance _a_ :: Show SortBy where
  show = G.genericShow

toSort :: SortBy -> CraftEssence -> Number
toSort ID     (CraftEssence ce) = negate $ toNumber ce.id
toSort Rarity (CraftEssence ce) = toNumber ce.rarity
toSort ATK    (CraftEssence ce) = toNumber ce.stats.max.atk
toSort HP     (CraftEssence ce) = toNumber ce.stats.max.hp

doSort :: SortBy -> Array CraftEssence -> Array (Tuple String CraftEssence)
doSort Rarity = map showSort <<< sortWith sorter
  where
    sorter (CraftEssence ce) = show (5 - ce.rarity) <> ce.name
    showSort ce'@(CraftEssence ce) = Tuple (fromMaybe "" ce.bond) ce'
doSort a = map showSort <<< sortWith sorter
  where
    sorter   = toSort a
    showSort = output <<< abs <<< sorter &&& identity
    output   = print 0

sorted :: Map SortBy (Array (Tuple String CraftEssence))
sorted = M.fromFoldable $ go <$> enumArray
  where
    go sorter = Tuple sorter $ doSort sorter craftEssences

getSort :: SortBy -> Array (Tuple String CraftEssence)
getSort sorter = fromMaybe [] $ M.lookup sorter sorted

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
