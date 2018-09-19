module Site.CraftEssences.Sorting (getSort) where

import StandardLibrary
import Data.Map as Map
import Data.Int as Int

import Database
import Printing
import Sorting

toSort :: SortBy -> CraftEssence -> Number
toSort ID     (CraftEssence ce) = negate $ Int.toNumber ce.id
toSort Rarity (CraftEssence ce) = Int.toNumber ce.rarity
toSort ATK    (CraftEssence ce) = Int.toNumber ce.stats.max.atk
toSort HP     (CraftEssence ce) = Int.toNumber ce.stats.max.hp
toSort _      _                 = infinity

doSort :: SortBy -> Array CraftEssence
       -> Array { label :: String, obj :: CraftEssence }
doSort Rarity = map showSort <<< sortWith sorter
  where
    sorter (CraftEssence ce) = show (5 - ce.rarity) <> ce.name
    showSort ce'@(CraftEssence ce) = { label: fromMaybe "" ce.bond, obj: ce'}
doSort x = map showSort <<< sortWith sorter
  where
    sorter      = toSort x
    showSort ce = { label: output <<< abs $ sorter ce, obj: ce }
    output      = places 0

sorted :: Map SortBy (Array {label :: String, obj :: CraftEssence})
sorted = Map.fromFoldable $ go <$> enumArray
  where
    go sorter = (sorter : doSort sorter craftEssences)

getSort :: SortBy -> Array { label :: String, obj ::  CraftEssence }
getSort sorter = fromMaybe [] $ Map.lookup sorter sorted
