module Site.Servants.Sorting (getSort) where

import StandardLibrary
import Data.Map as Map

import Data.Profunctor.Strong ((&&&))

import Database
import MyServant
import MyServant.Sorting

getSort :: SortBy -> Array MyServant -> Array (Tuple String MyServant)
getSort Rarity = map (Tuple "") <<< sortWith (sorter <<< getBase)
  where
    sorter (Servant s) = show (5 - s.rarity) <> s.name
getSort x = map showSort <<< sortWith sorter
  where
    sorter (MyServant ms)  = fromMaybe (-1.0) $ Map.lookup x ms.sorted
    showSort
      | x `elem` [NPDmg, NPDmgOver, NPSpec, NPSpecOver] = \ms'@(MyServant ms) -> 
          "NP" <> show ms.npLvl <> ": " <> (formatSort x <<< abs $ sorter ms')
          ^ ms'
      | otherwise = formatSort x <<< abs <<< sorter &&& identity
