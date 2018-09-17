module Site.Servants.Sorting (getSort) where

import StandardLibrary
import Data.Map as Map

import Database
import MyServant
import MyServant.Sorting

getSort :: SortBy -> Array MyServant 
        -> Array { label :: String, obj :: MyServant }
getSort Rarity = map { label: "", obj: _ } <<< sortWith (sorter <<< getBase)
  where
    sorter (Servant s) = show (5 - s.rarity) <> s.name
getSort x = map showSort <<< sortWith sorter
  where
    sorter (MyServant ms)  = fromMaybe (-1.0) $ Map.lookup x ms.sorted
    showSort ms'@(MyServant ms)
      | x `elem` [NPDmg, NPDmgOver, NPSpec, NPSpecOver] = 
          { label: "NP" <> show ms.npLvl <> ": " 
                   <> (formatSort x <<< abs $ sorter ms')
          , obj: ms'
          }
      | otherwise = 
          { label: formatSort x <<< abs $ sorter ms'
          , obj:   ms'
          }
