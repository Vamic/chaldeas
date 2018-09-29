module Site.Servants.Sorting (getSort) where

import StandardLibrary
import Data.Map as Map

import Database
import MyServant
import Sorting

getSort :: Boolean -> SortBy -> Array MyServant
        -> Array { label :: String, obj :: MyServant }
getSort _ Rarity = map { label: "", obj: _ } <<< sortWith (sorter <<< getBase)
  where
    sorter (Servant s) = show (5 - s.rarity) <> s.name
getSort addSkills x = map showSort <<< sortWith sorter
  where
    calcWith
      | addSkills = fst
      | otherwise = snd
    sorter (MyServant ms) = fromMaybe infinity $ 
                            calcWith <$> Map.lookup x ms.sorted
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
