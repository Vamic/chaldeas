module Site.Servants.Sorting
  ( SortBy(..)
  , toSort
  , getSort
  , formatSort
  )where

import Prelude
import Operators
import Generic as G
import Data.Map as M

import Data.Array
import Data.Foldable (sum)
import Data.Int
import Data.Maybe
import Data.Map (Map, isEmpty)
import Data.Ord
import Data.Profunctor.Strong
import Data.Tuple (Tuple(..))

import Database
import Database.MyServant
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

toSort :: SortBy -> Servant -> Number
toSort ID         (Servant s) = negate $ toNumber s.id
toSort Rarity     (Servant s) = toNumber s.rarity
toSort ATK        (Servant s) = toNumber s.stats.max.atk
toSort HP         (Servant s) = toNumber s.stats.max.hp
toSort StarWeight (Servant s) = toNumber s.gen.starWeight
toSort NPArts              s  = npPer s Arts
toSort NPDeck              s  = sum $ npPer s <$> getDeck s
toSort StarQuick           s  = starsPer s Quick
toSort StarDeck            s  = sum $ starsPer s <$> getDeck s
toSort NPDmg               s  = npDamage false false s
toSort NPDmgOver           s  = npDamage false true s
toSort NPSpec              s  = npDamage true false s
toSort NPSpecOver          s  = npDamage true true s

formatSort :: SortBy -> Number -> String
formatSort NPArts    = flip append "%" <<< places 2
formatSort NPDeck    = flip append "%" <<< places 2
formatSort StarQuick = places 2
formatSort StarDeck  = places 2
formatSort _         = places 0

doSort :: SortBy -> Array MyServant -> Array (Tuple String Servant)
doSort Rarity = map (Tuple "") <<< sortWith sorter <<< map getBase
  where
    sorter (Servant s) = show (5 - s.rarity) <> s.name
doSort x = map showSort <<< sortWith sorter
  where
    sorter (MyServant ms)  = toSort x $ ms.servant
    showSort
      | x `elem` [NPDmg, NPDmgOver, NPSpec, NPSpecOver] = \ms'@(MyServant ms) -> 
          flip Tuple (getBase ms') $
          "NP" <> show ms.npLvl <> ": " <> (formatSort x <<< abs $ sorter ms')
      | otherwise = formatSort x <<< abs <<< sorter &&& getBase

sorted :: Map SortBy (Array (Tuple String Servant))
sorted = M.fromFoldable $ go <$> enumArray
  where
    go sorter = Tuple sorter <<< doSort sorter $ unowned <$> servants

getSort :: Map Servant MyServant -> SortBy -> Array (Tuple String Servant)
getSort team sorter
  | isEmpty team = fromMaybe [] $ M.lookup sorter sorted
  | otherwise    = doSort sorter $ owned team <$> servants

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
