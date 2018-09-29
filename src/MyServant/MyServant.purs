-- | CHALDEAS has a "My Servants" feature for users to keep track of levels
-- | and stats for particular Servants. This module defines the container for
-- | such information, which is a `Database.Servant` wrapper with additional
-- | user info.
module MyServant
  ( MyServant(..), getBase
  , recalc
  , unowned, newServant, owned
  ) where

import StandardLibrary
import Data.Int as Int
import Data.Map as Map

import Data.Profunctor.Strong ((&&&))

import Sorting
import Database
import MyServant.GrowthCurves

newtype MyServant = MyServant { base    :: Servant
                              , level   :: Int
                              , fou     :: Stat
                              , skills  :: Array Int
                              , npLvl   :: Int
                              , ascent  :: Int
                              , sorted  :: Map SortBy Number
                              , servant :: Servant
                              }
instance _0_ :: Eq MyServant where
    eq x y = eq (getBase x) (getBase y)
instance _1_ :: Ord MyServant where
    compare = comparing getBase
instance _2_ :: Show MyServant where
    show = show <<< getBase

getBase :: MyServant -> Servant
getBase (MyServant {base}) = base

recalc :: MyServant -> MyServant
recalc (MyServant ms@{base:s'@(Servant s)}) = mapSort $ MyServant ms
    { servant = Servant s
        { stats    = s.stats{ base = calcStats, max = calcStats }
        , phantasm = s.phantasm
              { effect = mapAmount calcNP <$> s.phantasm.effect
              , over   = case ms.level of
                             0 -> s.phantasm.over
                             _ -> mapAmount calcOver <$> s.phantasm.over
              }
        , skills  = zipWith calcActives ms.skills s.skills
        }
    }
  where
    calcStats = addStats ms.fou $ lvlStats s' ms.level
    calcNP minAmount maxAmount = Flat $ minAmount + (maxAmount - minAmount) *
        case ms.npLvl of
            1 -> 0.0
            2 -> 0.5
            3 -> 0.75
            4 -> 0.875
            _ -> 1.0
    calcOver minAmount maxAmount = case ms.npLvl of
        1 -> Flat minAmount
        _ -> Range minAmount $ minAmount
                            + (maxAmount - minAmount)
                            * (Int.toNumber ms.npLvl - 1.0)
                            / 4.0
    calcActives lvl skill = skill { effect = mapAmount calc <$> skill.effect
                                  , cd = skill.cd - (max 2 lvl - 2) / 4
                                  }
      where
        calc minAmount maxAmount = case lvl of
            10 -> Flat maxAmount
            _  -> Flat $ minAmount
                       + (maxAmount - minAmount)
                       * (Int.toNumber lvl - 1.0)
                       / 10.0

toSort :: SortBy -> Servant -> Number
toSort ID         (Servant s) = negate $ Int.toNumber s.id
toSort Rarity     (Servant s) = Int.toNumber s.rarity
toSort ATK        (Servant s) = Int.toNumber s.stats.max.atk
toSort HP         (Servant s) = Int.toNumber s.stats.max.hp
toSort StarWeight (Servant s) = Int.toNumber s.gen.starWeight
toSort NPArts              s  = npPer s Arts
toSort NPDeck              s  = sum $ npPer s <$> getDeck s
toSort StarQuick           s  = starsPer s Quick
toSort StarDeck            s  = sum $ starsPer s <$> getDeck s
toSort NPDmg               s  = npDamage false false s
toSort NPDmgOver           s  = npDamage false true s
toSort NPSpec              s  = npDamage true false s
toSort NPSpecOver          s  = npDamage true true s

mapSort :: MyServant -> MyServant
mapSort (MyServant ms) = MyServant ms { sorted = sorted }
  where
    sorted = Map.fromFoldable $
             (identity &&& flip toSort ms.servant) <$> enumArray

makeUnowned :: Servant -> MyServant
makeUnowned servant@(Servant s) = mapSort $ MyServant
    { servant
    , base:   servant
    , level:  0
    , fou:    {atk: 990, hp: 990}
    , skills: [10, 10, 10]
    , npLvl:  if s.free || (s.rarity <= 3 && s.rarity > 0) then 5 else 1
    , ascent: 1
    , sorted: Map.empty
    }

unowneds :: Map Servant MyServant
unowneds = Map.fromFoldable $ (identity &&& makeUnowned) <$> servants

unowned :: Servant -> MyServant
unowned s = fromMaybe' (\_ -> makeUnowned s) $ Map.lookup s unowneds

newServant :: Servant -> MyServant
newServant servant@(Servant s) = MyServant
    { servant
    , base:   servant
    , level:  1
    , fou:    {atk: 0, hp: 0}
    , skills: [1, 1, 1]
    , npLvl:  1
    , ascent: 1
    , sorted: Map.empty
    }

owned :: Map Servant MyServant -> Servant -> MyServant
owned team servant = fromMaybe (unowned servant) $
                     Map.lookup servant team
