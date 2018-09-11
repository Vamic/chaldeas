module Database
  ( module Database.Model
  , module Database.Calculator
  , servants, getAll, ceGetAll, getPassives
  ) where

import Prelude
import Operators
import Data.Array
import Data.Function.Memoize

import Database.Model
import Database.Calculator
import Database.Servant.Archer
import Database.Servant.Assassin
import Database.Servant.Berserker
import Database.Servant.Caster
import Database.Servant.Extra
import Database.Servant.Lancer
import Database.Servant.Rider
import Database.Servant.Saber

servants :: Array Servant
servants = addUniversal <<< addHeavenOrEarth
       <$> archers
        <> assassins
        <> berserkers
        <> casters
        <> extras
        <> lancers
        <> riders
        <> sabers
  where
    addUniversal (Servant s) = Servant s 
        { traits   = sortWith show $ cons Humanoid s.traits
        , passives = sortWith show s.passives
        }
    addHeavenOrEarth s'@(Servant s)
      | s.attr /= Earth && s.attr /= Heaven = s'
      | otherwise = Servant s {traits = cons HeavenOrEarth s.traits}

getAll :: ∀ a. MatchServant a => Array a
getAll = flip memoize unit \_ -> sortWith show $ filter exists enumArray
  where
    exists eff = any (has eff false) servants

ceGetAll :: ∀ a. MatchCraftEssence a => Array a
ceGetAll = flip memoize unit \_ -> sortWith show $ filter exists enumArray
  where
    exists eff = any (ceHas eff false) craftEssences

getPassives :: Array String
getPassives = sort <<< nub $ servants >>= \(Servant s) -> _.name <$> s.passives
