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

servants ∷ Array Servant
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
    addUniversal (Servant s@{traits, passives})
        = Servant s { traits   = sortWith show $ cons Humanoid traits
                    , passives = sortWith show passives
                    }
    addHeavenOrEarth s'@(Servant s@{attr, traits})
      | attr /= Earth && attr /= Heaven = s'
      | otherwise = Servant s {traits = cons HeavenOrEarth traits}

getAll ∷ ∀ a. MatchServant a => Array a
getAll = (_ $ unit) <<< memoize $ \_ -> sortWith show $ filter exists enumArray
  where
    exists eff = any (has eff false) servants

ceGetAll ∷ ∀ a. MatchCraftEssence a => Array a
ceGetAll = (_ $ unit) <<< memoize
         $ \_ -> sortWith show $ filter exists enumArray
  where
    exists eff = any (ceHas eff false) craftEssences

getPassives ∷ Array String
getPassives = sort <<< nub $ servants >>= \(Servant s) -> _.name <$> s.passives
