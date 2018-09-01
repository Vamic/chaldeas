module Database
  ( module Database.Model
  , module Database.Calculator
  , module Database.CraftEssence
  , servants, getAll, getPassives
  )
  where


import Prelude (join, map, otherwise, show, unit, ($), (&&), (/=), (<$>), (<<<), (<>))
import Operators (enumArray)
import Data.Array (any, cons, filter, nub, sort, sortWith)
import Data.Function.Memoize (memoize)

import Database.Model
import Database.Calculator (npDamage, npPerArts, starsPerQuick)
import Database.CraftEssence (CraftEssence, craftEssences)
import Database.Servant.Archer (archers)
import Database.Servant.Assassin (assassins)
import Database.Servant.Berserker (berserkers)
import Database.Servant.Caster (casters)
import Database.Servant.Extra (extras)
import Database.Servant.Lancer (lancers)
import Database.Servant.Rider (riders)
import Database.Servant.Saber (sabers)

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
    addUniversal s@{traits, passives}
        = s { traits   = sortWith show $ cons Humanoid traits
            , passives = sortWith show passives
            }
    addHeavenOrEarth s@{attr, traits}
      | attr /= Earth && attr /= Heaven = s
      | otherwise = s{traits = cons HeavenOrEarth traits}

getAll ∷ ∀ a. MatchServant a => Array a
getAll = (_ $ unit) <<< memoize $ \_ -> sortWith show $ filter exists enumArray
  where
    exists eff = any (has eff false) servants

getPassives ∷ Array String
getPassives = sort <<< nub <<< join $ (map _.name <<< _.passives) <$> servants
