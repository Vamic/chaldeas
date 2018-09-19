-- | Packages together all information from the Database folder,
-- | including a collection of all the Servants in the
-- | [Database.Servant](./Servant/) folder.
module Database
  ( module Database.Model
  , module Database.Calculator
  , module Database.Has
  , servants, getAll, ceGetAll, getPassives
  ) where

import StandardLibrary

import Database.Model
import Database.Has
import Database.Calculator
import Database.Servant.Archer
import Database.Servant.Assassin
import Database.Servant.Berserker
import Database.Servant.Caster
import Database.Servant.Extra
import Database.Servant.Lancer
import Database.Servant.Rider
import Database.Servant.Saber

-- | All Servants available in EN. Collects the database in
-- | [Database/Servant](./Servant/).
-- Note: Names _must_ be true to their EN localization.
-- GrandOrder.Wiki is only trustworthy for Servants that have been in the game
-- for a while, especially for skills.
-- Servants introduced during events and the like should be checked against
-- the official announcement.
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
        { traits   = sortWith show $ Humanoid : s.traits
        , passives = sortWith show s.passives
        }
    addHeavenOrEarth s'@(Servant s)
      | s.attr /= Earth && s.attr /= Heaven = s'
      | otherwise = Servant s {traits = HeavenOrEarth : s.traits}

-- | Retrieves all values of a `Has <a>` Enum
-- | that at least one `<a>` in the database `has`.
genericGetAll :: ∀ a b f. Has a b => Foldable f => f a -> Array b
genericGetAll xs = sortWith show $ filter exists enumArray
  where
    exists eff = any (has eff false) xs

-- | Retrieves all values of a `Has Servant` Enum
-- | that at least one `Servant` in the database `has`.
-- | Memoized for performance.
getAll :: ∀ a. Has Servant a => Array a
getAll = flip memoize unit \_ -> genericGetAll servants

-- | Retrieves all values of a `Has CraftEssence` Enum
-- | that at least one `CraftEssence` in the database `has`.
-- | Memoized for performance.
ceGetAll :: ∀ a. Has CraftEssence a => Array a
ceGetAll = flip memoize unit \_ -> genericGetAll craftEssences

-- | Retrieves all passive skills defined in `Passive`
-- | that at least one `Servant` `has`.
-- | Memoized for performance.
getPassives :: Array Skill
getPassives = flip memoize unit \_ ->
              sortBy (comparing _.name) <<< nubBy (comparing _.name) $
              servants >>= \(Servant s) -> s.passives
