module Database
  ( module Database.Model
  , servants, getAll
  )
  where

import Operators
import Database

import Prelude
import Data.Array (any, cons, filter)

import Database.Model
import Database.Servant.Archer 
import Database.Servant.Assassin
import Database.Servant.Berserker
import Database.Servant.Caster
import Database.Servant.Extra
import Database.Servant.Lancer
import Database.Servant.Rider
import Database.Servant.Saber

servants ∷ Array Servant
servants = addUniversal ∘ addHeavenOrEarth
         ↤ archers
         ⧺ assassins
         ⧺ berserkers
         ⧺ casters
         ⧺ extra
         ⧺ lancers
         ⧺ riders
         ⧺ sabers    
  where
    addUniversal (Servant s@{traits}) = Servant s{traits = cons Humanoid traits}
    addHeavenOrEarth serv@(Servant s@{attr, traits})
      | attr ≡ Earth ∨ attr ≡ Earth = Servant s{traits = cons HeavenOrEarth traits}
      | otherwise                   = serv
getAll ∷ ∀ a. MatchServant a ⇒ Array a
getAll = filter exists enumArray
  where 
    exists eff = any (has eff) servants
