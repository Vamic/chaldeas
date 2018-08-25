module Database
  ( module Database.Model
  , servants, getAll, getPassives
  )
  where

import Operators

import Prelude
import Data.Array
import Data.Function.Memoize

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
         ⧺ extras
         ⧺ lancers
         ⧺ riders
         ⧺ sabers    
  where
    addUniversal (Servant s@{traits}) = Servant s{traits = cons Humanoid traits}
    addHeavenOrEarth serv@(Servant s@{attr, traits})
      | attr ≡ Earth ∨ attr ≡ Earth = Servant s{traits = cons HeavenOrEarth traits}
      | otherwise                   = serv

getAll ∷ ∀ a. MatchServant a ⇒ Array a
getAll = (_ $ unit) ∘ memoize $ \_ → sortWith show $ filter exists enumArray
  where
    exists eff = any (has eff) servants 

getPassives ∷ Array String
getPassives = sort ∘ nub ∘ concat $ getPassive ↤ servants
  where
    getPassive (Servant {passives}) = (_.name) ↤ passives
