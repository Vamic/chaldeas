module Database
  ( module Database.Model
  , module Database.Servant.Archer
  , module Database.Servant.Assassin
  , module Database.Servant.Avenger
  , module Database.Servant.Berserker
  , module Database.Servant.Caster
  , module Database.Servant.Lancer
  , module Database.Servant.Rider
  , module Database.Servant.Ruler
  , module Database.Servant.Saber
  , servants, getAll
  )
  where

import Prelude
import Operators
import Database

import Data.Array (any, filter, sortWith)

import Database.Model
import Database.Servant.Archer 
import Database.Servant.Assassin
import Database.Servant.Avenger
import Database.Servant.Berserker
import Database.Servant.Caster
import Database.Servant.Lancer
import Database.Servant.Rider
import Database.Servant.Ruler
import Database.Servant.Saber

servants ∷ Array Servant
servants = sortWith sorter
         $ archers
         ⧺ assassins
         ⧺ avengers
         ⧺ berserkers
         ⧺ casters
         ⧺ lancers
         ⧺ riders
         ⧺ rulers
         ⧺ sabers
  where 
    sorter (Servant s) = show (5 - s.rarity) ⧺ show s.class ⧺ show s.name

getAll ∷ ∀ a. ToActive a ⇒ Array a
getAll = filter exists enumArray
  where 
    exists eff = any (hasActive $ toActive eff) servants
