module Site.Preferences
    ( Preference(..)
    , Preferences
    , setPreference
    , getPreference
    , getPreferences
    ) where

import Prelude
import Operators
import Generic as G

import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe
import Data.Traversable
import Data.Tuple (Tuple(..))
import Effect
import Web.Storage.Storage
import Web.HTML
import Web.HTML.Window

data Preference
    = Artorify
    | ShowTables
    | ExcludeSelf
    | MaxAscension

instance _a_ ∷ Show Preference where
  show ExcludeSelf = "Exclude self-applied effects"
  show MaxAscension = "Show all at max ascension"
  show ShowTables = "Show skill and NP tables"
  show a = G.genericShow a

setPreference ∷ Preference -> Boolean -> Effect Unit
setPreference pref set = window >>= localStorage
                     >>= setItem (G.genericShow pref) (show set)

type Preferences = Map Preference Boolean

getPreference ∷ Preferences -> Preference -> Boolean
getPreference prefs pref = fromMaybe false $ lookup pref prefs

getPreferences ∷ Effect Preferences
getPreferences = fromFoldable <$> traverse go enumArray
  where
    fromFlag (Just "true") = true
    fromFlag _             = false
    go k = do
        v <- window >>= localStorage >>= getItem (G.genericShow k)
                    >>= pure <<< fromFlag
        pure $ Tuple k v

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ G.Generic Preference _
derive instance _1_ ∷ Eq Preference
derive instance _2_ ∷ Ord Preference
instance _3_ ∷ G.Enum Preference where
  succ = G.genericSucc
  pred = G.genericPred
instance _4_ ∷ G.Bounded Preference where
  top = G.genericTop
  bottom = G.genericBottom
instance _5_ ∷ G.BoundedEnum Preference where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum
