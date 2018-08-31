module Site.Preferences 
    ( Preference(..)
    , Preferences
    , setPreference
    , getPreference
    , getPreferences
    ) where

import Data.Enum
import Data.Generic.Rep
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum
import Data.Generic.Rep.Show
import Data.Map 
import Data.Maybe
import Data.Traversable
import Data.Tuple (Tuple(..))
import Effect
import Operators
import Prelude
import Web.Storage.Storage

import Web.HTML (window)
import Web.HTML.Window (localStorage)

data Preference = Artorify
                | ShowTables
                | ExcludeSelf
                | MaxAscension

instance _a_ ∷ Show Preference where
  show ExcludeSelf = "Exclude self-applied effects"
  show MaxAscension = "Show all at max ascension"
  show ShowTables = "Show skill and NP tables"
  show a = genericShow a

setPreference ∷ Preference -> Boolean -> Effect Unit
setPreference pref set = window >>= localStorage
                     >>= setItem (genericShow pref) (show set)
                 
type Preferences = Map Preference Boolean

getPreference ∷ Preferences -> Preference -> Boolean
getPreference prefs pref = fromMaybe false $ lookup pref prefs

getPreferences ∷ Effect Preferences
getPreferences = fromFoldable <$> traverse go enumArray 
  where
    fromFlag (Just "true") = true
    fromFlag _             = false
    go k = do
        v <- window >>= localStorage >>= getItem (genericShow k) 
                    >>= pure ∘ fromFlag
        pure $ Tuple k v

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Generic Preference _
derive instance _1_ ∷ Eq Preference
derive instance _2_ ∷ Ord Preference
instance _3_ ∷ Enum Preference where
  succ = genericSucc
  pred = genericPred
instance _4_ ∷ Bounded Preference where
  top = genericTop
  bottom = genericBottom
instance _5_ ∷ BoundedEnum Preference where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
