module Site.Preferences ( Preference(..), setPreference, getPreferences) where

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
                | ExcludeSelf
                | MaxAscension

instance _a_ ∷ Show Preference where
  show ExcludeSelf = "Exclude self-applied effects"
  show MaxAscension = "Show all at max ascension"
  show a = genericShow a

setPreference ∷ Preference -> Boolean -> Effect Unit
setPreference pref set = window >>= localStorage
                     >>= setItem (show pref) (show set)
                 
getPreferences ∷ Effect (Map Preference Boolean)
getPreferences = fromFoldable <$> traverse go enumArray 
  where
    go k = do
      v <- getPreference k
      pure $ Tuple k v
          
getPreference ∷ Preference -> Effect Boolean
getPreference pref = window >>= localStorage 
                 >>= getItem (show pref) 
                 >>= pure ∘ fromFlag
  where 
    fromFlag (Just "true") = true
    fromFlag _             = false

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
