module Site.Preferences
    ( Preference(..)
    , Preferences
    , MyServant(..)
    , setPreference
    , getPreference
    , getPreferences
    , getTeam
    , setTeam
    ) where

import Prelude
import Operators
import Generic as G

import Data.Array ((!!))
import Data.Int (fromString)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable
import Data.Tuple (Tuple(..))
import Effect
import Web.Storage.Storage
import Web.HTML
import Web.HTML.Window

import Database

data Preference
    = Artorify
    | NightMode
    | ShowTables
    | ExcludeSelf
    | MaxAscension
    | Thumbnails

instance _a_ ∷ Show Preference where
  show ExcludeSelf = "Exclude self-applied effects"
  show MaxAscension = "Show all at max ascension"
  show ShowTables = "Show skill and NP tables"
  show a = unCamel $ G.genericShow a

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

newtype MyServant = MyServant { servant ∷ Servant 
                              , level   ∷ Int
                              , fou     ∷ Stat
                              , skills  ∷ Array Int
                              , npLvl   ∷ Int
                              }
getServant ∷ MyServant -> Servant
getServant (MyServant {servant}) = servant

delimTeam ∷ String
delimTeam = "~"
delimServant ∷ String
delimServant = "`"
delimSkills ∷ String
delimSkills = "|"

writeServant ∷ MyServant -> String
writeServant (MyServant m@{servant:(Servant s)}) = joinWith delimServant
    [ s.name, show m.level, show m.fou.atk, show m.fou.hp, skil, show m.npLvl ]
  where
    skil = joinWith delimSkills $ show <$> m.skills


readServant ∷ String -> Maybe MyServant
readServant text = do
    servantName <- text' !! 0
    showLevel   <- text' !! 1
    showFouAtk  <- text' !! 2
    showFouHp   <- text' !! 3
    showSkills  <- text' !! 4
    showNpLvl   <- text' !! 5
    servant     <- find (eq servantName <<< show) servants
    level       <- fromString showLevel
    atk         <- fromString showFouAtk
    hp          <- fromString showFouHp
    skills      <- traverse fromString $ split (Pattern delimSkills) showSkills
    npLvl       <- fromString showNpLvl
    pure $ MyServant { servant, level, fou: {atk, hp}, skills, npLvl }
  where text' = split (Pattern delimServant) text

setTeam ∷ Array MyServant -> Effect Unit
setTeam team = window >>= localStorage >>= setItem "team" 
               (joinWith delimTeam $ writeServant <$> team)

getTeam ∷ Effect (Array MyServant)
getTeam = window >>= localStorage >>= getItem "team" >>= pure <<< fromMaybe [] 
          <<< (_ >>= traverse readServant <<< split (Pattern delimTeam))

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
