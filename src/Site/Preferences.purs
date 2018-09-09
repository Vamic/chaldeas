module Site.Preferences
    ( Preference(..)
    , Preferences
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
import Data.Map (Map, fromFoldable, lookup, toUnfoldableUnordered)
import Data.Maybe
import Data.Profunctor.Strong
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable
import Data.Tuple (Tuple(..), snd)
import Effect
import Web.Storage.Storage
import Web.HTML
import Web.HTML.Window

import Database
import Database.MyServant

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

delimTeam ∷ String
delimTeam = "~"
delimServant ∷ String
delimServant = "`"
delimSkills ∷ String
delimSkills = "|"

writeServant ∷ MyServant -> String
writeServant (MyServant m@{servant:(Servant s)}) = joinWith delimServant
    [ s.name
    , show m.level
    , show m.fou.atk
    , show m.fou.hp
    , joinWith delimSkills $ show <$> m.skills
    , show m.npLvl
    , show m.ascent 
    ]

readServant ∷ String -> Maybe MyServant
readServant text = do
    servantName <- text' !! 0
    showLevel   <- text' !! 1
    showFouAtk  <- text' !! 2
    showFouHp   <- text' !! 3
    showSkills  <- text' !! 4
    showNpLvl   <- text' !! 5
    showAscent  <- text' !! 6
    servant     <- find (eq servantName <<< show) servants
    level       <- fromString showLevel
    atk         <- fromString showFouAtk
    hp          <- fromString showFouHp
    skills      <- traverse fromString $ split (Pattern delimSkills) showSkills
    npLvl       <- fromString showNpLvl
    ascent      <- fromString showAscent
    let fou      = {atk, hp}
        base     = servant
    pure <<< recalc 
           $ MyServant { servant, level, fou, skills, npLvl, base, ascent }
  where text' = split (Pattern delimServant) text

setTeam ∷ Map Servant MyServant -> Effect Unit
setTeam team = window >>= localStorage >>= setItem "team" 
               (joinWith delimTeam $ writeServant <<< snd <$> toUnfoldableUnordered team)

getTeam ∷ Effect (Map Servant MyServant)
getTeam = window >>= localStorage >>= getItem "team" >>= pure 
          <<< fromFoldable <<< map (getBase &&& identity) <<< fromMaybe [] 
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
