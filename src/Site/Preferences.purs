-- | Persistent user settings. Uses local
-- | [Web Storage](https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API).
module Site.Preferences
    ( Preference(..)
    , Preferences
    , writePreference
    , prefer
    , getPreferences
    , setPreference
    , unfoldPreferences
    , getTeam
    , setTeam
    ) where

import StandardLibrary
import Generic             as G
import Web.HTML            as HTML
import Data.Int            as Int
import Data.Map            as Map
import Data.Set            as Set
import Data.String         as String
import Web.Storage.Storage as Storage
import Web.HTML.Window     as Window

import Data.Profunctor.Strong ((&&&))

import Data.Set (Set)
import Printing
import Database
import MyServant

data Preference
    = Artorify
    | NightMode
    | ShowTables
    | ExcludeSelf
    | MaxAscension
    | AddSkills
    | Thumbnails

instance _a_ :: Show Preference where
    show ExcludeSelf  = "Exclude self-applied effects"
    show MaxAscension = "Show all at max ascension"
    show ShowTables   = "Show skill and NP tables"
    show AddSkills    = "Add skills to NP damage"
    show x            = unCamel $ G.genericShow x

prefDefault :: Preference -> Boolean
prefDefault AddSkills = true
prefDefault _         = false

type Preferences = Set Preference

unfoldPreferences :: Preferences -> Array (Preference : Boolean)
unfoldPreferences prefs = (identity &&& prefer prefs) <$> enumArray

prefer :: Preferences -> Preference -> Boolean
prefer = flip elem

setPreference :: Preference -> Boolean -> Preferences -> Preferences
setPreference pref false = Set.delete pref
setPreference pref true  = Set.insert pref

writePreference :: Preference -> Boolean -> Effect Unit
writePreference pref set = HTML.window >>= Window.localStorage
                     >>= Storage.setItem (G.genericShow pref) (show set)

getPreferences :: Effect Preferences
getPreferences = Set.unions <$> traverse go enumArray
  where
    fromFlag Nothing x
      | prefDefault x        = Set.singleton x
      | otherwise            = Set.empty
    fromFlag (Just "true") x = Set.singleton x
    fromFlag _             _ = Set.empty
    go k = map (_ $ k) $
           HTML.window >>= Window.localStorage >>=
           Storage.getItem (G.genericShow k) >>= pure <<< fromFlag

delimTeam :: String
delimTeam = "~"
delimServant :: String
delimServant = "`"
delimSkills :: String
delimSkills = "|"

writeServant :: MyServant -> String
writeServant (MyServant m@{servant:(Servant s)}) = String.joinWith delimServant
    [ s.name
    , show m.level
    , show m.fou.atk
    , show m.fou.hp
    , String.joinWith delimSkills $ show <$> m.skills
    , show m.npLvl
    , show m.ascent
    ]

readServant :: String -> Maybe MyServant
readServant text = do
    servantName <- text' !! 0
    showLevel   <- text' !! 1
    showFouAtk  <- text' !! 2
    showFouHp   <- text' !! 3
    showSkills  <- text' !! 4
    showNpLvl   <- text' !! 5
    showAscent  <- text' !! 6
    servant     <- find (eq servantName <<< show) servants
    level       <- Int.fromString showLevel
    atk         <- Int.fromString showFouAtk
    hp          <- Int.fromString showFouHp
    skills      <- traverse Int.fromString $
                   String.split (Pattern delimSkills) showSkills
    npLvl       <- Int.fromString showNpLvl
    ascent      <- Int.fromString showAscent
    let fou      = {atk, hp}
        base     = servant
        sorted   = Map.empty
    pure <<< recalc $
    MyServant { servant, level, fou, skills, npLvl, base, ascent, sorted }
  where text' = String.split (Pattern delimServant) text

setTeam :: Map Servant MyServant -> Effect Unit
setTeam team = HTML.window >>= Window.localStorage >>=
               Storage.setItem "team" showTeam
  where
    showTeam = String.joinWith delimTeam $
               writeServant <<< snd <$> Map.toUnfoldableUnordered team

getTeam :: Effect (Map Servant MyServant)
getTeam = HTML.window >>= Window.localStorage >>= Storage.getItem "team" >>=
          pure <<< Map.fromFoldable <<< map (getBase &&& identity) <<<
          mapMaybe readServant <<< String.split (Pattern delimTeam) <<<
          fromMaybe ""

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ :: G.Generic Preference _
derive instance _1_ :: Eq Preference
derive instance _2_ :: Ord Preference
instance _3_ :: G.Enum Preference where
    succ = G.genericSucc
    pred = G.genericPred
instance _4_ :: G.Bounded Preference where
    top = G.genericTop
    bottom = G.genericBottom
instance _5_ :: G.BoundedEnum Preference where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum
