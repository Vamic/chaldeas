module Persist.Preferences exposing
  ( Preference(..), enumPreference, ordPreference
  , Preferences, noPreferences
  , prefer
  , setPreference
  , unfoldPreferences
  )

import Dict exposing (Dict)
import Set exposing (Set)

import StandardLibrary exposing (..)

type Preference
    = Artorify
    | NightMode
    | Thumbnails
    | ShowTables
    | ExcludeSelf
    | MaxAscension
    | AddSkills
    | HideClasses

enumPreference : List Preference
enumPreference =
    [ Artorify
    , NightMode
    , Thumbnails
    , ShowTables
    , ExcludeSelf
    , MaxAscension
    , AddSkills
    , HideClasses
    ]

type alias OrdPreference = Int

ordPreference : Preference -> OrdPreference
ordPreference = enumToOrd enumPreference

prefDefault : Preference -> Bool
prefDefault a = case a of
  AddSkills -> True
  _         -> False

type alias Preferences = Set OrdPreference

noPreferences : Preferences
noPreferences =
  let
    acc pref = setPreference pref <| prefDefault pref
  in
    List.foldr acc Set.empty enumPreference

prefer : Preferences -> Preference -> Bool
prefer prefs = ordPreference >> flip Set.member prefs

setPreference : Preference -> Bool -> Preferences -> Preferences
setPreference pref a = case a of
  True  -> Set.insert <| ordPreference pref
  False -> Set.remove <| ordPreference pref

unfoldPreferences : Preferences -> List (Preference, Bool)
unfoldPreferences prefs = flip List.map enumPreference <| \pref ->
    (pref, prefer prefs pref)
