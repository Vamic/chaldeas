module Database exposing (servants, getAll, ceGetAll)

import List.Extra as List

import StandardLibrary       exposing (..)
import Database.Base         exposing (..)
import Database.Skill        exposing (..)
import Database.Servant      exposing (..)
import Database.CraftEssence exposing (..)
import Database.Has          exposing (..)

import Database.Servant.Archer    exposing (archers)
import Database.Servant.Assassin  exposing (assassins)
import Database.Servant.Berserker exposing (berserkers)
import Database.Servant.Caster    exposing (casters)
import Database.Servant.Extra     exposing (extras)
import Database.Servant.Lancer    exposing (lancers)
import Database.Servant.Rider     exposing (riders)
import Database.Servant.Saber     exposing (sabers)

servants : List Servant
servants = 
  let
    addHeavenOrEarth s = case s.attr of
      Earth  -> { s | traits = HeavenOrEarth :: s.traits }
      Heaven -> { s | traits = HeavenOrEarth :: s.traits }
      _      -> s
    addUniversal s = 
        { s 
        | traits   = List.sortBy showTrait <| Humanoid :: s.traits 
        , passives = List.sortBy .name s.passives
        }
  in
    [ archers
    , assassins
    , berserkers
    , casters
    , extras
    , lancers
    , riders
    , sabers
    ] 
    |> List.concat
    >> List.map (addHeavenOrEarth >> addUniversal)

genericGetAll : List a -> Has a b -> List b
genericGetAll xs {show, has} = 
    xs 
    |> List.concatMap (has False)
    >> List.sortBy show
    >> List.uniqueBy show
    
getAll : Has Servant a -> List a
getAll = genericGetAll servants

ceGetAll : Has CraftEssence a -> List a
ceGetAll = genericGetAll craftEssences
