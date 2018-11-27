module Site.Servant.Filters exposing
  ( getFilters
  , singleFilter
  )

import List.Extra  as List
import Date exposing (Date)
import Time exposing (Month(..))

import StandardLibrary  exposing (..)
import Database         exposing (..)
import Database.Servant exposing (..)
import Database.Skill   exposing (..)
import Printing         exposing (..)
import Site.Algebra     exposing (..)
import Site.Base        exposing (..)
import Site.Filtering   exposing (..)

import Class.Has     as Has     exposing (Has)
import Class.ToImage as ToImage

extraFilters : List (Filter Servant)
extraFilters = List.concat
  [ [ nameFilter FilterAvailability "New"
      [ "Ishtar"
      , "Jeanne d'Arc Alter Santa Lily"
      ]
    , Filter Nothing FilterAvailability "Free" <|
      \_ s -> s.free
    , Filter Nothing FilterSource "Limited" <|
      \_ s -> s.limited
    , Filter Nothing FilterSource "Non-Limited" <|
      \_ s -> not s.limited
    ]
  , flip List.map (List.range 1 5) <| \rarity ->
    Filter Nothing FilterRarity (stars False rarity) <|
    \_ s -> rarity == s.rarity
  ]

scheduledFilters : List (ScheduledFilter Servant)
scheduledFilters =
  [ ScheduledFilter (Date 2018 Nov 27) (Date 2018 Dec 6) <|
    nameFilter FilterAvailability "Rate-Up"
    [ "Ishtar" ]
  , ScheduledFilter (Date 2018 Nov 27) (Date 2018 Dec 6) <|
    nameFilter FilterEventBonus "+1 Fruitcake"
    [ "Amakusa Shirou", "Ishtar", "Jeanne d'Arc Alter Santa Lily" ]
  , ScheduledFilter (Date 2018 Nov 27) (Date 2018 Dec 6) <|
    nameFilter FilterEventBonus "+1 Shortcake"
    [ "EMIYA", "Jeanne d'Arc", "Jeanne d'Arc (Alter)" ]
  , ScheduledFilter (Date 2018 Nov 27) (Date 2018 Dec 6) <|
    nameFilter FilterEventBonus "+1 Cheesecake"
    [ "Mash Kyrielight", "Ushiwakamaru", "Gilles de Rais (Caster)" 
    , "Gilgamesh (Child)", "Fuuma \"Evil-wind\" Kotarou", "Tawara Touta"
    , "Leonidas I", "Hassan of the Cursed Arm", "Asterios", "Mata Hari"
    ]
  ]

singleFilter : Has Servant a -> FilterTab -> a -> List (Filter Servant)
singleFilter has tab x =
  if exclusive tab then
    getAll has |> List.remove x >> List.map (matchFilter Nothing has tab)
  else
    [matchFilter Nothing has tab x]

getExtraFilters : Date -> FilterTab -> List (Filter Servant)
getExtraFilters today tab =
    getScheduled scheduledFilters today ++ extraFilters
    |> List.filter (.tab >> (==) tab)

getFilters : Date -> FilterTab -> List (Filter Servant)
getFilters today tab =
  let
    allEffects has toImage pred =
        getAll (has Has.servant)
        |> List.filter pred
        >> List.map (matchFilter toImage (has Has.servant) tab)
    all has toImage =
        getAll has
        |> List.map (matchFilter toImage has tab)
  in case tab of
    FilterAlignment    -> all Has.alignment Nothing
    FilterAttribute    -> all Has.attribute Nothing
    FilterCard         -> all Has.card <| Just ToImage.card
    FilterClass        -> all Has.class <| Just ToImage.class
    FilterDeck         -> all Has.deck Nothing
    FilterPhantasm     -> all Has.phantasmType Nothing
    FilterTrait        -> all Has.trait Nothing
    FilterPassiveSkill -> all Has.passive << Just <| ToImage.icon << .icon
    FilterMaterial     -> all Has.material <| Just ToImage.material

    FilterDebuff -> allEffects Has.debuffEffect (Just ToImage.debuffEffect) <|
                    always True
    FilterBuff c -> allEffects Has.buffEffect (Just ToImage.buffEffect) <|
                    buffCategory >> (==) c
    FilterAction -> allEffects Has.instantEffect Nothing <|
                    not << isDamage
    FilterDamage -> allEffects Has.instantEffect Nothing <|
                    isDamage
    _            -> getExtraFilters today tab
