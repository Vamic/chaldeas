module Site.Servant.Filters exposing 
  ( getFilters
  , singleFilter
  )

import List.Extra  as List
import Maybe.Extra as Maybe
import Date exposing (Date)
import Time exposing (Month(..))

import StandardLibrary  exposing (..)
import Database         exposing (..)
import Database.Servant exposing (..)
import Database.Skill   exposing (..)
import Printing         exposing (..)
import Site.Algebra     exposing (..)
import Site.Base        exposing (..)
import Site.Common      exposing (..)
import Site.Filtering   exposing (..)

import Class.Has     as Has     exposing (Has)
import Class.ToImage as ToImage

extraFilters : List (Filter Servant)
extraFilters = List.concat
  [ [ namedBonus FilterAvailability "New"
      [ "Illyasviel von Einzbern"
      , "Chloe von Einzbern"
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
  [ ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterAvailability "Rate-Up"
    [ "Tamamo-no-Mae", "Carmilla", "Tamamo Cat", "Mephistopheles"
    , "Darius III" 
    ]
  , ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterAvailability "Limited to Event"
    [ "Elisabeth Bathory (Halloween)" ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 15) <|
    namedBonus FilterAvailability "Rate-Up"
    [ "Cleopatra", "Vlad III (EXTRA)" ]
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
