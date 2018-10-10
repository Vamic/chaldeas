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
import Database.Has     exposing (..)
import Printing         exposing (..)
import Site.Algebra     exposing (..)
import Site.Common      exposing (..)
import Site.Filtering   exposing (..)
import Site.ToImage     exposing (..)

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
        getAll (has sEffects)
        |> List.filter pred
        >> List.map (matchFilter toImage (has sEffects) tab)
    all has toImage = 
        getAll has
        |> List.map (matchFilter toImage has tab)
  in case tab of
    FilterAlignment    -> all hasAlignment Nothing
    FilterAttribute    -> all hasAttribute Nothing
    FilterCard         -> all hasCard <| Just toImageCard
    FilterClass        -> all hasClass <| Just toImageClass
    FilterDeck         -> all hasDeck Nothing
    FilterPhantasm     -> all hasPhantasmType Nothing
    FilterTrait        -> all hasTrait Nothing
    FilterPassiveSkill -> all hasPassive << Just <| toImageIcon << .icon
    FilterMaterial     -> all hasMaterial <| Just toImageMaterial

    FilterDebuff -> allEffects hasDebuffEffect (Just toImageDebuffEffect) <| 
                    always True
    FilterBuff c -> allEffects hasBuffEffect (Just toImageBuffEffect) <|
                    buffCategory >> (==) c
    FilterAction -> allEffects hasInstantEffect Nothing <|
                    not << isDamage 
    FilterDamage -> allEffects hasInstantEffect Nothing <|
                    isDamage
    _            -> getExtraFilters today tab
