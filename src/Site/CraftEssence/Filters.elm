module Site.CraftEssence.Filters exposing (getFilters)

import Maybe.Extra as Maybe
import Date exposing (Date)
import Time exposing (Month(..))

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.CraftEssence exposing (..)
import Database.Skill        exposing (..)
import Printing              exposing (..)
import Site.Algebra          exposing (..)
import Site.Base             exposing (..)
import Site.Common           exposing (..)
import Site.Filtering        exposing (..)

import Class.Has     as Has
import Class.ToImage as ToImage

extraFilters : List (Filter CraftEssence)
extraFilters = List.concat
  [ [ namedBonus FilterAvailability "New"
      [ "Wizard & Priest"
      , "Mata Hari's Tavern"
      , "Hero Elly's Adventure"
      , "Count Romani Archaman's Hospitality"
      ]
    , Filter Nothing FilterSource "Limited" <|
      \_ ce -> ce.limited && Maybe.isNothing ce.bond
    , Filter Nothing FilterSource "Non-Limited" <|
      \_ ce -> not ce.limited && Maybe.isNothing ce.bond
    , Filter Nothing FilterSource "Bond" <|
      \_ ce -> Maybe.isJust ce.bond
    ]
  , flip List.map (List.range 1 5) <| \rarity ->
    Filter Nothing FilterRarity (stars False rarity) <|
    \_ ce -> rarity == ce.rarity
  ]

scheduledFilters : List (ScheduledFilter CraftEssence)
scheduledFilters =
  [ ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    namedBonus FilterEventBonus 
    "+25~100% [Skeleton], [Ghost], & [Lamia] spawn"
    [ "Wizard & Priest" ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    namedBonus FilterEventBonus 
    "+25~100% [Insect] & [Golem] spawn"
    [ "Mata Hari's Tavern" ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    namedBonus FilterEventBonus 
    "+25~100% [Beast], [Homunculus], & [Knight] spawn"
    [ "Hero Elly's Adventure" ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    namedBonus FilterEventBonus "+1~2 Copper Sack"
    [ "Count Romani Archaman's Hospitality" ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    namedBonus FilterAvailability "Rate-Up"
    [ "Dangerous Beast", "Witch Under the Moonlight"
    , "Count Romani Archaman's Hospitality"
    ]
  , ScheduledFilter (Date 2018 Oct 25) (Date 2018 Nov 8) <|
    namedBonus FilterAvailability "Limited to Event"
    [ "Wizard & Priest", "Mata Hari's Tavern", "Hero Elly's Adventure"
    , "Count Romani Archaman's Hospitality"
    ]
  ]

getExtraFilters : Date -> FilterTab -> List (Filter CraftEssence)
getExtraFilters today tab = 
    getScheduled scheduledFilters today ++ extraFilters
    |> List.filter (.tab >> (==) tab)

getFilters : Date -> FilterTab -> List (Filter CraftEssence)
getFilters today tab = 
  let
    allEffects has toImage pred = 
        ceGetAll (has .effect)
        |> List.filter pred
        >> List.map (matchFilter toImage (has .effect) tab)
  in case tab of
    FilterBonus  -> allEffects Has.bonusEffect Nothing <|
                    always True
    FilterDebuff -> allEffects Has.debuffEffect (Just ToImage.debuffEffect) <| 
                    always True
    FilterBuff c -> allEffects Has.buffEffect (Just ToImage.buffEffect) <|
                    buffCategory >> (==) c
    FilterAction -> allEffects Has.instantEffect Nothing <|
                    not << isDamage 
    FilterDamage -> allEffects Has.instantEffect Nothing <|
                    isDamage
    _            -> getExtraFilters today tab
