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
      [ "Hydra Dagger"
      , "Fragarach"
      , "Inverted Moon of the Heavens"
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
  [ ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterEventBonus "+1~2 Pumpkin Lantern"
    [ "Little Halloween Devil" ]
  , ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterEventBonus "+2~3 Mischievous Bat"
    [ "Maid in Halloween" ]
  , ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterEventBonus "+1~2 Mischievous Bat"
    [ "Halloween Arrangement" ]
  , ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterEventBonus "+2~3 Sweet Candle"
    [ "Halloween Princess" ]
  , ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterEventBonus "+1~2 Sweet Candle"
    [ "Jack-o'-lantern" ]
  , ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterEventBonus "+3~4 Petit Cake"
    [ "Trick or Treat" ]
  , ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterAvailability "Rate-Up"
    [ "Little Halloween Devil", "Halloween Arrangement", "Jack-o'-lantern" ]
  , ScheduledFilter (Date 2018 Oct 4) (Date 2018 Oct 17) <|
    namedBonus FilterAvailability "Limited to Event"
    [ "Maid in Halloween", "Halloween Princess", "Trick or Treat" 
    , "Little Halloween Devil", "Halloween Arrangement", "Jack-o'-lantern"  
    ]
  {-, ScheduledFilter (Date 2018 Oct 18) (Date 2018 Oct 24) <|
    namedBonus FilterAvailability "Rate-Up"
    [ "Hydra Dagger", "Fragarach", "Inverted Moon of the Heavens" ]-}
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
