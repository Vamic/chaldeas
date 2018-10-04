module Site.CraftEssences.Filters (getFilters) where

import StandardLibrary
import Data.String as String

import Data.Date(Date, Month(..))

import Site.Algebra
import Site.Common
import Site.Filtering
import Database

extraFilters :: Array (Filter CraftEssence)
extraFilters = join
  [ [ simpleFilter FilterSource "Limited"
      \_ (CraftEssence ce) -> ce.limited && isNothing ce.bond
    , simpleFilter FilterSource "Non-Limited"
      \_ (CraftEssence ce) -> not ce.limited && isNothing ce.bond
    , simpleFilter FilterSource "Bond"
      \_ (CraftEssence ce) -> isJust ce.bond
    ]
  , reverse (1..5) <#> \rarity
    -> simpleFilter FilterRarity (String.joinWith "" $ replicate rarity "★")
    \_ (CraftEssence ce) -> rarity == ce.rarity
  ]

scheduledFilters :: Array (ScheduledFilter CraftEssence)
scheduledFilters =
  [ ScheduledFilter (ymd 2018 October 4) (ymd 2018 October 17) $
    namedBonus FilterEventBonus "+1~2 Pumpkin Lantern"
    [ "Little Halloween Devil" ]
  , ScheduledFilter (ymd 2018 October 4) (ymd 2018 October 17) $
    namedBonus FilterEventBonus "+2~3 Mischievous Bat"
    [ "Maid in Halloween" ]
  , ScheduledFilter (ymd 2018 October 4) (ymd 2018 October 17) $
    namedBonus FilterEventBonus "+1~2 Mischievous Bat"
    [ "Halloween Arrangement" ]
  , ScheduledFilter (ymd 2018 October 4) (ymd 2018 October 17) $
    namedBonus FilterEventBonus "+2~3 Sweet Candle"
    [ "Halloween Princess" ]
  , ScheduledFilter (ymd 2018 October 4) (ymd 2018 October 17) $
    namedBonus FilterEventBonus "+1~2 Sweet Candle"
    [ "Jack-o'-lantern" ]
  , ScheduledFilter (ymd 2018 October 4) (ymd 2018 October 17) $
    namedBonus FilterEventBonus "+3~4 Petit Cake"
    [ "Trick or Treat" ]
  , ScheduledFilter (ymd 2018 October 4) (ymd 2018 October 17) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Little Halloween Devil", "Halloween Arrangement", "Jack-o'-lantern" ]
  , ScheduledFilter (ymd 2018 October 4) (ymd 2018 October 17) $
    namedBonus FilterAvailability "Limited to Event"
    [ "Maid in Halloween", "Halloween Princess", "Trick or Treat" 
    , "Little Halloween Devil", "Halloween Arrangement", "Jack-o'-lantern"  
    ]
  ]

getExtraFilters :: Date -> FilterTab -> Array (Filter CraftEssence)
getExtraFilters today tab =
    filter fromTab $ getScheduled scheduledFilters today <> extraFilters
  where
    fromTab (Filter x) = tab == x.tab

getFilters :: Date -> FilterTab -> Array (Filter CraftEssence)
getFilters _ f@FilterBonus    = matchFilter f
                                <$> ceGetAll :: Array BonusEffect
getFilters _ f@FilterDebuff   = imageFilter f
                                <$> ceGetAll :: Array DebuffEffect
getFilters _ f@(FilterBuff c) = imageFilter f <$> filter (eq c <<< buffCategory)
                                    ceGetAll :: Array BuffEffect
getFilters _ f@FilterAction   = matchFilter f <$> filter (not <<< isDamage)
                                    ceGetAll :: Array InstantEffect
getFilters _ f@FilterDamage   = matchFilter f <$> filter isDamage
                                    ceGetAll :: Array InstantEffect
getFilters today f = getExtraFilters today f
