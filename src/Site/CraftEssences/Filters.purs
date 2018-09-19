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
  [ ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterAvailability "Limited to Event"
    [ "Glory Is With Me", "Original Legion", "Howl at the Moon"
    , "Joint Recital", "Princess of the White Rose"
    ]
  , ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterEventBonus "+1~2 Crimson Petal"
    [ "Joint Recital" ]
  , ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterEventBonus "+100~200% Attack"
    [ "Princess of the White Rose" ]
  , ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterEventBonus "+1~2 Gold Medal"
    [ "Glory Is With Me" ]
  , ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterEventBonus "+1~2 Silver Medal"
    [ "Original Legion" ]
  , ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterEventBonus "+1~2 Bronze Medal"
    [ "Howl at the Moon" ]
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
getFilters _ f@FilterAction   = matchFilter f <$> filter (not <<< isDamage) ceGetAll :: Array InstantEffect
getFilters _ f@FilterDamage   = matchFilter f <$> filter isDamage
                                getAll :: Array InstantEffect
getFilters today f = getExtraFilters today f
