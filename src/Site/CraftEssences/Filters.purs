module Site.CraftEssences.Filters
  ( skillFilter
  , getFilters
  ) where

import StandardLibrary
import Data.String as String

import Data.Date(Date, Month(..))

import Site.Common
import Site.ToImage
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


matchFilter :: ∀ a. MatchCraftEssence a => FilterTab -> a -> Filter CraftEssence
matchFilter tab x =
    Filter { icon:  Nothing
           , tab
           , name:  show x
           , match: ceHas x
           }

imageFilter :: ∀ a. ToImage a => MatchCraftEssence a
            => FilterTab -> a -> Filter CraftEssence
imageFilter tab x =
    Filter { icon:  Just $ toImagePath x
           , tab
           , name:  show x
           , match: ceHas x
           }

namedBonus :: FilterTab -> String -> Array String -> Filter CraftEssence
namedBonus tab name names =
    Filter { icon: Nothing
           , tab
           , name
           , match: \_ (CraftEssence ce) -> ce.name `elem` names
           }

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

skillFilter :: SkillEffect -> Maybe (Filter CraftEssence)
skillFilter (Grant _ _ buff _)    = Just $ matchFilter
                                     (FilterBuff $ buffCategory buff) buff
skillFilter (Debuff _ _ debuff _) = Just $ matchFilter FilterDebuff debuff
skillFilter (To _ action _)       = Just $ matchFilter FilterAction action
skillFilter (Bonus bonus _)       = Just $ matchFilter FilterBonus bonus
skillFilter (Chance _ ef')        = skillFilter ef'
skillFilter (Chances _ _ ef')     = skillFilter ef'
skillFilter (When _ ef')          = skillFilter ef'
skillFilter (Times _ ef')         = skillFilter ef'
skillFilter (ToMax _ ef')         = skillFilter ef'
