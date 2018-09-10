module Site.CraftEssences.Filters
  ( activeFilter
  , getFilters
  ) where

import Prelude
import Data.String as S

import Data.Array
import Data.Date
import Data.Foldable
import Data.Maybe
import Data.Profunctor.Strong
import Data.Tuple

import Site.Filtering
import Database

extraFilters :: Array (Filter CraftEssence)
extraFilters = join
  [ [ Filter FilterSource "Limited"     
      \_ (CraftEssence ce) -> ce.limited && isNothing ce.bond
    , Filter FilterSource "Non-Limited" 
      \_ (CraftEssence ce) -> not ce.limited && isNothing ce.bond
    , Filter FilterSource "Bond"        
      \_ (CraftEssence ce) -> isJust ce.bond
    ]
  , reverse (1..5) <#> \rarity
    -> Filter FilterRarity (S.joinWith "" $ replicate rarity "★")
    \_ (CraftEssence ce) -> rarity == ce.rarity
  ]

scheduledFilters :: Array (ScheduledFilter CraftEssence)
scheduledFilters = []

matchFilter :: ∀ a. MatchCraftEssence a => FilterTab -> a -> Filter CraftEssence
matchFilter tab = uncurry (Filter tab) <<< (show &&& ceHas)

namedBonus :: FilterTab -> String -> Array String -> Filter CraftEssence
namedBonus tab bonus craftEssences = 
    Filter tab bonus \_ (CraftEssence ce) -> ce.name `elem` craftEssences

getExtraFilters :: Date -> FilterTab -> Array (Filter CraftEssence)
getExtraFilters today tab = 
    filter fromTab $ getScheduled scheduledFilters today <> extraFilters
  where
    fromTab (Filter t _ _) = tab == t

getFilters :: Date -> FilterTab -> Array (Filter CraftEssence)
getFilters _ f@FilterBonus    = matchFilter f
                                <$> ceGetAll :: Array BonusEffect
getFilters _ f@FilterDebuff   = matchFilter f 
                                <$> ceGetAll :: Array DebuffEffect
getFilters _ f@(FilterBuff c) = matchFilter f <$> filter (eq c <<< buffCategory) 
                                ceGetAll :: Array BuffEffect
getFilters _ f@FilterAction   = matchFilter f <$> filter (not <<< isDamage) 
                                ceGetAll :: Array InstantEffect
getFilters _ f@FilterDamage   = matchFilter f <$> filter isDamage
                                getAll :: Array InstantEffect
getFilters today f = getExtraFilters today f

activeFilter :: ActiveEffect -> Maybe (Filter CraftEssence)
activeFilter (Grant _ _ buff _)    = Just $ matchFilter 
                                     (FilterBuff $ buffCategory buff) buff
activeFilter (Debuff _ _ debuff _) = Just $ matchFilter FilterDebuff debuff
activeFilter (To _ action _)       = Just $ matchFilter FilterAction action
activeFilter (Bonus bonus _)       = Just $ matchFilter FilterBonus bonus
activeFilter (Chance _ ef')        = activeFilter ef'
activeFilter (Chances _ _ ef')     = activeFilter ef'
activeFilter (When _ ef')          = activeFilter ef'
activeFilter (Times _ ef')         = activeFilter ef'
activeFilter (ToMax _ ef')         = activeFilter ef'
