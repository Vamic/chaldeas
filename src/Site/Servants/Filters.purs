module Site.Servants.Filters
  ( getFilters
  , skillFilter
  , matchFilter
  , passiveFilter
  , singleFilter
  ) where

import Prelude
import Operators
import Data.String as S

import Data.Array
import Data.Date.Component
import Data.Date
import Data.Maybe
import Data.Profunctor.Strong
import Data.Tuple

import Database
import Site.Filtering

extraFilters :: Array (Filter Servant)
extraFilters = join
  [ [ namedBonus FilterAvailability "New"
      [ "Illyasviel von Einzbern"
      , "Chloe von Einzbern"
      ]
    , Filter FilterAvailability "Free"
      \_ (Servant s) -> s.free
    ]
  , [ Filter FilterSource "Limited"
      \_ (Servant s) -> s.limited
    , Filter FilterSource "Non-Limited"
      \_ (Servant s) -> not s.limited
  ]
  , reverse (1..5) <#> \rarity
    -> Filter FilterRarity (S.joinWith "" $ replicate rarity "★")
    \_ (Servant s) -> rarity == s.rarity
  ]

scheduledFilters :: Array (ScheduledFilter Servant)
scheduledFilters = 
  [ ScheduledFilter (ymd 2018 September 11) (ymd 2018 September 11) $
    Filter FilterAvailability "Rate-Up"
    \_ (Servant s) -> not s.limited && s.class == Berserker

  ------------
  -- NERO FEST
  ------------

  , ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterAvailability "Limited to Event"
    [ "Nero Claudius (Bride)" ]

  , ScheduledFilter (ymd 2018 September 13) (ymd 2018 September 26) $
    namedBonus FilterAvailability "Rate-Up"
    [ "Nero Claudius (Bride)", "Nero Claudius"
    , "Gaius Julius Caesar", "Romulus", "Boudica"
    ]
  ]

matchFilter :: ∀ a. MatchServant a => FilterTab -> a -> Filter Servant
matchFilter tab = uncurry (Filter tab) <<< (show &&& has)

singleFilter :: ∀ a. MatchServant a => FilterTab -> a -> Array (Filter Servant)
singleFilter tab x
  | exclusive tab = matchFilter tab <$> delete x getAll
  | otherwise = [matchFilter tab x]

namedBonus :: FilterTab -> String -> Array String -> Filter Servant
namedBonus tab bonus servants = Filter tab bonus 
                                \_ (Servant s) -> s.name `elem` servants

getExtraFilters :: Date -> FilterTab -> Array (Filter Servant)
getExtraFilters today tab = 
    filter fromTab $ getScheduled scheduledFilters today <> extraFilters
  where
    fromTab (Filter t _ _) = tab == t

getFilters :: Date -> FilterTab -> Array (Filter Servant)
getFilters _ f@FilterAlignment    = matchFilter f 
                                    <$> getAll :: Array Alignment
getFilters _ f@FilterAttribute    = matchFilter f 
                                    <$> getAll :: Array Attribute
getFilters _ f@FilterCard         = matchFilter f 
                                    <$> getAll :: Array Card
getFilters _ f@FilterClass        = matchFilter f 
                                    <$> getAll :: Array Class
getFilters _ f@FilterDebuff       = matchFilter f
                                    <$> getAll :: Array DebuffEffect
getFilters _ f@FilterDeck         = matchFilter f 
                                    <$> getAll :: Array Deck
getFilters _ f@FilterPhantasm     = matchFilter f 
                                    <$> getAll :: Array PhantasmType
getFilters _ f@FilterTrait        = matchFilter f 
                                    <$> getAll :: Array Trait
getFilters _ f@FilterPassiveSkill = passiveFilter 
                                    <$> getPassives
getFilters _ f@(FilterBuff c)     = matchFilter f 
                                    <$> filter (eq c <<< buffCategory) 
                                    getAll :: Array BuffEffect
getFilters _ f@FilterAction       = matchFilter f 
                                    <$> filter (not <<< isDamage) 
                                    getAll :: Array InstantEffect
getFilters _ f@FilterDamage       = matchFilter f 
                                    <$> filter isDamage 
                                    getAll :: Array InstantEffect
getFilters today f               = getExtraFilters today f

passiveFilter :: String -> Filter Servant
passiveFilter = uncurry (Filter FilterPassiveSkill) <<< 
                (identity &&& const <<< hasPassive)
  where
    hasPassive p (Servant s) = any (eq p) $ _.name <$> s.passives

plural :: ∀ a. MatchServant a => a -> Maybe a
plural x
  | null <<< drop 1 $ filter (has x false) servants = Nothing
  | otherwise = Just x

skillFilter :: SkillEffect -> Maybe (Filter Servant)
skillFilter (Grant _ _ buff _)    = Just $ matchFilter 
                                     (FilterBuff $ buffCategory buff) buff
skillFilter (Debuff _ _ debuff _) = matchFilter FilterDebuff <$> plural debuff
skillFilter (To _ action _)       = matchFilter FilterAction <$> plural action
skillFilter (Bonus _ _)           = Nothing
skillFilter (Chance _ ef')        = skillFilter ef'
skillFilter (Chances _ _ ef')     = skillFilter ef'
skillFilter (When _ ef')          = skillFilter ef'
skillFilter (Times _ ef')         = skillFilter ef'
skillFilter (ToMax _ ef')         = skillFilter ef'
