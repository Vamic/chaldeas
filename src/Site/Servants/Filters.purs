module Site.Servants.Filters
  ( getFilters
  , skillFilter
  , matchFilter
  , passiveFilter
  , singleFilter
  ) where

import Database
import Site.Common
import Site.Filtering
import Site.ToImage
import StandardLibrary

import Data.Date (Date, Month(..))
import Data.String as String

extraFilters :: Array (Filter Servant)
extraFilters = join
  [ [ namedBonus FilterAvailability "New"
      [ "Illyasviel von Einzbern"
      , "Chloe von Einzbern"
      ]
    , simpleFilter FilterAvailability "Free"
      \_ (Servant s) -> s.free
    ]
  , [ simpleFilter FilterSource "Limited"
      \_ (Servant s) -> s.limited
    , simpleFilter FilterSource "Non-Limited"
      \_ (Servant s) -> not s.limited
  ]
  , reverse (1..5) <#> \rarity
    -> simpleFilter FilterRarity (String.joinWith "" $ replicate rarity "★")
    \_ (Servant s) -> rarity == s.rarity
  ]

scheduledFilters :: Array (ScheduledFilter Servant)
scheduledFilters = 
  [ ScheduledFilter (ymd 2018 September 11) (ymd 2018 September 11) $
    simpleFilter FilterAvailability "Rate-Up"
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
matchFilter tab x = 
    Filter { icon:  Nothing
           , tab
           , name:  show x
           , match: has x
           }

imageFilter :: ∀ a. ToImage a => MatchServant a 
            => FilterTab -> a -> Filter Servant
imageFilter tab x = 
    Filter { icon:  Just $ toImagePath x
           , tab
           , name:  show x 
           , match: has x
           }

passiveFilter :: Skill -> Filter Servant
passiveFilter p = 
    Filter { icon: Just $ toImagePath p.icon
           , tab:  FilterPassiveSkill
           , name: p.name
           , match: \_ (Servant s) -> 
                   any (eq p.name) $ _.name <$>  s.passives
           }

namedBonus :: FilterTab -> String -> Array String -> Filter Servant
namedBonus tab name names = 
    Filter { icon: Nothing
           , tab
           , name
           , match: \_ (Servant s) -> s.name `elem` names
           }

singleFilter :: ∀ a. MatchServant a => FilterTab -> a -> Array (Filter Servant)
singleFilter tab x
  | exclusive tab = matchFilter tab <$> delete x getAll
  | otherwise = [matchFilter tab x]

getExtraFilters :: Date -> FilterTab -> Array (Filter Servant)
getExtraFilters today tab = 
    filter fromTab $ getScheduled scheduledFilters today <> extraFilters
  where
    fromTab (Filter x) = tab == x.tab

getFilters :: Date -> FilterTab -> Array (Filter Servant)
getFilters _ f@FilterAlignment    = matchFilter f 
                                    <$> getAll :: Array Alignment
getFilters _ f@FilterAttribute    = matchFilter f 
                                    <$> getAll :: Array Attribute
getFilters _ f@FilterCard         = imageFilter f 
                                    <$> getAll :: Array Card
getFilters _ f@FilterClass        = imageFilter f 
                                    <$> getAll :: Array Class
getFilters _ f@FilterDebuff       = imageFilter f
                                    <$> getAll :: Array DebuffEffect
getFilters _ f@FilterDeck         = matchFilter f 
                                    <$> getAll :: Array Deck
getFilters _ f@FilterPhantasm     = matchFilter f 
                                    <$> getAll :: Array PhantasmType
getFilters _ f@FilterTrait        = matchFilter f 
                                    <$> getAll :: Array Trait
getFilters _ f@FilterPassiveSkill = passiveFilter 
                                    <$> getPassives
getFilters _ f@(FilterBuff c)     = imageFilter f 
                                    <$> filter (eq c <<< buffCategory) 
                                    getAll :: Array BuffEffect
getFilters _ f@FilterAction       = matchFilter f 
                                    <$> filter (not <<< isDamage) 
                                    getAll :: Array InstantEffect
getFilters _ f@FilterDamage       = matchFilter f 
                                    <$> filter isDamage 
                                    getAll :: Array InstantEffect
getFilters today f               = getExtraFilters today f

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
