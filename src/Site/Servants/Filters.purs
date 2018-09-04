module Site.Servants.Filters
  ( getFilters
  , activeFilter
  , matchFilter
  , passiveFilter
  , singleFilter
  ) where

import Prelude
import Data.String as S

import Data.Array
import Data.Maybe
import Data.Profunctor.Strong
import Data.Tuple

import Database
import Site.Filtering

extraFilters ∷ Array (Filter Servant)
extraFilters = join
  [ [ namedBonus FilterEventBonus "+100% Attack"
      [ "Illyasviel von Einzbern"
      , "Chloe von Einzbern"
      ,  "Mash Kyrielight"
      ]
    , namedBonus FilterEventBonus "+50% Attack"
      [ "Queen Medb"
      , "Nursery Rhyme"
      , "Helena Blavatsky"
      , "Medea (Lily)"
      ]
    , Filter FilterEventBonus "+50% Kaleid CE"
      \_ (Servant s) -> Male `notElem` s.traits
    ]
  , [ namedBonus FilterAvailability "New"
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

matchFilter ∷ ∀ a. MatchServant a => FilterTab -> a -> Filter Servant
matchFilter tab = uncurry (Filter tab) <<< (show &&& has)

singleFilter ∷ ∀ a. MatchServant a => FilterTab -> a -> Array (Filter Servant)
singleFilter tab a
  | exclusive tab = matchFilter tab <$> delete a getAll
  | otherwise = [matchFilter tab a]

namedBonus ∷ FilterTab -> String -> Array String -> Filter Servant
namedBonus tab bonus servants
    = Filter tab bonus \_ (Servant s) -> s.name `elem` servants

getExtraFilters ∷ FilterTab -> Array (Filter Servant)
getExtraFilters tab = filter fromTab extraFilters
  where
    fromTab (Filter t _ _) = tab == t

getFilters ∷ FilterTab -> Array (Filter Servant)
getFilters f@FilterAction       = matchFilter f <$> getAll ∷ Array InstantEffect
getFilters f@FilterAlignment    = matchFilter f <$> getAll ∷ Array Alignment
getFilters f@FilterAttribute    = matchFilter f <$> getAll ∷ Array Attribute
getFilters f@FilterBuff         = matchFilter f <$> getAll ∷ Array BuffEffect
getFilters f@FilterCard         = matchFilter f <$> getAll ∷ Array Card
getFilters f@FilterClass        = matchFilter f <$> getAll ∷ Array Class
getFilters f@FilterDebuff       = matchFilter f <$> getAll ∷ Array DebuffEffect
getFilters f@FilterDeck         = matchFilter f <$> getAll ∷ Array Deck
getFilters f@FilterPhantasm     = matchFilter f <$> getAll ∷ Array PhantasmType
getFilters f@FilterTrait        = matchFilter f <$> getAll ∷ Array Trait
getFilters f@FilterPassiveSkill = passiveFilter <$> getPassives
getFilters f                    = getExtraFilters f

passiveFilter ∷ String -> Filter Servant
passiveFilter = uncurry (Filter FilterPassiveSkill)
            <<< (identity &&& const <<< hasPassive)
  where
    hasPassive p (Servant s) = any (eq p) $ _.name <$> s.passives

plural ∷ ∀ a. MatchServant a => a -> Maybe a
plural a
  | null <<< drop 1 $ filter (has a false) servants = Nothing
  | otherwise = Just a

activeFilter ∷ ActiveEffect -> Maybe (Filter Servant)
activeFilter (Grant _ _ buff _) = matchFilter FilterBuff <$> plural buff
activeFilter (Debuff _ _ debuff _) = matchFilter FilterDebuff <$> plural debuff
activeFilter (To _ action _) = matchFilter FilterAction <$> plural action
activeFilter (Bonus _ _) = Nothing
activeFilter (Chance _ ef') = activeFilter ef'
activeFilter (Chances _ _ ef') = activeFilter ef'
activeFilter (When _ ef') = activeFilter ef'
activeFilter (Times _ ef') = activeFilter ef'
activeFilter (ToMax _ ef') = activeFilter ef'
