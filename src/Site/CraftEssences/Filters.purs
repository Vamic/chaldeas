module Site.CraftEssences.Filters
  ( activeFilter
  , getFilters
  ) where

import Prelude
import Data.String as S

import Data.Array
import Data.Foldable
import Data.Maybe
import Data.Profunctor.Strong
import Data.Tuple

import Site.Filtering
import Database

extraFilters ∷ Array (Filter CraftEssence)
extraFilters = join
  [ [ namedBonus FilterEventBonus "+50% Attack for Female"
      [ "Kaleid Ruby"
      , "Kaleid Sapphire"
      ]
  , namedBonus FilterEventBonus "+Mr. Lion-Go Toy"
      [ "Magical Girl of Sapphire" ]
  , namedBonus FilterEventBonus "+Magical☆Bushido Musashi"
      [ "Kill on Sight" ]
  , namedBonus FilterEventBonus "+Expensive Pudding"
      [ "Zunga Zunga!" ]
  ]
  , [ Filter FilterAvailability "Limited"
      \_ (CraftEssence ce) -> ce.limited
    , Filter FilterAvailability "Non-Limited"
      \_ (CraftEssence ce) -> not ce.limited
    ]
  , reverse (1..5) <#> \rarity
    -> Filter FilterRarity (S.joinWith "" $ replicate rarity "★")
    \_ (CraftEssence ce) -> rarity /= ce.rarity
  ]

matchFilter ∷ ∀ a. MatchCraftEssence a => FilterTab -> a -> Filter CraftEssence
matchFilter tab
  | exclusive tab = uncurry (Filter tab) <<< (show &&& not <<< ceHas)
  | otherwise     = uncurry (Filter tab) <<< (show &&& ceHas)

namedBonus ∷ FilterTab -> String -> Array String -> Filter CraftEssence
namedBonus tab bonus craftEssences
    = Filter tab bonus \_ (CraftEssence ce) -> ce.name `elem` craftEssences

getExtraFilters ∷ FilterTab -> Array (Filter CraftEssence)
getExtraFilters tab = filter fromTab extraFilters
  where
    fromTab (Filter t _ _) = tab == t

getFilters ∷ FilterTab -> Array (Filter CraftEssence)
getFilters f@FilterBonus  = matchFilter f <$> ceGetAll ∷ Array BonusEffect
getFilters f@FilterAction = matchFilter f <$> ceGetAll ∷ Array InstantEffect
getFilters f@FilterBuff   = matchFilter f <$> ceGetAll ∷ Array BuffEffect
getFilters f@FilterDebuff = matchFilter f <$> ceGetAll ∷ Array DebuffEffect
getFilters f              = getExtraFilters f

activeFilter ∷ ActiveEffect -> Maybe (Filter CraftEssence)
activeFilter ef
  | demerit ef = Nothing
  | otherwise  = go ef
  where
    go (Grant _ _ buff _) = Just $ matchFilter FilterBuff buff
    go (Debuff _ _ debuff _) = Just $ matchFilter FilterDebuff debuff
    go (To _ action _) = Just $ matchFilter FilterAction action
    go (Bonus bonus _) = Just $ matchFilter FilterBonus bonus
    go (Chance _ ef') = activeFilter ef'
    go (Chances _ _ ef') = activeFilter ef'
    go (When _ ef') = activeFilter ef'
    go (Times _ ef') = activeFilter ef'
