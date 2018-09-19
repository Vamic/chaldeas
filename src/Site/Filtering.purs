-- | Sidebars for filtering displayed elements in the list.
module Site.Filtering
  ( simpleFilter
  , ScheduledFilter(..), getScheduled
  , FilterList(..), collectFilters
  , getTab
  , updateListing
  , matchFilter
  , imageFilter
  , namedBonus
  , skillFilter
  ) where

import StandardLibrary

import Data.Date (Date)

import Database.Skill
import Database.Has
import Site.Algebra
import Site.Preferences
import Site.ToImage

simpleFilter :: ∀ a. FilterTab -> String -> (Boolean -> a -> Boolean)
             -> Filter a
simpleFilter tab name match = Filter { icon: Nothing, tab, name, match }

getTab :: ∀ a. Filter a -> FilterTab
getTab (Filter x) = x.tab

data ScheduledFilter a = ScheduledFilter Date Date (Filter a)

getScheduled :: ∀ a. Array (ScheduledFilter a) -> Date -> Array (Filter a)
getScheduled xs today = toFilter <$> filter scheduled xs
  where
    scheduled (ScheduledFilter start end _) = start <= today && today <= end
    toFilter (ScheduledFilter _ _ x) = x

updateListing :: ∀ a b c. (b -> a) -> SiteState a b c -> SiteState a b c
updateListing f st = st{ listing = filter (match <<< f <<< _.obj) st.sorted }
  where
    noSelf = prefer st.prefs ExcludeSelf
    matches x (Filter filt) = filt.match noSelf x
    match x = (null st.exclude || all (not <<< matches x) st.exclude)
           && (null st.filters || (if st.matchAny then any else all)
              (matches x) st.filters)

type FilterList a = Array { tab :: FilterTab, filters :: Array (Filter a) }

collectFilters :: ∀ a. (Date -> FilterTab -> Array (Filter a)) -> Date
               -> FilterList a
collectFilters f today = go <$> enumArray
  where
    go tab = { tab, filters: f today tab }

matchFilter :: ∀ a b. Has a b => FilterTab -> b -> Filter a
matchFilter tab x =
    Filter { icon:  Nothing
           , tab
           , name:  show x
           , match: has x
           }

imageFilter :: ∀ a b. ToImage b => Has a b
            => FilterTab -> b -> Filter a
imageFilter tab x =
    Filter { icon:  Just $ toImagePath x
           , tab
           , name:  show x
           , match: has x
           }

namedBonus :: ∀ a. Show a => FilterTab -> String -> Array String -> Filter a
namedBonus tab name names =
    Filter { icon: Nothing
           , tab
           , name
           , match: const $ (_ `elem` names) <<< show
           }

skillFilter :: ∀ a. HasEffects a => SkillEffect -> Maybe (Filter a)
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
