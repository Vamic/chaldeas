-- | Sidebars for filtering displayed elements in the list.
module Site.Filtering
  ( FilterTab(..)
  , Filter(..), simpleFilter
  , ScheduledFilter(..), getScheduled
  , FilterList(..), collectFilters
  , exclusive, getTab
  , updateListing
  ) where

import StandardLibrary
import Generic     as G
import Data.String as String

import Data.Date (Date)
import Data.Profunctor.Strong ((&&&))

import Printing
import Database.Skill
import Site.Preferences
import Site.ToImage

data FilterTab
    = FilterEventBonus
    | FilterAvailability
    | FilterAlignment
    | FilterTrait
    | FilterPassiveSkill
    | FilterBonus | FilterAction | FilterDebuff
    | FilterBuff BuffCategory
    | FilterDamage
    -- Exclusive
    | FilterSource
    | FilterPhantasm | FilterCard
    | FilterClass
    | FilterDeck
    | FilterAttribute
    | FilterRarity

exclusive :: FilterTab -> Boolean
exclusive = (_ >= FilterSource)

instance _a_ :: Show FilterTab where
    show FilterPhantasm = "NP Type"
    show FilterCard     = "NP Card"
    show (FilterBuff c) = "Buff (" <> show c <> ")"
    show x              = unCamel <<< String.drop 6 $ G.genericShow x

--data Filter a = Filter FilterTab String (Boolean -> a -> Boolean)

newtype Filter a = Filter { tab   :: FilterTab 
                          , name  :: String
                          , icon  :: Maybe ImagePath
                          , match :: Boolean -> a -> Boolean
                          }
simpleFilter :: ∀ a. FilterTab -> String -> (Boolean -> a -> Boolean) 
             -> Filter a
simpleFilter tab name match = Filter { icon: Nothing, tab, name, match }

getTab :: ∀ a. Filter a -> FilterTab
getTab (Filter x) = x.tab

instance _b_ :: Eq (Filter a) where
    eq (Filter x) (Filter y) = x.tab == y.tab && x.name == y.name
instance _c_ :: Ord (Filter a) where
    compare (Filter x) (Filter y) = compareThen _.tab _.name x y
instance _d_ :: Show (Filter a) where
    show (Filter x) = x.name

data ScheduledFilter a = ScheduledFilter Date Date (Filter a)

getScheduled :: ∀ a. Array (ScheduledFilter a) -> Date -> Array (Filter a)
getScheduled xs today = toFilter <$> filter scheduled xs
  where
    scheduled (ScheduledFilter start end _) = start <= today && today <= end
    toFilter (ScheduledFilter _ _ x) = x

type FilterState a b = { sorted   :: Array (Tuple String a)
                       , listing  :: Array (Tuple String a)
                       , matchAny :: Boolean
                       , filters  :: Array (Filter a)
                       , exclude  :: Array (Filter a)
                       , prefs    :: Preferences
                       | b
                       }

updateListing :: ∀ a b. FilterState a b -> FilterState a b
updateListing st = st{ listing = filter (match <<< snd) st.sorted }
  where
    noSelf = getPreference st.prefs ExcludeSelf
    matchFilter x (Filter f) = f.match noSelf x
    match x = (null st.exclude || all (not <<< matchFilter x) st.exclude)
           && (null st.filters || (if st.matchAny then any else all)
              (matchFilter x) st.filters)

type FilterList a = Array (Tuple FilterTab (Array (Filter a)))

collectFilters :: ∀ a. (Date -> FilterTab -> Array (Filter a)) -> Date 
               -> FilterList a
collectFilters f today = (identity &&& f today) <$> enumArray

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ :: G.Generic FilterTab _
derive instance _1_ :: Eq FilterTab
derive instance _2_ :: Ord FilterTab
instance _4_ :: G.Enum FilterTab where
    succ = G.genericSucc
    pred = G.genericPred
instance _5_ :: G.Bounded FilterTab where
    top = G.genericTop
    bottom = G.genericBottom
instance _6_ :: G.BoundedEnum FilterTab where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum
