module Site.Filtering
  ( FilterTab(..)
  , Filter(..)
  , ScheduledFilter(..), getScheduled
  , FilterList(..), collectFilters
  , exclusive
  , getTab
  , updateListing
  ) where

import Prelude
import Operators
import Generic as G
import Data.String as S

import Data.Array
import Data.Date
import Data.Tuple

import Database.Skill
import Site.Preferences

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
  show x              = unCamel <<< S.drop 6 $ G.genericShow x

data Filter a = Filter FilterTab String (Boolean -> a -> Boolean)

getTab :: ∀ a. Filter a -> FilterTab
getTab (Filter tab _ _) = tab

instance _c_ :: Eq (Filter a) where
  eq (Filter tabX x _) (Filter tabY y _) = tabX == tabY && x == y
instance _d_ :: Ord (Filter a) where
  compare = compareThen getTab \(Filter _ x _) -> x
instance _e_ :: Show (Filter a) where
  show (Filter tab x _) = x

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
    matchFilter x (Filter _ _ f) = f noSelf x
    match x = (null st.exclude || all (not <<< matchFilter x) st.exclude)
           && (null st.filters || (if st.matchAny then any else all)
              (matchFilter x) st.filters)

type FilterList a = Array (Tuple FilterTab (Array (Filter a)))

collectFilters :: ∀ a. (Date -> FilterTab -> Array (Filter a)) -> Date 
               -> FilterList a
collectFilters f today = enumArray <#> \filt -> Tuple filt $ f today filt

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
