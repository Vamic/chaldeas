-- | Shared Halogen logic.
module Site.Algebra
  ( Section(..)
  , SiteQuery(..), SiteState(..), SiteMessage(..)
  , FilterTab(..), Filter(..), exclusive
  ) where

import StandardLibrary
import Data.String.CodeUnits as CodeUnits
import Generic               as G
import Data.String           as String

import Database
import Printing
import Sorting
import Site.Preferences
import Site.ToImage

-- | Parts of the site.
data Section 
    = SectionBrowse 
    | SectionSettings 
    | SectionSortBy 
    | SectionInclude 
    | SectionFilter
instance _a_ :: Show Section where
    show = unCamel <<< String.drop 7 <<< G.genericShow

-- | Commands used in links and `Site.Eval`.
data SiteQuery inFilters inFocus toAlternate a
    = Switch    (Maybe toAlternate) a
    | ToSection (Maybe Section) a
    | Focus     (Maybe inFocus) a
    | ClearAll  a
    | Check     FilterTab Boolean a
    | FilterBy  (Array (Filter inFilters)) a
    | Toggle    (Filter inFilters) a
    | MatchAny  Boolean a
    | SetSort   SortBy a
    | SetPref   Preference Boolean a
    | Ascend    inFocus Int a
    | OnTeam    Boolean inFocus a
    | MineOnly  Boolean a
    | DoNothing a

-- | Switches `Site.Component` between 
-- | `Site.Servants.Component` and `Site.CraftEssences.Component`.
data SiteMessage inFilters toAlternate = 
    SiteMessage SortBy (Array (Filter inFilters)) (Maybe toAlternate)

-- | Halogen state.
type SiteState inFilters inFocus e =
    { section  :: Maybe Section
    , filters  :: Array (Filter inFilters)
    , exclude  :: Array (Filter inFilters)
    , matchAny :: Boolean
    , focus    :: Maybe inFocus
    , sortBy   :: SortBy
    , prefs    :: Preferences
    , sorted   :: Array { label :: String, obj :: inFocus }
    , listing  :: Array { label :: String, obj :: inFocus }
    | e
    }

data FilterTab
    = FilterEventBonus
    | FilterAvailability
    | FilterAlignment
    | FilterTrait
    | FilterPassiveSkill
    | FilterMaterial
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

instance _b_ :: Show FilterTab where
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

instance _c_ :: Eq (Filter a) where
    eq (Filter x) (Filter y) = x.tab == y.tab && x.name == y.name
instance _d_ :: Ord (Filter a) where
    compare (Filter x) (Filter y) = compareThen _.tab trimName x y
      where
        trimName {name} = fromMaybe name do
            {head} <- CodeUnits.uncons name
            guard $ head == '+'
            {before, after} <- splitAround (Pattern " ") name
            pure $ after <> before
instance _e_ :: Show (Filter a) where
    show (Filter x) = x.name
derive instance _f_ :: Newtype (Filter a) _

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

derive instance _10_ :: G.Generic Section _
derive instance _11_ :: Eq Section
derive instance _12_ :: Ord Section
instance _13_ :: G.Enum Section where
    succ = G.genericSucc
    pred = G.genericPred
instance _14_ :: G.Bounded Section where
    top = G.genericTop
    bottom = G.genericBottom
instance _15_ :: G.BoundedEnum Section where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum
