module Site.Filtering
  ( FilterTab(..)
  , Filter(..)
  , exclusive
  , getTab
  , updateListing
  ) where

import Prelude
import Operators
import Generic as G
import Data.String as S

import Data.Array
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

exclusive ∷ FilterTab -> Boolean
exclusive = (_ >= FilterSource)

instance _a_ ∷ Show FilterTab where
  show FilterPhantasm = "NP Type"
  show FilterCard     = "NP Card"
  show (FilterBuff c) = "Buff (" <> show c <> ")"
  show a              = unCamel <<< S.drop 6 $ G.genericShow a

data Filter a = Filter FilterTab String (Boolean -> a -> Boolean)

getTab ∷ ∀ a. Filter a -> FilterTab
getTab (Filter tab _ _) = tab

instance _c_ ∷ Eq (Filter a) where
  eq (Filter tabA a _) (Filter tabB b _) = tabA == tabB && a == b
instance _d_ ∷ Ord (Filter a) where
  compare (Filter tabA a _) (Filter tabB b _) = case compare tabA tabB of
      LT -> LT
      GT -> GT
      EQ -> compare a b
instance _e_ ∷ Show (Filter a) where
  show (Filter tab a _) = a

type FilterState a b = { sorted   ∷ Array (Tuple String a)
                       , listing  ∷ Array (Tuple String a)
                       , matchAny ∷ Boolean
                       , filters  ∷ Array (Filter a)
                       , exclude  ∷ Array (Filter a)
                       , prefs    ∷ Preferences
                       | b
                       }

updateListing ∷ ∀ a b. FilterState a b -> FilterState a b
updateListing st@{exclude, filters, matchAny, prefs, sorted}
    = st{ listing = filter (match <<< snd) sorted }
  where
    noSelf = getPreference prefs ExcludeSelf
    matchFilter x (Filter _ _ f) = f noSelf x
    match x = (null exclude || all (not <<< matchFilter x) exclude)
           && (null filters || (if matchAny then any else all)
                               (matchFilter x) filters)


-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ G.Generic FilterTab _
derive instance _1_ ∷ Eq FilterTab
derive instance _2_ ∷ Ord FilterTab
instance _4_ ∷ G.Enum FilterTab where
  succ = G.genericSucc
  pred = G.genericPred
instance _5_ ∷ G.Bounded FilterTab where
  top = G.genericTop
  bottom = G.genericBottom
instance _6_ ∷ G.BoundedEnum FilterTab where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum
