module Site.Filters
  ( FilterTab(..)
  , Filter(..)
  , getFilters
  , exclusive
  ) where

import Prelude
import Operators (unCamel)
import Generic as G

import Data.Array ((..), filter, replicate, reverse)
import Data.Foldable (any, elem, notElem)
import Data.Profunctor.Strong ((&&&))
import Data.String (drop, joinWith)
import Data.Tuple (uncurry)

import Database (class MatchServant, Alignment, Attribute, BuffEffect, Card, Class, DebuffEffect, Deck, InstantEffect, PhantasmType, Servant, Trait(..), getAll, getPassives, has)

extraFilters ∷ Array Filter
extraFilters = join
  [ [ servantBonus FilterEventBonus "+100% Attack"
      [ "Illyasviel von Einzbern"
      , "Chloe von Einzbern"
      ,  "Mash Kyrielight"
      ]
    , servantBonus FilterEventBonus "+50% Attack"
      [ "Queen Medb"
      , "Nursery Rhyme"
      , "Helena Blavatsky"
      , "Medea (Lily)"
      ]
    , Filter FilterEventBonus "+50% Kaleid CE"
      <<< const $ notElem Male <<< _.traits
    ]
  , [ servantBonus FilterAvailability "New"
      [ "Illyasviel von Einzbern"
      , "Chloe von Einzbern"
      ]
    , Filter FilterAvailability "Limited" $ const _.limited
    , Filter FilterAvailability "Non-Limited" <<< const $ not <<< _.limited
    , Filter FilterAvailability "Free" $ const _.free
    ]
  , reverse (1..5) <#> \rarity
    -> Filter FilterRarity (joinWith "" $ replicate rarity "★")
    <<< const $ not <<< eq rarity <<< _.rarity
  ]
data FilterTab
    = FilterEventBonus
    | FilterAvailability
    | FilterAlignment
    | FilterTrait
    | FilterPassiveSkill
    | FilterAction | FilterBuff | FilterDebuff
    -- Exclusive
    | FilterPhantasm | FilterCard
    | FilterClass
    | FilterDeck
    | FilterAttribute
    | FilterRarity

exclusive ∷ FilterTab -> Boolean
exclusive = (_ >= FilterPhantasm)

instance _a_ ∷ Show FilterTab where
  show FilterPhantasm = "NP Type"
  show FilterCard     = "NP Card"
  show a              = unCamel <<< drop 6 $ G.genericShow a

data Filter = Filter FilterTab String (Boolean -> Servant -> Boolean)

instance _c_ ∷ Eq Filter where
  eq (Filter tabA a _) (Filter tabB b _) = tabA == tabB && a == b
instance _d_ ∷ Show Filter where
  show (Filter tab a _) = a

matchFilter ∷ ∀ a. MatchServant a => FilterTab -> a -> Filter
matchFilter tab
  | exclusive tab = uncurry (Filter tab) <<< (show &&& not <<< has)
  | otherwise     = uncurry (Filter tab) <<< (show &&& has)

servantBonus ∷ FilterTab -> String -> Array String -> Filter
servantBonus tab bonus servants
    = Filter tab bonus <<< const $ (_ `elem` servants) <<< _.name

getExtraFilters ∷ FilterTab -> Array Filter
getExtraFilters tab = filter fromTab extraFilters
  where
    fromTab (Filter t _ _) = tab == t

getFilters ∷ FilterTab -> Array Filter
getFilters f@FilterAction    = matchFilter f <$> getAll ∷ Array InstantEffect
getFilters f@FilterAlignment = matchFilter f <$> getAll ∷ Array Alignment
getFilters f@FilterAttribute = matchFilter f <$> getAll ∷ Array Attribute
getFilters f@FilterBuff      = matchFilter f <$> getAll ∷ Array BuffEffect
getFilters f@FilterCard      = matchFilter f <$> getAll ∷ Array Card
getFilters f@FilterClass     = matchFilter f <$> getAll ∷ Array Class
getFilters f@FilterDebuff    = matchFilter f <$> getAll ∷ Array DebuffEffect
getFilters f@FilterDeck      = matchFilter f <$> getAll ∷ Array Deck
getFilters f@FilterPhantasm  = matchFilter f <$> getAll ∷ Array PhantasmType
getFilters f@FilterTrait     = matchFilter f <$> getAll ∷ Array Trait
getFilters f@FilterPassiveSkill   = uncurry (Filter f)
                             <<< (identity &&& const <<< hasPassive) <$> getPassives
  where
    hasPassive p = any (eq p) <<< map (_.name) <<< _.passives
getFilters f                 = getExtraFilters f

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
