module Filters where

import Prelude
import Operators

import Data.Array (elem, filter, notElem)
import Data.Enum
import Data.Profunctor.Strong
import Data.Generic.Rep    
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum 
import Data.Generic.Rep.Show 
import Data.String (drop)
import Data.Tuple  

import Database

extraFilters ∷ Array Filter
extraFilters = 
  [ servantBonus FilterEvent "+100% Attack" 
    [ "Illyasviel von Einzbern"
    , "Chloe von Einzbern"
    ,  "Mash Kyrielight"
    ]
  , servantBonus FilterEvent "+50% Attack"
    [ "Queen Medb"
    , "Nursery Rhyme"
    , "Helena Blavatsky"
    , "Medea (Lily)"
    ]
  , Filter FilterEvent "+50% Kaleid CE" 
    $ \_ (Servant {traits}) -> Male `notElem` traits
  , servantBonus FilterOther "New" 
    [ "Illyasviel von Einzbern" 
    , "Chloe von Einzbern"
    ]
  , Filter FilterOther "Limited" $ \_ (Servant {limited}) -> limited
  , Filter FilterOther "Non-Limited" $ \_ (Servant {limited}) -> not limited
  ]
data FilterTab = FilterEvent | FilterOther
               | FilterPhantasm | FilterCard 
               | FilterClass | FilterDeck | FilterAttribute
               | FilterAlignment | FilterTrait | FilterPassive
               | FilterAction | FilterBuff | FilterDebuff 

instance _a_ ∷ Show FilterTab where
  show FilterEvent    = "Event Bonus"
  show FilterPhantasm = "NP Type"
  show FilterCard     = "NP Card"
  show FilterPassive  = "Passive Skill"
  show FilterOther    = "Availability"
  show a              = drop 6 $ genericShow a

data Filter = Filter FilterTab String (Boolean -> Servant -> Boolean)
instance _b_ ∷ Eq Filter where
  eq (Filter tabA a _) (Filter tabB b _) = tabA == tabB && a == b
instance _c_ ∷ Show Filter where
  show (Filter tab a _) = a

matchFilter ∷ ∀ a. MatchServant a ⇒ FilterTab -> a -> Filter
matchFilter tab = uncurry (Filter tab) ∘ (show &&& has)
  
servantBonus ∷ FilterTab -> String -> Array String -> Filter
servantBonus tab bonus servants = Filter tab bonus match
    where
      match _ (Servant {name}) = name `elem` servants

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
getFilters f@FilterPassive   = uncurry (Filter f) 
                             ∘ (identity &&& const ∘ hasPassive) <$> getPassives
getFilters f@FilterEvent     = getExtraFilters f
getFilters f@FilterOther     = getExtraFilters f

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Generic FilterTab _
derive instance _1_ ∷ Eq FilterTab
derive instance _2_ ∷ Ord FilterTab
instance _4_ ∷ Enum FilterTab where
  succ = genericSucc
  pred = genericPred
instance _5_ ∷ Bounded FilterTab where
  top = genericTop
  bottom = genericBottom
instance _6_ ∷ BoundedEnum FilterTab where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
