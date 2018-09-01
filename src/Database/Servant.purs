module Database.Servant
  ( Servant
  , Deck(..)
  , NoblePhantasm
  , Gen
  , Hits
  , PhantasmType(..)
  , class MatchServant, has
  ) where

import Prelude
import Operators ((:))
import Generic as G

import Data.Foldable (any, elem)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (charAt, fromCharArray)
import Data.Tuple (Tuple)

import Database.Base (Alignment, Attribute, Card, Class, Stat, Trait)
import Database.Passive (Passive)
import Database.Skill (Active, ActiveEffect(..), BuffEffect, DebuffEffect, InstantEffect(..), Rank, Target(..), allied, simplify)

type Servant = { name     ∷ String
               , id       ∷ Int
               , rarity   ∷ Int
               , class    ∷ Class
               , attr     ∷ Attribute
               , deck     ∷ Deck
               , stats    ∷ { base ∷ Stat, max ∷ Stat, grail ∷ Stat }
               , actives  ∷ Array Active
               , passives ∷ Array Passive
               , phantasm ∷ NoblePhantasm
               , gen      ∷ Gen
               , hits     ∷ Hits
               , traits   ∷ Array Trait
               , death    ∷ Number
               , align    ∷ Tuple Alignment Alignment
               , limited  ∷ Boolean
               , free     ∷ Boolean
               }

data Deck = Deck Card Card Card Card Card

type Ratings = { damage     ∷ Int
               , np         ∷ Int
               , critical   ∷ Int
               , utility    ∷ Int
               , support    ∷ Int
               , durability ∷ Int
               }

type NoblePhantasm = { name   ∷ String
                     , desc   ∷ String
                     , rank   ∷ Rank
                     , card   ∷ Card
                     , kind   ∷ String
                     , hits   ∷ Int
                     , effect ∷ Array ActiveEffect
                     , over   ∷ Array ActiveEffect
                     , first  ∷ Boolean
                     }

type Hits = { arts ∷ Int, buster ∷ Int, quick ∷ Int, ex ∷ Int }

type Gen = { starWeight ∷ Int
           , starRate   ∷ Number
           , npAtk      ∷ Number
           , npDef      ∷ Int
           }

getEffects ∷ Servant -> Array ActiveEffect
getEffects {phantasm:{effect, over}, actives}
    = simplify <$> effect <> over <> (actives >>= _.effect)

phantasmEffects ∷ NoblePhantasm -> Array ActiveEffect
phantasmEffects {effect, over} = effect <> over

data PhantasmType = SingleTarget | MultiTarget | Support
instance _01_ ∷ Show PhantasmType where
  show = case _ of
    SingleTarget -> "Single-Target"
    MultiTarget  -> "Multi-Target"
    Support      -> "Support"


class (G.BoundedEnum a, Show a) <= MatchServant a where
    has ∷ a -> Boolean -> Servant -> Boolean
instance _a_ ∷ MatchServant BuffEffect where
    has a noSelf = any match <<< getEffects where
        match (Grant t _ b _) = a == b && allied t && (not noSelf || t /= Self)
        match _ = false
instance _b_ ∷ MatchServant DebuffEffect where
    has a _ = any match <<< getEffects where
        match (Debuff t _ b _) = a == b && not (allied t)
        match _ = false
instance _c_ ∷ MatchServant InstantEffect where
    has DemeritBuffs  _ = const false
    has DemeritCharge _ = const false
    has DemeritDamage _ = const false
    has DemeritGauge  _ = const false
    has DemeritHealth _ = const false
    has DemeritKill   _ = const false
    has a noSelf        = any match <<< getEffects where
        match (To t b _) = a == b && (not noSelf || t /= Self)
        match _ = false
instance _d_ ∷ MatchServant Trait where
    has a = const $ elem a <<< _.traits
instance _e_ ∷ MatchServant Alignment where
    has a _ {align:(b:c)} = a == b || a == c
instance _f_ ∷ MatchServant PhantasmType where
    has SingleTarget _ {phantasm} = any match $ phantasmEffects phantasm
      where
        match (To Enemy Damage _) = true
        match (To Enemy DamageThruDef _) = true
        match _ = false
    has MultiTarget _ {phantasm} = any match $ phantasmEffects phantasm
      where
        match (To Enemies Damage _) = true
        match (To Enemies DamageThruDef _) = true
        match _ = false
    has Support x s = not (has SingleTarget x s) && not (has MultiTarget x s)
instance _g_ ∷ MatchServant Class where
    has a = const $ eq a <<< _.class
instance _h_ ∷ MatchServant Attribute where
    has a = const $ eq a <<< _.attr
instance _i_ ∷ MatchServant Deck where
    has a = const $ eq a <<< _.deck
instance _j_ ∷ MatchServant Card where
    has a = const $ eq a <<< _.phantasm.card
-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _7_ ∷ Eq PhantasmType
derive instance _8_ ∷ Ord PhantasmType
derive instance _9_ ∷ G.Generic PhantasmType _
instance _10_ ∷ G.Enum PhantasmType where
  succ = G.genericSucc
  pred = G.genericPred
instance _11_ ∷ G.Bounded PhantasmType where
  top = G.genericTop
  bottom = G.genericBottom
instance _12_ ∷ G.BoundedEnum PhantasmType where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum

derive instance _34_ ∷ Eq Deck
derive instance _35_ ∷ Ord Deck
derive instance _36_ ∷ G.Generic Deck _
instance _37_ ∷ G.Enum Deck where
  succ = G.genericSucc
  pred = G.genericPred
instance _38_ ∷ G.Bounded Deck where
  top = G.genericTop
  bottom = G.genericBottom
instance _39_ ∷ G.BoundedEnum Deck where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum
instance _40_ ∷ Show Deck where
  show (Deck a b c d e) = fromCharArray
                        $ (fromMaybe '?' <<< charAt 0 <<< show) <$> [a, b, c, d, e]
