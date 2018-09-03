module Database.Servant
  ( Servant(..)
  , Deck(..)
  , NoblePhantasm
  , Gen
  , Hits
  , PhantasmType(..)
  , class MatchServant, has
  ) where

import Prelude
import Generic as G

import Data.Array
import Data.Maybe
import Data.String.CodeUnits
import Data.Tuple

import Database.Base
import Database.Passive
import Database.Skill

newtype Servant = Servant { name     ∷ String
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

instance _0_ ∷ Show Servant where
  show (Servant s) = s.name

data Deck = Deck Card Card Card Card Card

data Ratings = Ratings { damage     ∷ Int
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
getEffects (Servant {phantasm:{effect, over}, actives})
    = filter (not <<< demerit)
    $ simplify <$> effect <> over <> (actives >>= _.effect)

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
        match (Grant t _ b _) = a == b && (not noSelf || t /= Self)
        match _ = false
instance _b_ ∷ MatchServant DebuffEffect where
    has a _ = any match <<< getEffects where
        match (Debuff t _ b _) = a == b
        match _ = false
instance _c_ ∷ MatchServant InstantEffect where
    has a noSelf        = any match <<< getEffects where
        match (To t b _) = a == b && (not noSelf || t /= Self)
        match _ = false
instance _d_ ∷ MatchServant Trait where
    has a _ (Servant s) = a `elem` s.traits
instance _e_ ∷ MatchServant Alignment where
    has a _ (Servant {align:Tuple b c}) = a == b || a == c
instance _f_ ∷ MatchServant PhantasmType where
    has SingleTarget _ (Servant s) = any match $ phantasmEffects s.phantasm
      where
        match (To Enemy Damage _) = true
        match (To Enemy DamageThruDef _) = true
        match _ = false
    has MultiTarget _ (Servant s) = any match $ phantasmEffects s.phantasm
      where
        match (To Enemies Damage _) = true
        match (To Enemies DamageThruDef _) = true
        match _ = false
    has Support x s = not (has SingleTarget x s) && not (has MultiTarget x s)
instance _g_ ∷ MatchServant Class where
    has a _ (Servant s) = a == s.class
instance _h_ ∷ MatchServant Attribute where
    has a _ (Servant s) = a == s.attr
instance _i_ ∷ MatchServant Deck where
    has a _ (Servant s) = a == s.deck
instance _j_ ∷ MatchServant Card where
    has a _ (Servant s) = a == s.phantasm.card
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
