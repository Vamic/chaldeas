-- | This module defines the data structure of Servants.
-- It does not contain actual database information from the game; that can
-- be found in the [Database.Servant](./Servant) _folder_, which has a
-- separate file for each Class.
module Database.Servant
  ( Servant(..), maxLevel
  , Deck(..)
  , NoblePhantasm
  , Gen
  , Hits
  , PhantasmType(..)
  , class MatchServant, has
  , getDeck
  ) where

import StandardLibrary
import Data.String.CodeUnits as CodeUnits
import Generic               as G

import Database.Base
import Database.Skill

newtype Servant =
    Servant { name     :: String
            , id       :: Int
            , rarity   :: Int
            , class    :: Class
            , attr     :: Attribute
            , deck     :: Deck
            , curve    :: Int
            , stats    :: { base :: Stat, max :: Stat, grail :: Stat }
            , skills   :: Array Skill
            , passives :: Array Skill
            , phantasm :: NoblePhantasm
            , gen      :: Gen
            , hits     :: Hits
            , traits   :: Array Trait
            , death    :: Number
            , align    :: Array Alignment
            , limited  :: Boolean
            , free     :: Boolean
            }

instance _0_ :: Show Servant where
    show (Servant s) = s.name
instance _1_ :: Eq Servant where
    eq (Servant x) (Servant y) = eq x.id y.id
instance _2_ :: Ord Servant where
    compare = comparing \(Servant x) -> x.id

data Deck = Deck Card Card Card Card Card

data Ratings = Ratings { damage     :: Int
                        , np         :: Int
                        , critical   :: Int
                        , utility    :: Int
                        , support    :: Int
                        , durability :: Int
                        }

type NoblePhantasm = { name   :: String
                     , desc   :: String
                     , rank   :: Rank
                     , card   :: Card
                     , kind   :: String
                     , hits   :: Int
                     , effect :: Array SkillEffect
                     , over   :: Array SkillEffect
                     , first  :: Boolean
                     }

type Hits = { arts :: Int, buster :: Int, quick :: Int, ex :: Int }

type Gen = { starWeight :: Int
           , starRate   :: Number
           , npAtk      :: Number
           , npDef      :: Int
           }

-- | Returns all `Card`s in a `Servant`'s `Deck`. Does not include NP card.
getDeck :: Servant -> Array Card
getDeck (Servant {deck:Deck a b c d e}) = [a, b, c, d, e]

getEffects :: Servant -> Array SkillEffect
getEffects (Servant s) = filter (not <<< demerit) $ simplify
                         <$> s.phantasm.effect
                          <> s.phantasm.over
                          <> (s.skills >>= _.effect)

phantasmEffects :: NoblePhantasm -> Array SkillEffect
phantasmEffects {effect, over} = effect <> over

data PhantasmType = SingleTarget | MultiTarget | Support
instance _01_ :: Show PhantasmType where
      show SingleTarget = "Single-Target"
      show MultiTarget  = "Multi-Target"
      show Support      = "Support"

maxLevel :: Servant -> Int
maxLevel (Servant {rarity: 5}) = 90
maxLevel (Servant {rarity: 4}) = 80
maxLevel (Servant {rarity: 3}) = 70
maxLevel (Servant {rarity: 2}) = 65
maxLevel (Servant {rarity: 1}) = 60
maxLevel (Servant {rarity: _}) = 65

class (G.BoundedEnum a, Show a) <= MatchServant a where
    has :: a -> Boolean -> Servant -> Boolean
instance _a_ :: MatchServant BuffEffect where
    has x noSelf = any match <<< getEffects where
        match (Grant t _ y _) = x == y && (not noSelf || t /= Self)
        match _ = false
instance _b_ :: MatchServant DebuffEffect where
    has x _ = any match <<< getEffects where
        match (Debuff t _ y _) = x == y
        match _ = false
instance _c_ :: MatchServant InstantEffect where
    has BecomeHyde _ = const false
    has x noSelf     = any match <<< getEffects where
        match (To t y _) = x == y && (not noSelf || t /= Self)
        match _ = false
instance _d_ :: MatchServant Trait where
    has x _ (Servant s) = x `elem` s.traits
instance _e_ :: MatchServant Alignment where
    has x _ (Servant s) = x `elem` s.align
instance _f_ :: MatchServant PhantasmType where
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
instance _g_ :: MatchServant Class where
    has x _ (Servant s) = x == s.class
instance _h_ :: MatchServant Attribute where
    has x _ (Servant s) = x == s.attr
instance _i_ :: MatchServant Deck where
    has x _ (Servant s) = x == s.deck
instance _j_ :: MatchServant Card where
    has x _ (Servant s) = x == s.phantasm.card
-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _7_ :: Eq PhantasmType
derive instance _8_ :: Ord PhantasmType
derive instance _9_ :: G.Generic PhantasmType _
instance _10_ :: G.Enum PhantasmType where
    succ = G.genericSucc
    pred = G.genericPred
instance _11_ :: G.Bounded PhantasmType where
    top = G.genericTop
    bottom = G.genericBottom
instance _12_ :: G.BoundedEnum PhantasmType where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _34_ :: Eq Deck
derive instance _35_ :: Ord Deck
derive instance _36_ :: G.Generic Deck _
instance _37_ :: G.Enum Deck where
    succ = G.genericSucc
    pred = G.genericPred
instance _38_ :: G.Bounded Deck where
    top = G.genericTop
    bottom = G.genericBottom
instance _39_ :: G.BoundedEnum Deck where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum
instance _40_ :: Show Deck where
    show (Deck a b c d e) = CodeUnits.fromCharArray $
      (fromMaybe '?' <<< CodeUnits.charAt 0 <<< show) <$> [a, b, c, d, e]
