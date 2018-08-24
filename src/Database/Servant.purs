module Database.Servant where

import Prelude
import Operators

import Data.Enum              
import Data.Foldable     
import Data.Generic.Rep
import Data.Generic.Rep.Bounded    
import Data.Generic.Rep.Enum      
import Data.Generic.Rep.Show 
import Data.Tuple (Tuple)

import Database.Skill
import Database.Passive
import Database.Trait

newtype Servant = Servant { name     ∷ String
                          , rarity   ∷ Int
                          , class    ∷ Class
                          , attr     ∷ Attribute
                          , deck     ∷ Deck
                          , stats    ∷ Stats
                          , ratings  ∷ Ratings
                          , actives  ∷ Array Active
                          , passives ∷ Array Passive
                          , phantasm ∷ NoblePhantasm
                          , gen      ∷ Gen
                          , hits     ∷ Hits
                          , traits   ∷ Array Trait
                          , death    ∷ Number
                          , align    ∷ Tuple Alignment Alignment
                          }

data Attribute = Mankind | Earth | Heaven | Star

data Card = Arts | Buster | Quick
data Deck = Deck Card Card Card Card Card

type Stat = { atk ∷ Int, hp ∷ Int }
type Stats = { base ∷ Stat, max ∷ Stat, grail ∷ Stat }
showStat ∷ Stat → String
showStat {atk, hp} = "ATK: " ⧺ show atk ⧺ ", HP: " ⧺ show hp

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
                     }

type Hits = { a ∷ Int, b ∷ Int, q ∷ Int, ex ∷ Int }

type Gen = { starAbsorb ∷ Int
           , starGen    ∷ Number
           , npPerHit   ∷ Number
           , npAttacked ∷ Int
           }

data Alignment = Lawful | Neutral | Chaotic | Good | Evil 
               | Mad | Summer | Bride

showAlignment ∷ Tuple Alignment Alignment → String
showAlignment = case _ of
    Neutral:Neutral → "True Neutral"
    a:b             → show a ⧺ " " ⧺ show b

getEffects ∷ Servant → Array ActiveEffect
getEffects (Servant {phantasm:{effect, over}, actives}) 
    = effect ⧺ over ⧺ (actives ≫= _.effect)

phantasmEffects ∷ Servant → Array ActiveEffect
phantasmEffects (Servant {phantasm:{effect, over}}) = effect ⧺ over

data PhantasmType = SingleTarget | MultiTarget | Support
instance _01_ ∷ Show PhantasmType where
  show = case _ of
    SingleTarget → "Single-Target"
    MultiTarget  → "Multi-Target"
    Support      → "Support"

class (BoundedEnum a, Show a) <= MatchServant a where
    has ∷ a → Servant → Boolean
instance _a_ ∷ MatchServant BuffEffect where 
    has a = any match ∘ getEffects where 
        match (Grant _ _ b _) = a ≡ b
        match _ = false
instance _b_ ∷ MatchServant DebuffEffect where 
    has a = any match ∘ getEffects where 
        match (Debuff t _ b _) = a ≡ b ∧ not (allied t)
        match _ = false
instance _c_ ∷ MatchServant InstantEffect where 
    has a = any match ∘ getEffects where 
        match (To _ b _) = a ≡ b
        match _ = false
instance _d_ ∷ MatchServant Trait where 
    has a (Servant {traits}) = a ∈ traits
instance _e_ ∷ MatchServant Alignment where
    has a (Servant {align:(b:c)}) = a ≡ b ∨ a ≡ c
instance _f_ ∷ MatchServant PhantasmType where
    has SingleTarget s = any match $ phantasmEffects s
      where 
        match (To Enemy Damage _) = true
        match (To Enemy DamageThruDef _) = true
        match (To (EnemyType _) Damage _) = true
        match (To (EnemyType _) DamageThruDef _) = true
        match _ = false
    has MultiTarget s = any match $ phantasmEffects s
      where 
        match (To Enemies Damage _) = true
        match (To Enemies DamageThruDef _) = true
        match (To (EnemiesType _) Damage _) = true
        match (To (EnemiesType _) DamageThruDef _) = true
        match _ = false
    has Support s = not has SingleTarget s ∧ not has MultiTarget s
instance _g_ ∷ MatchServant Class where
    has a (Servant {class: cla}) = a ≡ cla
instance _h_ ∷ MatchServant Attribute where
    has a (Servant {attr}) = a ≡ attr

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Eq Alignment
derive instance _1_ ∷ Ord Alignment
derive instance _2_ ∷ Generic Alignment _
instance _3_ ∷ Show Alignment where
  show = genericShow
instance _4_ ∷ Enum Alignment where
  succ = genericSucc
  pred = genericPred
instance _5_ ∷ Bounded Alignment where
  top = genericTop
  bottom = genericBottom
instance _6_ ∷ BoundedEnum Alignment where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance _7_ ∷ Eq PhantasmType
derive instance _8_ ∷ Ord PhantasmType
derive instance _9_ ∷ Generic PhantasmType _
instance _10_ ∷ Enum PhantasmType where
  succ = genericSucc
  pred = genericPred
instance _11_ ∷ Bounded PhantasmType where
  top = genericTop
  bottom = genericBottom
instance _12_ ∷ BoundedEnum PhantasmType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance _20_ ∷ Eq Attribute
derive instance _21_ ∷ Ord Attribute
derive instance _22_ ∷ Generic Attribute _
instance _23_ ∷ Show Attribute where
  show = genericShow
instance _24_ ∷ Enum Attribute where
  succ = genericSucc
  pred = genericPred
instance _25_ ∷ Bounded Attribute where
  top = genericTop
  bottom = genericBottom
instance _26_ ∷ BoundedEnum Attribute where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance _27_ ∷ Generic Card _
instance _28_ ∷ Show Card where
  show = genericShow
