module Database.Servant where

import Prelude
import Operators

import Data.Enum              
import Data.Foldable     
import Data.Generic.Rep
import Data.Generic.Rep.Bounded    
import Data.Generic.Rep.Enum      
import Data.Generic.Rep.Show 
import Data.Maybe              
import Data.Int                
import Data.String.CodeUnits   
import Data.Tuple             
 
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

data Card = Arts | Buster | Quick
data Deck = Deck Card Card Card Card Card

type Stat = { atk ∷ Int, hp ∷ Int }
type Stats = { base ∷ Stat, max ∷ Stat, grail ∷ Stat }
showStat ∷ Stat -> String
showStat {atk, hp} = "ATK: " ++ show atk ++ ", HP: " ++ show hp

hasPassive ∷ String -> Servant -> Boolean
hasPassive p (Servant {passives}) = any (eq p) $ (_.name) <$> passives

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

type Gen = { starWeight  ∷ Int
           , starRate    ∷ Number
           , npPerHit    ∷ Number
           , npPerDefend ∷ Int
           }

getEffects ∷ Servant -> Array ActiveEffect
getEffects (Servant {phantasm:{effect, over}, actives}) 
    = simplify <$> effect ++ over ++ (actives >>= _.effect)
  where
    simplify (Chance _ ef) = simplify ef
    simplify (When _ ef)   = simplify ef
    simplify ef            = ef
phantasmEffects ∷ Servant -> Array ActiveEffect
phantasmEffects (Servant {phantasm:{effect, over}}) = effect ++ over

npDamage ∷ Servant -> Number
npDamage (Servant s@{stats:{max:{atk}}, phantasm:{card, effect, over}}) 
    = cardBonus * toNumber atk * classModifier s.class 
    * sum (((_ / 100.0) ∘ dmg) <$> effect ++ over)
  where
    dmg (To Enemy Damage a) = a
    dmg (To Enemy DamageThruDef a) = a
    --dmg (To (EnemyType _) Damage a) = a
    --dmg (To (EnemyType _) DamageThruDef a) = a
    dmg (To Enemies Damage a) = 3.0 * a
    dmg (To Enemies DamageThruDef a) = 3.0 * a
    --dmg (To (EnemiesType _) Damage a) = 3.0 * a
    --dmg (To (EnemiesType _) DamageThruDef a) = 3.0 * a
    dmg _ = 0.0
    cardBonus = case card of
        Arts -> 1.0
        Buster -> 1.5
        Quick -> 0.8

data PhantasmType = SingleTarget | MultiTarget | Support
instance _01_ ∷ Show PhantasmType where
  show = case _ of
    SingleTarget -> "Single-Target"
    MultiTarget  -> "Multi-Target"
    Support      -> "Support"

class (BoundedEnum a, Show a) <= MatchServant a where
    has ∷ a -> Servant -> Boolean
instance _a_ ∷ MatchServant BuffEffect where 
    has a = any match ∘ getEffects where 
        match (Grant t _ b _) = a == b && allied t
        match _ = false
instance _b_ ∷ MatchServant DebuffEffect where 
    has a = any match ∘ getEffects where 
        match (Debuff t _ b _) = a == b && not (allied t)
        match _ = false
instance _c_ ∷ MatchServant InstantEffect where 
    has DemeritBuffs  = const false
    has DemeritCharge = const false
    has DemeritDamage = const false
    has DemeritGauge  = const false
    has DemeritHealth = const false
    has DemeritKill   = const false
    has a = any match ∘ getEffects where 
        match (To _ b _) = a == b
        match _ = false
instance _d_ ∷ MatchServant Trait where 
    has a (Servant {traits}) = a `elem` traits
instance _e_ ∷ MatchServant Alignment where
    has a (Servant {align:(b:c)}) = a == b || a == c
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
    has Support s = not has SingleTarget s && not has MultiTarget s
instance _g_ ∷ MatchServant Class where
    has a (Servant {class: cla}) = a == cla
instance _h_ ∷ MatchServant Attribute where
    has a (Servant {attr}) = a == attr
instance _i_ ∷ MatchServant Deck where
    has a (Servant {deck}) = a == deck

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

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

derive instance _27_ ∷ Eq Card
derive instance _28_ ∷ Ord Card
derive instance _29_ ∷ Generic Card _
instance _30_ ∷ Enum Card where
  succ = genericSucc
  pred = genericPred
instance _31_ ∷ Bounded Card where
  top = genericTop
  bottom = genericBottom
instance _32_ ∷ BoundedEnum Card where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
instance _33_ ∷ Show Card where
  show = genericShow

derive instance _34_ ∷ Eq Deck
derive instance _35_ ∷ Ord Deck
derive instance _36_ ∷ Generic Deck _
instance _37_ ∷ Enum Deck where
  succ = genericSucc
  pred = genericPred
instance _38_ ∷ Bounded Deck where
  top = genericTop
  bottom = genericBottom
instance _39_ ∷ BoundedEnum Deck where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
instance _40_ ∷ Show Deck where
  show (Deck a b c d e) = fromCharArray 
                        $ (fromMaybe '?' ∘ charAt 0 ∘ show) <$> [a, b, c, d, e]
