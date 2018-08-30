module Database.Servant where

import Prelude
import Operators

import Control.MonadZero
import Data.Array (head)
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

type Servant = { name     ∷ String
               , id       ∷ Int
               , rarity   ∷ Int
               , class    ∷ Class
               , attr     ∷ Attribute
               , deck     ∷ Deck
               , stats    ∷ Stats
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

data Card = Arts | Buster | Quick
data Deck = Deck Card Card Card Card Card

type Stat = { atk ∷ Int, hp ∷ Int }
type Stats = { base ∷ Stat, max ∷ Stat, grail ∷ Stat }
showStat ∷ Stat -> String
showStat {atk, hp} = "ATK: " ++ show atk ++ ", HP: " ++ show hp

hasPassive ∷ String -> Servant -> Boolean
hasPassive p = any (eq p) ∘ map (_.name) ∘ _.passives

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

simplify ∷ ActiveEffect -> ActiveEffect
simplify (Chances _ _ ef) = simplify ef
simplify (Chance _ ef)    = simplify ef
simplify (When _ ef)      = simplify ef
simplify ef               = ef

getEffects ∷ Servant -> Array ActiveEffect
getEffects {phantasm:{effect, over}, actives}
    = simplify <$> effect ++ over ++ (actives >>= _.effect)
phantasmEffects ∷ NoblePhantasm -> Array ActiveEffect
phantasmEffects {effect, over} = effect ++ over

npDamage ∷ Servant -> Number
npDamage s@{ phantasm:{card, effect, over, first}}
  | not ∘ any (notEq 0.0 ∘ dmg Humanoid toMin ∘ simplify) $ effect ++ over = 0.0
  | otherwise = fromMaybe 0.0 ∘ maximum $ npDamageVs s <$> enumArray

npDamageVs ∷ Servant -> Trait -> Number
npDamageVs s@{ phantasm:{card, effect, over, first}} t
  | any (notEq 0.0 ∘ dmg t toMin ∘ simplify) $ effect ++ over 
    = sum (flat toMax <$> overEfs ++ skillEfs)
    + sum (flat npStrength <$> npEfs)
    + cardBonus * toNumber s.stats.max.atk * classModifier s.class 
    * (mapSum (dmg t npStrength) npEfs + mapSum (dmg t toMax) overEfs)
    * mapSum (boost t npStrength) npEfs 
    * mapSum (boost t toMax) (guard first >> head overEfs)
    * mapSum (boost t toMax) skillEfs
  where
    skillEfs 
        = simplify <$> (s.actives >>= _.effect) ++ (s.passives >>= _.effect)
    npEfs = simplify <$> effect
    overEfs = simplify <$> over
    npStrength
      | s.free || s.rarity < 4 = toMax
      | otherwise              = toMin
    cardBonus = case card of
        Arts -> 1.0
        Buster -> 1.5
        Quick -> 0.8
  | otherwise = 0.0
    
mapSum ∷ ∀ f a. Foldable f => Functor f => (a -> Number) -> f a -> Number
mapSum f = max 1.0 ∘ sum ∘ map ((_ / 100.0) ∘ f)

flat ∷ (Amount -> Number) -> ActiveEffect -> Number
flat f (Grant t _ DamageUp a)
  | allied t = f a
flat f (Debuff t _ DamageVuln a)
  | not $ allied t = f a
flat _ _ = 0.0

dmg ∷ Trait -> (Amount -> Number) -> ActiveEffect -> Number
dmg _ f (To Enemy Damage a) = f a
dmg _ f (To Enemy DamageThruDef a) = f a
dmg _ f (To Enemies Damage a) = f a
dmg _ f (To Enemies DamageThruDef a) = f a
dmg t f (To _ (DamageVs t1) a) | t == t1 = f a
dmg _ _ _ = 0.0

boost ∷ Trait -> (Amount -> Number) -> ActiveEffect -> Number
boost t f (Grant targ _ buff a) 
  | allied targ = buffBoost buff
  where
    buffBoost ArtsUp = f a
    buffBoost AttackUp = f a
    buffBoost (AttackUpVs t1) | t == t1 = f a
    buffBoost BusterUp = f a
    buffBoost (DamageAffinity _) = f a
    buffBoost NPUp = f a
    buffBoost QuickUp = f a
    buffBoost _ = 0.0
boost t f (Debuff targ _ debuff a) 
  | not $ allied targ = debuffBoost debuff
  where
    debuffBoost DefenseDown = f a
    debuffBoost _ = 0.0
boost _ _ _ = 0.0

data PhantasmType = SingleTarget | MultiTarget | Support
instance _01_ ∷ Show PhantasmType where
  show = case _ of
    SingleTarget -> "Single-Target"
    MultiTarget  -> "Multi-Target"
    Support      -> "Support"


class (BoundedEnum a, Show a) <= MatchServant a where
    has ∷ a -> Boolean -> Servant -> Boolean
instance _a_ ∷ MatchServant BuffEffect where 
    has a exclude = any match ∘ getEffects where 
        match (Grant t _ b _) = a == b && allied t && (not exclude || t /= Self)
        match _ = false
instance _b_ ∷ MatchServant DebuffEffect where 
    has a _ = any match ∘ getEffects where 
        match (Debuff t _ b _) = a == b && not (allied t)
        match _ = false
instance _c_ ∷ MatchServant InstantEffect where 
    has DemeritBuffs  _ = const false
    has DemeritCharge _ = const false
    has DemeritDamage _ = const false
    has DemeritGauge  _ = const false
    has DemeritHealth _ = const false
    has DemeritKill   _ = const false
    has a exclude       = any match ∘ getEffects where 
        match (To t b _) = a == b && (not exclude || t /= Self)
        match _ = false
instance _d_ ∷ MatchServant Trait where 
    has a = const $ elem a ∘ _.traits
instance _e_ ∷ MatchServant Alignment where
    has a _ {align:(b:c)} = a == b || a == c
instance _f_ ∷ MatchServant PhantasmType where
    has SingleTarget _ {phantasm} = any match $ phantasmEffects phantasm
      where 
        match (To Enemy Damage _) = true
        match (To Enemy DamageThruDef _) = true
        match (To (EnemyType _) Damage _) = true
        match (To (EnemyType _) DamageThruDef _) = true
        match _ = false
    has MultiTarget _ {phantasm} = any match $ phantasmEffects phantasm
      where 
        match (To Enemies Damage _) = true
        match (To Enemies DamageThruDef _) = true
        match (To (EnemiesType _) Damage _) = true
        match (To (EnemiesType _) DamageThruDef _) = true
        match _ = false
    has Support x s = not (has SingleTarget x s) && not (has MultiTarget x s)
instance _g_ ∷ MatchServant Class where
    has a = const $ eq a ∘ _.class
instance _h_ ∷ MatchServant Attribute where
    has a = const $ eq a ∘ _.attr
instance _i_ ∷ MatchServant Deck where
    has a = const $ eq a ∘ _.deck
instance _j_ ∷ MatchServant Card where
    has a = const $ eq a ∘ _.phantasm.card
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
