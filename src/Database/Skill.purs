module Database.Skill where

import Prelude
import Operators

import Data.Enum          
import Data.Number.Format      
import Data.Generic.Rep    
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum 
import Data.Maybe
import Data.String            
import Data.Tuple             

import Database.Icon
import Database.Trait

outputNumber ∷ Number → String
outputNumber (-1.0) = "X"
outputNumber x = toString x

data Rank = Unknown | EX   
          | APlusPlus | APlus | A 
          | BPlusPlus | BPlus | B | BMinus 
          | CPlusPlus | CPlus | C | CMinus
                      | DPlus | D 
                      | EPlus | E | EMinus
instance _a_ ∷ Show Rank where
  show = case _ of
    Unknown   → "--"
    EX        → "EX"
    APlusPlus → "A++"
    APlus     → "A+"
    A         → "A"
    BPlusPlus → "B++"
    BPlus     → "B+"
    B         → "B"
    BMinus    → "B-"
    CPlusPlus → "C++"
    CPlus     → "C+"
    C         → "C"
    CMinus    → "C-"
    DPlus     → "D+"
    D         → "D"
    EPlus     → "E+"
    E         → "E"
    EMinus    → "E-"

data Target = Someone
            | Self | Ally | Allies | Party | Enemy | Enemies | Others
            | AlliesType Trait | EnemyType Trait | EnemiesType Trait
derive instance _00_ ∷ Eq Target

allied ∷ Target → Boolean
allied Self = true
allied Ally = true
allied Allies = true
allied Party = true
allied Others = true
allied (AlliesType _) = true
allied _ = false

possessiveAndSubject ∷ Target → Tuple String String
possessiveAndSubject = case _ of
    Someone       → ""            
                  : ""
    Self          → "own"         
                  : "self"
    Ally          → "one ally's"  
                  : "one ally"
    Allies        → "all allies'" 
                  : "allies"
    Party         → "party's"       
                  : "party"
    Enemy         → "one enemy's" 
                  : "one enemy"
    Enemies       → "all enemy"   
                  : "all enemies"
    Others        → "allies' (excluding self)" 
                  : "allies (excluding self)"
    AlliesType t    → show t ⧺ " allies'" 
                  : show t ⧺ " allies"
    EnemyType t   → "one " ⧺ show t ⧺ " enemy's" 
                  : "one " ⧺ show t ⧺ " enemy"
    EnemiesType t → "all " ⧺ show t ⧺ " enemy" 
                  : "all " ⧺ show t ⧺ " enemies"
    
times ∷ Number → String
times (-1.0) = ""
times   0.0  = ""
times   1.0  = " (1 time)"
times amt    = " (" ⧺ outputNumber amt ⧺ " times)"

data BuffEffect = ArtsUp
                | AttackUp
                | AttackUpVs Trait
                | BuffUp
                | BurnImmunity
                | BusterUp 
                | CharmResist 
                | CritUp 
                | DamageAffinity Class
                | DamageCut
                | DamageUp
                | DebuffResist 
                | DebuffImmunity
                | DebuffSuccess
                | DefenseUp
                | DefenseUpVs Trait           
                | Evasion
                | GaugePerTurn 
                | Guts
                | HealPerTurn
                | HealingReceived
                | HealUp
                | IgnoreInvinc
                | Invincibility
                | KillChance
                | KillImmunity
                | KillResist
                | KillUp
                | MaxHP
                | MentalResist
                | MentalSuccess
                | NPUp
                | NPFromDamage
                | NPGen
                | Overcharge
                | PoisonResist
                | QuickUp
                | ReduceDamage
                | StarAbsorb
                | StarDrop
                | StarUp
                | StarsPerTurn
                | StunSuccess
                | SureHit
                | Taunt
instance _b_ ∷ Show BuffEffect where
    show = showBuff Someone (-1.0)

showBuff ∷ Target → Number → BuffEffect → String
showBuff target amount buff = case buff of
    ArtsUp           → "Increase " ⧺ p ⧺ " Arts performance " ⧺ by
    AttackUp         → "Increase " ⧺ p ⧺ " attack " ⧺ by
    AttackUpVs    t  → "Increase " ⧺ p ⧺ " attack against " ⧺ show t 
                     ⧺ " enemies " ⧺ by
    BuffUp           → "Increase " ⧺ p ⧺ " buff success rate " ⧺ by
    BurnImmunity     → "Grant " ⧺ s ⧺ " Burn Immunity" ⧺ times amount
    BusterUp         → "Increase " ⧺ p ⧺ " Buster performance " ⧺ by
    CharmResist      → "Increase " ⧺ p ⧺ " Charm resistance " ⧺ by
    CritUp           → "Increase " ⧺ p ⧺ " critical damage " ⧺ by
    DamageAffinity c → "Increase " ⧺ p ⧺ " damage against " ⧺ show c 
                     ⧺ " enemies " ⧺ by
    DamageCut        → "Reduce " ⧺ p ⧺ " damage taken by " ⧺ n ⧺ " for 1 attack"
    DamageUp         → "Increase " ⧺ p ⧺ " damage by " ⧺ n
    DebuffImmunity   → "Grant " ⧺ s ⧺ " Debuff Immunity" ⧺ times amount
    DebuffResist     → "Increase " ⧺ p ⧺ " debuff resistance " ⧺ by
    DebuffSuccess    → "Increase " ⧺ p ⧺ " debuff success rate " ⧺ by
    DefenseUp        → "Increase " ⧺ p ⧺ " defense " ⧺ by
    DefenseUpVs   t  → "Increase " ⧺ p ⧺ " defense against " ⧺ show t 
                     ⧺ " enemies " ⧺ by
    Evasion          → "Grant " ⧺ s ⧺ " Evasion" ⧺ times amount
    GaugePerTurn     → "Charge " ⧺ p ⧺ " NP gauge " ⧺ by ⧺ " every turn"
    Guts             → "Grant " ⧺ s ⧺ " Guts" ⧺ times amount
    HealingReceived  → "Increase " ⧺ p ⧺ " healing received " ⧺ by
    HealPerTurn      → "Restore " ⧺ n ⧺ " health" ⧺ to ⧺ " every turn"
    HealUp           → "Increase " ⧺ n ⧺ " healing power " ⧺ by
    IgnoreInvinc     → "Ignore Invincibility" ⧺ times amount
    Invincibility    → "Grant " ⧺ s ⧺ " Invincibility" ⧺ times amount
    KillChance       → n ⧺ "% chance to Instant-Kill enemy with normal attacks"
    KillImmunity     → "Grant " ⧺ s ⧺ " Instant-Kill Immunity" ⧺ times amount
    KillResist       → "Increase " ⧺ p ⧺ " Instant-Kill resistance " ⧺ by
    KillUp           → "Increase " ⧺ p ⧺ " Instant-Kill success rate " ⧺ by
    MaxHP            → "Increase " ⧺ p ⧺ " Max HP by " ⧺ n
    MentalResist     → "Increase " ⧺ p ⧺ " mental debuff resistance " ⧺ by
    MentalSuccess    → "Increase " ⧺ p ⧺ " mental debuff success rate " ⧺ by
    NPUp             → "Increase " ⧺ p ⧺ " NP Damage " ⧺ by
    NPFromDamage     → "Increase " ⧺ p 
                     ⧺ " NP generation rate when taking damage " ⧺  by
    NPGen            → "Increase " ⧺ p ⧺ " NP generation rate " ⧺ by
    Overcharge       → "Overcharge " ⧺ p ⧺ " NP by " ⧺ n ⧺ " stages"
    PoisonResist     → "Increase " ⧺ p ⧺ " poison debuff resistance " ⧺ by
    QuickUp          → "Increase " ⧺ p ⧺ " Quick performance " ⧺ by
    ReduceDamage     → "Reduce " ⧺ p ⧺ " damage taken by " ⧺ n
    StarAbsorb       → "Increase " ⧺ p ⧺ " critical star absorption " ⧺ by
    StarDrop         → "Increase C. Star Drop Rate for " ⧺ s ⧺ " " ⧺ by
    StarUp           →  "Increase " ⧺ p ⧺ " critical star generation rate " ⧺ by
    StarsPerTurn     → "Gain " ⧺ n ⧺ " stars every turn"
    StunSuccess      → "Increase " ⧺ p ⧺ " Stun success rate for 1 time " ⧺ by
    SureHit          → "Grant " ⧺ s ⧺ " Sure Hit" ⧺ times amount
    Taunt            → "Draw attention of all enemies" ⧺ to
  where 
    n     = outputNumber amount
    p:s   = possessiveAndSubject target
    to    = if s ≡ "" then "" else " to " ⧺ s
    by    = " by " ⧺ n ⧺ "%"
       
data DebuffEffect = AttackDown
                  | BuffBlock
                  | BuffFail
                  | Burn
                  | Charm
                  | CritChance
                  | CritDown
                  | Curse
                  | DamageVuln
                  | DeathDown
                  | DebuffVuln
                  | DefenseDown
                  | Disorder
                  | NPDown
                  | Poison
                  | SealNP
                  | SealSkills
                  | Stun
                  | StunBomb
                  | Terror
instance _c_ ∷ Show DebuffEffect where
    show = showDebuff Someone (-1.0)

showDebuff ∷ Target → Number → DebuffEffect → String
showDebuff target amount debuff = case debuff of
    AttackDown  → "Reduce " ⧺ p ⧺ " attack by " ⧺ n ⧺ "%"
    BuffBlock   → "Inflict Buff Block status" ⧺ to ⧺ times amount
    BuffFail    → "Reduce " ⧺ p ⧺ " attack buff success rate by " ⧺ n ⧺ "%"
    Burn        → "Inflict " ⧺ n ⧺ " Burn damage" ⧺ to
    Charm       → "Charm " ⧺ s
    CritChance  → "Reduce " ⧺ p ⧺ " critical attack chance by " ⧺ n ⧺ "%"
    CritDown    → "Reduce " ⧺ p ⧺ " critical damage by " ⧺ n ⧺ " %"
    Curse       → "Inflict " ⧺ n ⧺ " Curse damage" ⧺ to
    DamageVuln  → "Increase " ⧺ s ⧺ " damage taken by " ⧺ n
    DeathDown   → "Reduce " ⧺ p ⧺ " Instant-Death resistance by " ⧺ n ⧺ "%"
    DebuffVuln  → "Reduce " ⧺ p ⧺ " debuff resistance by " ⧺ n ⧺ "%"
    DefenseDown → "Reduce " ⧺ p ⧺  " defense by " ⧺ n ⧺ "%"
    Disorder    → "Inflict Disorder status" ⧺ to 
                ⧺ ", causing " ⧺ n ⧺ "% chance to Seal skills every turn"
    NPDown      → "Decrease " ⧺ p ⧺ " Noble Phantasm damage by " ⧺ n ⧺ "%"
    Poison      → "Inflict " ⧺ n ⧺ " Poison damage" ⧺ to
    SealNP      → "Seal " ⧺ p ⧺ " NP"
    SealSkills  → "Seal " ⧺ p ⧺ " skills"
    Stun        → "Stun " ⧺ s
    StunBomb    → "Stun " ⧺ s ⧺ " after 1 turn"
    Terror      → "Inflict Terror status" ⧺ to ⧺ ", causing " ⧺ n 
                ⧺ "% chance to be Stunned every turn"
  where 
    n   = outputNumber amount
    p:s = possessiveAndSubject target
    to  = if s ≡ "" then "" else " to " ⧺ s
 
data InstantEffect = GaugeDown
                   | Cooldowns
                   | Cure
                   | Damage
                   | DamageThruDef
                   | DemeritBuffs
                   | DemeritCharge
                   | DemeritDamage
                   | DemeritGauge
                   | DemeritHealth
                   | DemeritKill
                   | GainStars
                   | GaugeUp
                   | Heal
                   | HealToFull
                   | Kill
                   | RemoveBuffs
                   | RemoveDebuffs
                   | RemoveMental
instance _d_ ∷ Show InstantEffect where
    show = showInstant Someone (-1.0)

showInstant ∷ Target → Number → InstantEffect → String
showInstant target amount instant = case instant of
    Cooldowns     → "Reduce " ⧺ p ⧺ " cooldowns by " ⧺ n
    Cure          → "Remove " ⧺ p ⧺ " poison debuffs"
    GaugeDown     → "Reduce " ⧺ p ⧺ " NP gauge by " ⧺ n
    Damage        → "Deal " ⧺ n ⧺ "% damage" ⧺ to
    DamageThruDef → "Deal " ⧺ n ⧺ "% damage" ⧺ to ⧺ ", ignoring defense"
    DemeritBuffs  → "Removes " ⧺ p ⧺ " buffs [Demerit]"
    DemeritCharge → "Increase " ⧺ s ⧺ " NP gauge by " ⧺ n ⧺ " [Demerit]"
    DemeritGauge  → "Decrease " ⧺ p ⧺ " NP gauge by " ⧺ n ⧺ "% [Demerit]"
    DemeritDamage → "Deal " ⧺ n ⧺ " damage" ⧺ to ⧺ " [Demerit]"
    DemeritKill   → "Sacrifice " ⧺ s ⧺ " (can trigger Guts) [Demerit]"
    DemeritHealth → "Deal " ⧺ n ⧺ " damage" ⧺ to ⧺ " down to a minimum of 1 [Demerit]"
    GaugeUp       → "Increase " ⧺ p ⧺ " NP gauge by " ⧺ n ⧺ "%"
    Heal          → "Restore " ⧺ n ⧺ " health" ⧺ to
    HealToFull    → "Heal " ⧺ s ⧺ " to full"
    RemoveBuffs   → "Remove " ⧺ p ⧺ " buffs"
    RemoveDebuffs → "Remove " ⧺ p ⧺ " debuffs"
    RemoveMental  → "Remove " ⧺ p ⧺ " mental debuffs"
    Kill          → n ⧺ "% chance to Instant-Kill " ⧺ s
    GainStars     → "Gain " ⧺ n ⧺ " critical stars" ⧺ case target of
        Self → " for yourself"
        _    → ""
  where
    n   = outputNumber amount
    p:s = possessiveAndSubject target
    to  = if s ≡ "" then "" else " to " ⧺ s

-- | Int field is duration, Number field is amount
data ActiveEffect = Grant Target Int BuffEffect Number
                  | Debuff Target Int DebuffEffect Number
                  | To Target InstantEffect Number
                  | Chance Int ActiveEffect

{-
activeVal ∷ ActiveEffect → Number
activeVal (Grant _ _ _ val)  = val
activeVal (Debuff _ _ _ val) = val
activeVal (To _ _ val)       = val
activeVal (Chance _ active)  = activeVal active
-}

instance _01_ ∷ Eq ActiveEffect where
  eq activeA activeB = case activeA, activeB of
      Grant ta _ a _,  Grant tb _ b _  → eq a b ∧ eq' ta tb
      Debuff ta _ a _, Debuff tb _ b _ → eq a b ∧ eq' ta tb
      To ta a _,       To tb b _       → eq a b ∧ eq' ta tb
      Chance _ a,      Chance _ b      → eq a b 
      _,               _               → false
    where 
      eq' ta tb = ta ≡ tb ∨ ta ≡ Someone ∨ tb ≡ Someone

instance _e_ ∷ Show ActiveEffect where
  show = case _ of
      Grant t dur buff amt    → showBuff t amt buff ⧺ turns dur
                              ⧺ if not allied t then " [Demerit]." else "."
      Debuff t dur debuff amt → showDebuff t amt debuff ⧺ turns dur
                              ⧺ if allied t then " [Demerit]." else "."
      To t instant amt        → showInstant t amt instant ⧺ "."
      Chance per ef           → show per ⧺ "% chance to " ⧺ show ef
    where
      turns 0   = ""
      turns 1   = " for 1 turn"
      turns dur = " for " ⧺ show dur ⧺ " turns"

uncap ∷ String → String
uncap s = case uncons s of
    Nothing → s
    Just {head: x, tail: xs} → toLower (singleton x) ⧺ xs

type Active = { name   ∷ String 
              , icon   ∷ Icon
              , cd     ∷ Int
              , effect ∷ Array ActiveEffect
              }

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Generic BuffEffect _
derive instance _1_ ∷ Eq BuffEffect
derive instance _2_ ∷ Ord BuffEffect
instance _3_ ∷ Enum BuffEffect where
  succ = genericSucc
  pred = genericPred
instance _4_ ∷ Bounded BuffEffect where
  top = genericTop
  bottom = genericBottom
instance _5_ ∷ BoundedEnum BuffEffect where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance _6_ ∷ Eq Rank
derive instance _7_ ∷ Ord Rank

derive instance _8_ ∷ Generic DebuffEffect _
derive instance _9_ ∷ Eq DebuffEffect
derive instance _10_ ∷ Ord DebuffEffect
instance _11_ ∷ Enum DebuffEffect where
  succ = genericSucc
  pred = genericPred
instance _12_ ∷ Bounded DebuffEffect where
  top = genericTop
  bottom = genericBottom
instance _13_ ∷ BoundedEnum DebuffEffect where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance _14_ ∷ Generic InstantEffect _
derive instance _15_ ∷ Eq InstantEffect
derive instance _16_ ∷ Ord InstantEffect
instance _17_ ∷ Enum InstantEffect where
  succ = genericSucc
  pred = genericPred
instance _18_ ∷ Bounded InstantEffect where
  top = genericTop
  bottom = genericBottom
instance _19_ ∷ BoundedEnum InstantEffect where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
