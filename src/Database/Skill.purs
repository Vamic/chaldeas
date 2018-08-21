module Database.Skill where

import Prelude
import Operators

import Data.Enum          
import Data.Generic.Rep    
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum 
import Data.Number.Format 
import Data.String              (joinWith)
import Data.Tuple               (Tuple)

import Database.Icon
import Database.Trait

outputNumber ∷ Number → String
outputNumber (-1.0) = "X"
outputNumber x = toString x

data Rank = Unknown | EX   
          | APlusPlus | APlus | A 
          | BPlusPlus | BPlus | B | BMinus 
                      | CPlus | C | CMinus
                      | DPlus | D 
                      | EPlus | E | EMinus
instance showRank ∷ Show Rank where
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
            | AllyType Trait | EnemyType Trait
derive instance eqTarget ∷ Eq Target

possessiveAndSubject ∷ Target → Tuple String String
possessiveAndSubject = case _ of
    Someone     → ""            : ""
    Self        → "own"         : "self"
    Ally        → "one ally's"  : "one ally"
    Allies      → "all allies'" : "allies"
    Party       → "party"       : "party"
    Enemy       → "one enemy's" : "one enemy"
    Enemies     → "all enemy"   : "all enemies"
    Others      → "allies'"     : "allies"
    AllyType t  → show t ⧺ " allies'" : show t ⧺ " allies"
    EnemyType t → "all " ⧺ show t ⧺ " enemy" : "all " ⧺ show t ⧺ " enemies"

times ∷ Number → String
times 0.0 = ""
times 1.0 = " (1 time)"
times amt = " (" ⧺ outputNumber amt ⧺ " times)"

data BuffEffect = ArtsUp
                | AttackUp
                | BuffUp
                | BurnImmunity
                | BusterUp 
                | CharmResist 
                | CritUp 
                | DamageUp
                | DamageUpVs Trait
                | DebuffResist 
                | DebuffImmunity
                | DebuffSuccess
                | DefenseUp
                | DefenseUpVs Trait
                | DemeritDebuff 
                | Evasion
                | Guts
                | HealPerTurn
                | HealingReceived
                | IgnoreInvinc
                | Invincibility
                | KillChance
                | KillImmunity
                | KillResist
                | MentalResist
                | NPUp
                | NPFromDamage
                | NPGen
                | GaugePerTurn 
                | QuickUp
                | ReduceDamage
                | StarAbsorb
                | StarDrop
                | StarGen
                | StarsPerTurn
                | Taunt
instance showBuffEffect ∷ Show BuffEffect where
    show = showBuff Someone (-1.0)

showBuff ∷ Target → Number → BuffEffect → String
showBuff target amount buff = case buff of
    ArtsUp          → "Increases " ⧺ p ⧺ " Arts performance " ⧺ by
    AttackUp        → "Increases " ⧺ p ⧺ " attack " ⧺ by
    BuffUp          → "Increases " ⧺ p ⧺ " buff success rate " ⧺ by
    BurnImmunity    → "Grants " ⧺ s ⧺ " Burn Immunity"
    BusterUp        → "Increases " ⧺ p ⧺ " Buster performance " ⧺ by
    CharmResist     → "Increases " ⧺ p ⧺ " Charm resistance " ⧺ by
    CritUp          → "Increases " ⧺ p ⧺ " critical damage " ⧺ by
    DamageUp        → "Increases " ⧺ p ⧺ " own damage " ⧺ by
    DamageUpVs    t → "Increases " ⧺ p ⧺ " attack against " ⧺ show t ⧺ " enemies " ⧺ by
    DebuffImmunity  → "Grants " ⧺ s ⧺ " Debuff Immunity"
    DebuffResist    → "Increases " ⧺ p ⧺ " debuff resistance " ⧺ by
    DebuffSuccess   → "Increases " ⧺ p ⧺ " debuff success rate " ⧺ by
    DefenseUp       → "Increases " ⧺ p ⧺ " defense " ⧺ by
    DefenseUpVs   t → "Increases " ⧺ p ⧺ " defense against " ⧺ show t ⧺ " enemies " ⧺ by
    DemeritDebuff   → "Reduces " ⧺ p ⧺ " resistance " ⧺ by ⧺ " [Demerit]"
    Evasion         → "Grants self Evasion" ⧺ times amount
    GaugePerTurn    → "Charges " ⧺ p ⧺ " NP gauge " ⧺ by ⧺ " every turn"
    Guts            → "Grants " ⧺ s ⧺ " Guts status" ⧺ times amount
    HealingReceived → "Increases " ⧺ p ⧺ " healing received " ⧺ by
    HealPerTurn     → "Restores " ⧺ n ⧺ " health" ⧺ to ⧺ " every turn"
    IgnoreInvinc    → "Ignores Invincibility"
    Invincibility   → "Grants " ⧺ s ⧺ " Invincibility"
    KillChance      → n ⧺ "% Chance to Instant-Kill enemy with normal attack"
    KillImmunity    → "Grants " ⧺ s ⧺ " Instant-Kill Immunity"
    KillResist      → "Increases " ⧺ p ⧺ " Instant-Kill resistance " ⧺ by
    MentalResist    → "Increases " ⧺ p ⧺ " mental debuff resistance " ⧺ by
    NPUp            → "Increases " ⧺ p ⧺ " NP Damage " ⧺ by
    NPFromDamage    → "Increases " ⧺ p ⧺ " NP generation rate when taking damage " ⧺  by
    NPGen           → "Increases " ⧺ p ⧺ " NP generation rate"
    QuickUp         → "Increases " ⧺ p ⧺ " Quick performance " ⧺ by
    ReduceDamage    → "Reduces " ⧺ p ⧺ " damage taken"
    StarAbsorb      → "Increases " ⧺ p ⧺ " critical star absorption"
    StarDrop        → "Increases C. Star Drop Rate for " ⧺ s ⧺ " " ⧺ by
    StarGen         → "Increases " ⧺ p ⧺ " critical star generation rate " ⧺ by
    StarsPerTurn    → "Gains " ⧺ n ⧺ " stars every turn"
    Taunt           → "Draws attention of all enemies" ⧺ to
  where 
    n     = outputNumber amount
    p:s   = possessiveAndSubject target
    to    = if s ≡ "" then "" else " to " ⧺ s
    by    = " by " ⧺ n ⧺ "%"
       
data DebuffEffect = AttackDown
                  | BuffBlock
                  | Burn
                  | Charm
                  | CritDown
                  | Curse
                  | DebuffVuln
                  | DefenseDown
                  | NPDown
                  | SealNP
                  | Stun
                  | StunChance
instance showDebuffEffect ∷ Show DebuffEffect where
    show = showDebuff Someone (-1.0)

showDebuff ∷ Target → Number → DebuffEffect → String
showDebuff target amount debuff = case debuff of
    AttackDown  → "Reduces " ⧺ p ⧺ " attack by " ⧺ n ⧺ "%"
    BuffBlock   → "Inflicts Buff Block status" ⧺ to ⧺ times amount
    Burn        → "Inflicts " ⧺ n ⧺ " Burn damage" ⧺ to
    Charm       → "Charm " ⧺ s
    CritDown    → "Reduces " ⧺ p ⧺ " critical attack chance by " ⧺ n ⧺ "%"
    Curse       → "Apply " ⧺ n ⧺ " Curse damage" ⧺ to
    DebuffVuln  → "Reduces " ⧺ p ⧺ " debuff resistance by " ⧺ n ⧺ "%"
    DefenseDown → "Reduce " ⧺ p ⧺  " defense by " ⧺ n ⧺ "%"
    NPDown      → "Decrease " ⧺ p ⧺ " Noble Phantasm damage by " ⧺ n ⧺ "%"
    SealNP      → "Seal " ⧺ p ⧺ " NP"
    Stun        → "Stuns " ⧺ s
    StunChance  → n ⧺ "% chance to stun " ⧺ s
  where 
    n   = outputNumber amount
    p:s = possessiveAndSubject target
    to  = if s ≡ "" then "" else " to " ⧺ s
 
data InstantEffect = GaugeDown
                   | Cooldowns
                   | Damage
                   | DamageThruDef
                   | DemeritCharge
                   | DemeritGauge
                   | DemeritHealth
                   | DemeritLose
                   | DemeritStun
                   | Drain
                   | GainStars
                   | GaugeUp
                   | Heal
                   | InstantKill
                   | RemoveBuffs
                   | RemoveDebuffs
instance showInstantEffect ∷ Show InstantEffect where
    show = showInstant Someone (-1.0)

showInstant ∷ Target → Number → InstantEffect → String
showInstant target amount instant = case instant of
    Cooldowns     → "Reduce " ⧺ p ⧺ " cooldowns by " ⧺ n
    GaugeDown     → "Reduce " ⧺ p ⧺ " NP gauge by " ⧺ n
    Damage        → "Deals " ⧺ n ⧺ "% damage" ⧺ to
    DamageThruDef → "Deals " ⧺ n ⧺ "% damage" ⧺ to ⧺ ", ignores defense"
    DemeritCharge → "Increase " ⧺ s ⧺ " NP gauge by " ⧺ n ⧺ " [Demerit]"
    DemeritGauge  → "Decrease " ⧺ p ⧺ " NP gauge by " ⧺ n ⧺ "% [Demerit]"
    DemeritHealth → "Deals " ⧺ n ⧺ " damage" ⧺ to ⧺ " [Demerit]"
    DemeritLose   → "Deals " ⧺ n ⧺ " damage" ⧺ to ⧺ " down to a minimum of 1 [Demerit]"
    DemeritStun   → "Stun " ⧺ s ⧺ " for 1 turn after " ⧺ n ⧺ " turn [Demerit]" 
    Drain         → n ⧺ "% chance to reduce " ⧺ p ⧺ " NP gauge by 1"
    GaugeUp       → "Increase " ⧺ p ⧺ " NP gauge by " ⧺ n ⧺ "%"
    Heal          → "Restores " ⧺ n ⧺ " health" ⧺ to
    RemoveBuffs   → "Removes " ⧺ p ⧺ " buffs"
    RemoveDebuffs → "Removes " ⧺ p ⧺ " debuffs"
    InstantKill   → n ⧺ "% chance to instant kill " ⧺ s
    GainStars     → "Gains " ⧺ n ⧺ " critical stars" ⧺ case target of
        Self → " for yourself"
        _    → ""
  where
    n   = outputNumber amount
    p:s = possessiveAndSubject target
    to  = if s ≡ "" then "" else " to " ⧺ s

data ActiveEffect = Grant Target Int BuffEffect Number
                  | Debuff Target Int DebuffEffect Number
                  | To Target InstantEffect Number
                  | Chance Int Target Int BuffEffect Number

getTarget ∷ ActiveEffect → Target
getTarget = case _ of
    Grant    t _ _ _ → t
    Debuff   t _ _ _ → t
    To       t _ _   → t
    Chance _ t _ _ _ → t

class (BoundedEnum a) <= ToActive a where
    toActive ∷ a → ActiveEffect
instance _a_ ∷ ToActive BuffEffect where toActive x = Grant Someone 0 x (-1.0)
instance _b_ ∷ ToActive DebuffEffect where toActive x = Debuff Someone 0 x (-1.0)
instance _c_ ∷ ToActive InstantEffect where toActive x = To Someone x (-1.0)

instance eqActiveEffect ∷ Eq ActiveEffect where
  eq activeA activeB = case activeA, activeB of
      Grant ta _ a _,    Grant tb _ b _    → a ≡ b ∧ eq' ta tb
      Debuff ta _ a _,   Debuff tb _ b _   → a ≡ b ∧ eq' ta tb
      To ta a _,         To tb b _         → a ≡ b ∧ eq' ta tb
      Chance _ ta _ a _, Chance _ tb _ b _ → a ≡ b ∧ eq' ta tb
      _,                 _                 → false
    where 
      eq' ta tb = ta ≡ tb ∨ ta ≡ Someone ∨ tb ≡ Someone

instance showActiveEffect ∷ Show ActiveEffect where
  show = case _ of
      Grant t dur buff amt      → showBuff t amt buff ⧺ turns dur
      Debuff t dur debuff amt   → showDebuff t amt debuff ⧺ turns dur
      To t instant amt          → showInstant t amt instant
      Chance per t dur buff amt → show per ⧺ "% Chance: " 
                                ⧺ showBuff t amt buff ⧺ turns dur
    where
      turns 0   = ""
      turns dur = " for " ⧺ show dur ⧺ " turns"

type Active = { name   ∷ String 
              , icon   ∷ Icon
              , cd     ∷ Int
              , effect ∷ Array ActiveEffect
              }
showActive ∷ Active → String
showActive {effect} = joinWith "\n" $ (_ ⧺ ".") ∘ show ↤ effect

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
