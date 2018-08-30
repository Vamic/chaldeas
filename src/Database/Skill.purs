module Database.Skill where

import Prelude
import Operators

import Data.Enum          
import Data.Number.Format      
import Data.Generic.Rep    
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum 
import Data.Generic.Rep.Show 
import Data.Maybe
import Data.String            
import Data.Tuple             

import Database.Icon
import Database.Trait

data Amount 
    = Placeholder
    | Full
    | Flat Number
    | Range Number Number

infix 1 Range as ~

instance _a_ ∷ Show Amount where
  show Placeholder = "X"
  show Full = ""
  show (Flat x) = toString x
  show (Range a b) = toString a ++ "~" ++ toString b

toMin ∷ Amount -> Number
toMin Placeholder = 0.0
toMin Full = 0.0
toMin (Flat x) = x
toMin (Range a _) = a

toMax ∷ Amount -> Number
toMax Placeholder = 0.0
toMax Full = 0.0
toMax (Flat x) = x
toMax (Range _ b) = b

data Rank = Unknown | EX   
          | APlusPlus | APlus | A | AMinus
          | BPlusPlus | BPlus | B | BMinus 
          | CPlusPlus | CPlus | C | CMinus
                      | DPlus | D 
                      | EPlus | E | EMinus
instance _b_ ∷ Show Rank where
  show = case _ of
    Unknown   -> "--"
    EX        -> "EX"
    APlusPlus -> "A++"
    APlus     -> "A+"
    A         -> "A"
    AMinus    -> "A-"
    BPlusPlus -> "B++"
    BPlus     -> "B+"
    B         -> "B"
    BMinus    -> "B-"
    CPlusPlus -> "C++"
    CPlus     -> "C+"
    C         -> "C"
    CMinus    -> "C-"
    DPlus     -> "D+"
    D         -> "D"
    EPlus     -> "E+"
    E         -> "E"
    EMinus    -> "E-"

data Target = Someone
            | Self | Ally | Allies | Party | Enemy | Enemies | Others
            | AlliesType Trait | EnemyType Trait | EnemiesType Trait
derive instance _00_ ∷ Eq Target

allied ∷ Target -> Boolean
allied Self = true
allied Ally = true
allied Allies = true
allied Party = true
allied Others = true
allied (AlliesType _) = true
allied _ = false

possessiveAndSubject ∷ Target -> Tuple String String
possessiveAndSubject = case _ of
    Someone       -> ""            
                  : ""
    Self          -> " own"         
                  : " self"
    Ally          -> " one ally's"  
                  : " one ally"
    Allies        -> " all allies'" 
                  : " allies"
    Party         -> " party's"       
                  : " party"
    Enemy         -> " one enemy's" 
                  : " one enemy"
    Enemies       -> " all enemy"   
                  : " all enemies"
    Others        -> " allies' (excluding self)" 
                  : " allies (excluding self)"
    AlliesType t  -> " " ++ show t ++ " allies'" 
                  : " " ++ show t ++ " allies"
    EnemyType t   -> " one " ++ show t ++ " enemy's" 
                  : " one " ++ show t ++ " enemy"
    EnemiesType t -> " all " ++ show t ++ " enemy" 
                  : " all " ++ show t ++ " enemies"
    
times ∷ Amount -> String
times Placeholder = ""
times Full = ""
times (Flat 1.0)  = " (1 time)"
times amt = " (" ++ show amt ++ " times)"

data BuffEffect = AlignAffinity Alignment
                | ArtsUp
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
                | GutsTriple
                | GutsUnlimited
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
                | OffensiveResist
                | Overcharge
                | PoisonResist
                | QuickUp
                | ReduceDamage
                | StarAbsorb
                | StarAffinity Class
                | StarUp
                | StarsPerTurn
                | StunSuccess
                | SureHit
                | Taunt
instance _c_ ∷ Show BuffEffect where
    show = showBuff Someone Placeholder

showBuff ∷ Target -> Amount -> BuffEffect -> String
showBuff target amount buff = case buff of
    ArtsUp           -> "Increase" ++ p ++ " Arts performance" ++ by
    AttackUp         -> "Increase" ++ p ++ " attack" ++ by
    AttackUpVs    t  -> "Increase" ++ p ++ " attack against " ++ show t 
                    ++ " enemies" ++ by
    AlignAffinity a  -> "Increase" ++ p ++ " attack against " ++ show a
                    ++ " enemies" ++ by
    BuffUp           -> "Increase" ++ p ++ " buff success rate" ++ by
    BurnImmunity     -> "Grant" ++ s ++ " Burn Immunity" ++ times amount
    BusterUp         -> "Increase" ++ p ++ " Buster performance" ++ by
    CharmResist      -> "Increase" ++ p ++ " Charm resistance" ++ by
    CritUp           -> "Increase" ++ p ++ " critical damage" ++ by
    DamageAffinity c -> "Increase" ++ p ++ " damage against " ++ show c 
                    ++ " enemies" ++ by
    DamageCut        -> "Reduce" ++ p ++ " damage taken by " ++ n 
                    ++ " for 1 attack"
    DamageUp         -> "Increase" ++ p ++ " damage by " ++ n
    DebuffImmunity   -> "Grant" ++ s ++ " Debuff Immunity" ++ times amount
    DebuffResist     -> "Increase" ++ p ++ " debuff resistance" ++ by
    DebuffSuccess    -> "Increase" ++ p ++ " debuff success rate" ++ by
    DefenseUp        -> "Increase" ++ p ++ " defense" ++ by
    DefenseUpVs   t  -> "Increase" ++ p ++ " defense against " ++ show t 
                    ++ " enemies" ++ by
    Evasion          -> "Grant" ++ s ++ " Evasion" ++ times amount
    GaugePerTurn     -> "Charge" ++ p ++ " NP gauge" ++ by ++ " every turn"
    Guts             -> "Grant" ++ s ++ " Guts (1 time) with " ++ n ++ " HP"
    GutsTriple       -> "Grant" ++ s ++ " Guts (3 times) with " ++ n ++ " HP"
    GutsUnlimited    -> "Grant" ++ s ++ " Guts with " ++ n ++ " HP"
    HealingReceived  -> "Increase" ++ p ++ " healing received" ++ by
    HealPerTurn      -> "Restore " ++ n ++ " health" ++ to ++ " every turn"
    HealUp           -> "Increase" ++ p ++ " healing power" ++ by
    IgnoreInvinc     -> "Grant" ++ s ++ " Ignore Invincibility" ++ times amount
    Invincibility    -> "Grant" ++ s ++ " Invincibility" ++ times amount
    KillChance       -> "Grant" ++ s ++ " " ++ n
                    ++ "% chance to Instant-Kill enemy with normal attacks"
    KillImmunity     -> "Grant" ++ s ++ " Instant-Kill Immunity" ++ times amount
    KillResist       -> "Increase" ++ p ++ " Instant-Kill resistance" ++ by
    KillUp           -> "Increase" ++ p ++ " Instant-Kill success rate" ++ by
    MaxHP            -> "Increase" ++ p ++ " Max HP by " ++ n
    MentalResist     -> "Increase" ++ p ++ " mental debuff resistance" ++ by
    MentalSuccess    -> "Increase" ++ p ++ " mental debuff success rate" ++ by
    NPUp             -> "Increase" ++ p ++ " NP Damage" ++ by
    NPFromDamage     -> "Increase" ++ p 
                    ++ " NP generation rate when taking damage" ++ by
    NPGen            -> "Increase" ++ p ++ " NP generation rate" ++ by
    OffensiveResist  -> "Increase" ++ p ++ " offensive debuff resistance" ++ by
    Overcharge       -> "Overcharge" ++ p ++ " NP by " ++ n ++ " stages"
    PoisonResist     -> "Increase" ++ p ++ " poison debuff resistance" ++ by
    QuickUp          -> "Increase" ++ p ++ " Quick performance" ++ by
    ReduceDamage     -> "Reduce" ++ p ++ " damage taken by " ++ n
    StarAbsorb       -> "Increase" ++ p ++ " critical star absorption" ++ by
    StarAffinity   c -> "Increase" ++ p ++ " critical star generation against " 
                    ++ show c ++ " enemies" ++ by
    StarUp           -> "Increase" ++ p ++ " critical star generation rate" 
                    ++ by
    StarsPerTurn     -> "Gain " ++ n ++ " stars every turn"
    StunSuccess      -> "Increase" ++ p ++ " Stun success rate" ++ by 
                    ++ " (1 time)"
    SureHit          -> "Grant" ++ s ++ " Sure Hit" ++ times amount
    Taunt            -> "Draw attention of all enemies" ++ to
  where 
    n   = show amount
    p:s = possessiveAndSubject target
    to  = if s == "" then "" else " to" ++ s
    by  = " by " ++ n ++ "%"
       
data DebuffEffect = ApplyTrait Trait
                  | AttackDown
                  | BuffBlock
                  | BuffFail
                  | Burn
                  | Charm
                  | CharmVuln
                  | CritChance
                  | CritDown
                  | Curse
                  | DamageVuln
                  | DeathDown
                  | DebuffVuln
                  | DefenseDown
                  | Disorder
                  | MentalVuln
                  | NPDown
                  | Poison
                  | SealNP
                  | SealSkills
                  | Stun
                  | StunBomb
                  | Terror
instance _d_ ∷ Show DebuffEffect where
    show = showDebuff Someone Placeholder

showDebuff ∷ Target -> Amount -> DebuffEffect -> String
showDebuff target amount debuff = case debuff of
    ApplyTrait t -> "Apply the " ++ show t ++ " trait" ++ to
    AttackDown   -> "Reduce" ++ p ++ " attack by " ++ n ++ "%"
    BuffBlock    -> "Inflict Buff Block status" ++ to ++ times amount
    BuffFail     -> "Reduce" ++ p ++ " attack buff success rate by " ++ n ++ "%"
    Burn         -> "Inflict " ++ n ++ " Burn damage" ++ to
    Charm        -> "Charm" ++ s
    CharmVuln    -> "Reduce" ++ p ++ " Charm resistance by " ++ n ++ "%"
    CritChance   -> "Reduce" ++ p ++ " critical attack chance by " ++ n ++ "%"
    CritDown     -> "Reduce" ++ p ++ " critical damage by " ++ n ++ " %"
    Curse        -> "Inflict " ++ n ++ " Curse damage" ++ to
    DamageVuln   -> "Increase" ++ s ++ " damage taken by " ++ n
    DeathDown    -> "Reduce" ++ p ++ " Instant-Death resistance by " ++ n ++ "%"
    DebuffVuln   -> "Reduce" ++ p ++ " debuff resistance by " ++ n ++ "%"
    DefenseDown  -> "Reduce" ++ p ++  " defense by " ++ n ++ "%"
    Disorder     -> "Inflict Disorder status" ++ to 
                ++ ", causing " ++ n ++ "% chance to Seal skills every turn"
    MentalVuln   -> "Reduce" ++ p ++ " mental debuff resistance by " ++ n ++ "%"
    NPDown       -> "Decrease" ++ p ++ " Noble Phantasm damage by " ++ n ++ "%"
    Poison       -> "Inflict " ++ n ++ " Poison damage" ++ to
    SealNP       -> "Seal" ++ p ++ " NP"
    SealSkills   -> "Seal" ++ p ++ " skills"
    Stun         -> "Stun" ++ s
    StunBomb     -> "Stun" ++ s ++ " after 1 turn"
    Terror       -> "Inflict Terror status" ++ to ++ ", causing " ++ n 
                 ++ "% chance to be Stunned every turn"
  where  
    n   = show amount
    p:s = possessiveAndSubject target
    to  = if s == "" then "" else " to" ++ s
 
mental ∷ DebuffEffect -> Boolean
mental Charm = true
mental Disorder = true
mental Terror = true
mental _ = false

offensive ∷ DebuffEffect -> Boolean
offensive AttackDown = true
offensive CritDown   = true
offensive NPDown     = true
-- offensive DamageDown = true
-- offensive QuickDown = true
-- offensive ArtsDown = true
-- offensive BusterDown = true
offensive _ = false

data InstantEffect = Avenge
                   | ChangeClass Class
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
                   | GaugeDown
                   | GaugeUp
                   | Heal
                   | HealToFull
                   | Kill
                   | LastStand
                   | OverChance
                   | RemoveBuffs
                   | RemoveDebuffs
                   | RemoveMental
instance _e_ ∷ Show InstantEffect where
    show = showInstant Someone Placeholder

showInstant ∷ Target -> Amount -> InstantEffect -> String
showInstant target amount instant = case instant of
    Avenge        -> "Wait 1 turn [Demerit], then deal " ++ n 
                  ++ "% of damage taken during that turn" ++ to
    ChangeClass c -> "Change" ++ p ++ " class to " ++ show c
    Cooldowns     -> "Reduce" ++ p ++ " cooldowns by " ++ n
    Cure          -> "Remove" ++ p ++ " poison debuffs"
    Damage        -> "Deal " ++ n ++ "% damage" ++ to
    DamageThruDef -> "Deal " ++ n ++ "% damage" ++ to ++ ", ignoring defense"
    DemeritBuffs  -> "Remove" ++ p ++ " buffs [Demerit]"
    DemeritCharge -> "Increase" ++ s ++ " NP gauge by " ++ n ++ " [Demerit]"
    DemeritGauge  -> "Decrease" ++ p ++ " NP gauge by " ++ n ++ "% [Demerit]"
    DemeritDamage -> "Deal " ++ n ++ " damage" ++ to ++ " [Demerit]"
    DemeritKill   -> "Sacrifice" ++ s ++ " (can trigger Guts) [Demerit]"
    DemeritHealth -> "Deal " ++ n ++ " damage" ++ to 
                 ++ " down to a minimum of 1 [Demerit]"
    GaugeDown     -> "Reduce" ++ p ++ " NP gauge by " ++ n
    GaugeUp       -> "Increase" ++ p ++ " NP gauge by " ++ n ++ "%"
    Heal          -> "Restore " ++ n ++ " health" ++ to
    HealToFull    -> "Heal" ++ s ++ " to full"
    Kill          -> n ++ "% chance to Instant-Kill " ++ s
    LastStand     -> "Deal up to " ++ n ++ "% damage based on missing health" 
                  ++ to
    OverChance    -> "Gain " ++ n ++ "% chance to apply Overcharge buffs"
    RemoveBuffs   -> "Remove" ++ p ++ " buffs"
    RemoveDebuffs -> "Remove" ++ p ++ " debuffs"
    RemoveMental  -> "Remove" ++ p ++ " mental debuffs"
    GainStars     -> "Gain " ++ n ++ " critical stars" ++ case target of
        Self -> " for yourself"
        _    -> ""
  where
    n   = show amount
    p:s = possessiveAndSubject target
    to  = if s == "" then "" else " to" ++ s

-- | Int field is duration, Number field is amount
data ActiveEffect = Grant Target Int BuffEffect Amount
                  | Debuff Target Int DebuffEffect Amount
                  | To Target InstantEffect Amount
                  | Chance Int ActiveEffect
                  | Chances Int Int ActiveEffect
                  | When String ActiveEffect

instance _01_ ∷ Eq ActiveEffect where
  eq activeA activeB = case activeA, activeB of
      Grant ta _ a _,  Grant tb _ b _  -> eq a b && eq' ta tb
      Debuff ta _ a _, Debuff tb _ b _ -> eq a b && eq' ta tb
      To ta a _,       To tb b _       -> eq a b && eq' ta tb
      Chance _ a,      Chance _ b      -> eq a b 
      Chances _ _ a,   Chances _ _ b   -> eq a b
      _,               _               -> false
    where 
      eq' ta tb = ta == tb || ta == Someone || tb == Someone

instance _f_ ∷ Show ActiveEffect where
  show = case _ of
      Grant t dur buff amt    -> showBuff t amt buff ++ turns dur
                             ++ if not allied t then " [Demerit]." else "."
      Debuff t dur debuff amt -> showDebuff t amt debuff ++ turns dur
                             ++ if allied t then " [Demerit]." else "."
      To t instant amt        -> showInstant t amt instant ++ "."
      Chance per ef           -> show per ++ "% chance to " ++ uncap (show ef)
      Chances a b ef      -> show a ++ "~" ++ show b ++ "% chance to " ++
                                 uncap (show ef)
      When cond ef            -> "If " ++ cond ++ ": " ++ uncap (show ef)
    where
      turns 0   = ""
      turns 1   = " for 1 turn"
      turns dur = " for " ++ show dur ++ " turns"

uncap ∷ String -> String
uncap s = case uncons s of
    Nothing                  -> s
    Just {head: x, tail: xs} -> toLower (singleton x) ++ xs

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

derive instance _20_ ∷ Generic Target _
instance _21_ ∷ Show Target where
  show = genericShow
