-- | There are three types of skill effects in the game:
-- | buffs, or positive status effects with a duration;
-- | debuffs, or negative status effects with a duration;
-- | and instant actions like gaining stars or reducing an enemy's NP gauge.
-- |
-- | In CHALDEAS, Buffs are represented by `BuffEffect`s,
-- | debuffs by `DebuffEffect`s, and instant actions by `InstantEffect`s.
-- | This system is also used by passive skills, Noble Phantasm effects,
-- | and Craft Essences.
-- |
-- | For Craft Essences only, there are also `BonusEffect`s that increase gains
-- | of Quartz Points, Bond, and so on.
module Database.Skill
  ( Amount(..), (~), toMin, toMax
  , Rank(..)
  , Target(..), allied
  , BuffEffect(..), BuffCategory(..), buffCategory
  , DebuffEffect(..)
  , InstantEffect(..), isDamage
  , BonusEffect(..)
  , SkillEffect(..), demerit, simplify, ranges
  , Skill
  , RangeInfo(..)
  , apAmount, mapAmount
  ) where

import StandardLibrary
import Data.String.CodeUnits as CodeUnits
import Data.Number.Format    as Format
import Generic               as G
import Data.Int              as Int
import Data.String           as String

import Database.Base
import Printing

data BonusEffect
    = Bond
    | EXP
    | FriendPoints
    | MysticCode
    | QPDrop
    | QPQuest

data InstantEffect
    = Avenge
    | BecomeHyde -- is there a better way to do this?
    | Cooldowns
    | Cure
    | Damage
    | DamageThruDef
    | DamageVs Trait
    | DamagePoison
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
    | Kill
    | LastStand
    | OverChance
    | RemoveBuffs
    | RemoveDebuffs
    | RemoveMental

data DebuffEffect
    = ApplyTrait Trait
    | AttackDown
    | BuffBlock
    | BuffFail
    | Burn
    | Charm
    | CharmVuln
    | CritChance
    | CritDown
    | Confusion
    | Curse
    | DamageVuln
    | DeathDown
    | DebuffVuln
    | DefenseDown
    | Fear
    | HealthLoss
    | MentalVuln
    | NPDown
    | Poison
    | SealNP
    | SealSkills
    | StarDown
    | Stun
    | StunBomb

data BuffEffect
    = AlignAffinity Alignment
    | AttackUp
    | AttackVs Trait
    | Performance Card
    | BuffUp
    | CritUp
    | ClassAffinity Class
    | DamageDown
    | DamageUp
    | DebuffResist
    | DebuffSuccess
    | DefenseUp
    | DefenseVs Trait
    | Evasion
    | GaugePerTurn
    | Guts
    | GutsPercent
    | HealPerTurn
    | HealingReceived
    | HealUp
    | IgnoreInvinc
    | Invincibility
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
    | Resist DebuffEffect
    | StarAbsorb
    | StarAffinity Class
    | StarUp
    | StarsPerTurn
    | Success DebuffEffect
    | SureHit
    | Taunt

instance _c_ :: Show BonusEffect where
    show = showBonus Placeholder false

showBonus :: Amount -> Boolean -> BonusEffect -> String
showBonus amount isPerc = case _ of
    Bond
      -> gain "Bond Points"
    EXP
      -> gain "Master EXP"
    FriendPoints
      -> "Friend Points obtained from support becomes +" <> n
    MysticCode
      -> gain "Mystic Code EXP"
    QPDrop
      -> "Increase QP from completing quests by " <> n
    QPQuest
      -> "Increase QP from enemy drops by " <> n
  where
    n 
      | isPerc    = show amount <> "%"
      | otherwise = show amount
    gain x = "Increase " <> x <> " gained by " <> n

instance _d_ :: Show InstantEffect where
    show = showInstant Someone Placeholder

showInstant :: Target -> Amount -> InstantEffect -> String
showInstant target amount = case _ of
    Avenge
      -> "At the end of the next turn, deal " <> n
      <> "% of damage taken during that turn" <> to
    BecomeHyde
      -> "Transform into Hyde. Class: [Berserker]. \
         \Star Weight: 9. Star Rate: 5%. NP/Hit: 1.02%. NP/Defend: 5%. \
         \Alignment: Chaotic Evil. Lose [" <> show Brynhild <> "] trait. \
         \Skills are more effective"
    Cooldowns
      -> "Reduce" <> p <> " cooldowns by " <> n
    Cure
      -> "Remove" <> p <> " poison debuffs"
    Damage
      -> "Deal " <> n <> "% damage" <> to
    DamageThruDef
      -> "Deal " <> n <> "% damage" <> to <> ", ignoring defense"
    DamageVs t
      -> "Deal " <> n <> "% extra damage to [" <> show t <> "]"
    DamagePoison
      -> "Deal " <> n <> "% extra damage to [Poisoned]"
    DemeritBuffs
      -> "Remove" <> p <> " buffs"
    DemeritCharge
      -> "Increase" <> s <> " NP gauge by " <> n
    DemeritGauge
      -> "Decrease" <> p <> " NP gauge by " <> n <> "%"
    DemeritDamage
      -> "Deal " <> n <> " damage" <> to
    DemeritKill
      -> "Sacrifice" <> s <> " (can trigger Guts)"
    DemeritHealth
      -> "Deal " <> n <> " damage" <> to <> " down to a minimum of 1"
    GaugeDown
      -> "Reduce" <> p <> " NP gauge by " <> n
    GaugeUp
      -> "Increase" <> p <> " NP gauge by " <> n <> "%"
    Heal
      -> "Restore " <> (if full then "all" else n) <> " HP" <> to
    LastStand
      -> "Deal up to " <> n <> "% damage based on missing health" <> to
    OverChance
      -> "Gain " <> n <> "% chance to apply Overcharge buffs"
    RemoveBuffs
      -> "Remove" <> p <> " buffs"
    RemoveDebuffs
      -> "Remove" <> p <> " debuffs"
    RemoveMental
      -> "Remove" <> p <> " mental debuffs"
    Kill
      -> (not full ? append $ n <> "% chance to ") $ "Instant-Kill " <> s
    GainStars
      -> "Gain " <> n <> " critical stars"
         <> case target of
                Self -> " for yourself"
                _    -> ""
  where
    n      = show amount
    {p, s} = possessiveAndSubject target
    to     = case s of
                 "" -> ""
                 _  -> " to" <> s
    full   = amount == Full

instance _e_ :: Show DebuffEffect where
    show = showDebuff Someone Placeholder

showDebuff :: Target -> Amount -> DebuffEffect -> String
showDebuff target amount = case _ of
    ApplyTrait t -> "Apply [" <> show t <> "]" <> to
    AttackDown   -> reduce "attack"
    BuffBlock    -> "Inflict Buff Block status" <> to
    BuffFail     -> reduce "attack buff success rate"
    Burn         -> damage "Burn"
    Charm        -> "Charm" <> s
    CharmVuln    -> unresist "Charm"
    Confusion    -> eachTurn "Confusion" "Seal skills"
    CritChance   -> reduce "critical attack chance"
    CritDown     -> reduce "critical damage"
    Curse        -> damage "Curse"
    DamageVuln   -> "Increase" <> s <> " damage taken by " <> n
    DeathDown    -> unresist "Instant-Death"
    DebuffVuln   -> unresist "debuff"
    DefenseDown  -> reduce "defense"
    Fear         -> eachTurn "Fear" "be Stunned"
    HealthLoss   -> "Decrease" <> p <> " HP by " <> n <> " per turn"
    MentalVuln   -> unresist "mental debuff"
    NPDown       -> reduce "NP Damage"
    Poison       -> damage "Poison"
    SealNP       -> "Seal" <> p <> " NP"
    SealSkills   -> "Seal" <> p <> " skills"
    StarDown     -> reduce "C. Star drop rate"
    Stun         -> "Stun" <> s
    StunBomb     -> "Stun" <> s <> " after 1 turn"
  where
    n          = show amount
    {p, s}     = possessiveAndSubject target
    to         = case s of
                     "" -> ""
                     _  -> " to" <> s
    reduce   x = "Reduce" <> p <> " " <> x <> " by " <> n <> "%"
    unresist x = reduce $ x <> " resistance"
    damage   x = "Inflict " <> n <> " " <> x <> " damage" <> to <> " every turn"
    eachTurn x perTurn = "Inflict " <> x <> " status" <> to <> ", causing "
                         <> n <> "% chance to " <> perTurn <> " every turn"

instance _f_ :: Show BuffEffect where
    show = showBuff Someone Placeholder

showBuff :: Target -> Amount -> BuffEffect -> String
showBuff target amount = case _ of
    AttackUp        -> increase "attack"
    AttackVs t      -> increase $ "attack" <> against t
    AlignAffinity a -> increase $ "attack" <> against a
    Performance c   -> increase $ show c <> " performance"
    BuffUp          -> success "buff"
    CritUp          -> increase "critical damage"
    ClassAffinity c -> increase $ "damage" <> against c
    DamageDown      -> "Reduce" <> p <> " damage taken by " <> n
    DamageUp        -> "Increase" <> p <> " damage by " <> n
    DebuffResist    -> resist "debuff"
    DebuffSuccess   -> success "debuff"
    DefenseUp       -> increase "defense"
    DefenseVs t   -> increase $ "defense" <> against t
    Evasion         -> grant "Evasion"
    GaugePerTurn    -> "Charge" <> p <> " NP gauge" <> by <> " every turn"
    Guts            -> "Grant" <> s <> " Guts with " <> n <> " HP"
    GutsPercent     -> "Grant" <> s <> " Guts with " <> n <> "% HP"
    HealingReceived -> increase "healing received"
    HealPerTurn     -> "Restore " <> n <> " HP" <> to
    HealUp          -> increase "healing power"
    IgnoreInvinc    -> grant "Ignore Invincibility"
    Invincibility   -> grant "Invincibility"
    KillResist      -> resist "Instant-Kill"
    KillUp          -> success "Instant-Kill"
    MaxHP           -> "Increase" <> p <> " Max HP by " <> n
    MentalResist    -> resist "mental debuff"
    MentalSuccess   -> success "mental debuff"
    NPUp            -> increase "NP Damage"
    NPFromDamage    -> increase "NP generation rate when taking damage"
    NPGen           -> increase "NP generation rate"
    OffensiveResist -> resist "offensive debuff"
    Overcharge      -> "Overcharge" <> p <> " NP by " <> n <> " stages"
    Resist debuff   -> resist $ G.genericShow debuff <> " debuff"
    StarAbsorb      -> increase "C. Star absorption"
    StarAffinity c  -> increase $ "C. Star drop" <> against c
    StarUp          -> increase "C. Star drop rate"
    Success debuff  -> success $ G.genericShow debuff
    SureHit         -> grant "Sure Hit"
    Taunt           -> "Draw attention of all enemies" <> to
    StarsPerTurn    -> "Gain " <> n <> " stars every turn" <> case target of
        Self -> " for yourself"
        _    -> ""
  where
    n       = show amount
    {p, s}  = possessiveAndSubject target
    to      = case s of
                  "" -> ""
                  _  -> " to" <> s
    by      = " by " <> n <> "%"
    grant    x = "Grant" <> s <> " " <> x
    increase x = "Increase" <> p <> " " <> x <> by
    success  x = increase $ x <> " success rate"
    resist   x = case amount of
                     Full -> "Grant" <> s <> " " <> x <> " immunity"
                     _    -> "Increase" <> p <> " " <> x <> " resistance" <> by
    against :: ∀ a. Show a => a -> String
    against x = " against [" <> show x <> "]"

data BuffCategory
    = BuffOffensive
    | BuffDefensive
    | BuffSupport
    | BuffUtility
    | BuffSpecialist

buffCategory :: BuffEffect -> BuffCategory
buffCategory AttackUp          = BuffOffensive
buffCategory (AttackVs _)      = BuffSpecialist
buffCategory (AlignAffinity _) = BuffSpecialist
buffCategory (Performance _)   = BuffOffensive
buffCategory BuffUp            = BuffUtility
buffCategory CritUp            = BuffOffensive
buffCategory (ClassAffinity _) = BuffSpecialist
buffCategory DamageDown         = BuffDefensive
buffCategory DamageUp          = BuffOffensive
buffCategory DebuffResist      = BuffUtility
buffCategory DebuffSuccess     = BuffUtility
buffCategory DefenseUp         = BuffDefensive
buffCategory (DefenseVs _)     = BuffSpecialist
buffCategory Evasion           = BuffDefensive
buffCategory GaugePerTurn      = BuffSupport
buffCategory Guts              = BuffDefensive
buffCategory GutsPercent       = BuffDefensive
buffCategory HealingReceived   = BuffSupport
buffCategory HealPerTurn       = BuffDefensive
buffCategory HealUp            = BuffSupport
buffCategory IgnoreInvinc      = BuffOffensive
buffCategory Invincibility     = BuffDefensive
buffCategory KillResist        = BuffUtility
buffCategory KillUp            = BuffUtility
buffCategory MaxHP             = BuffDefensive
buffCategory MentalResist      = BuffUtility
buffCategory MentalSuccess     = BuffUtility
buffCategory NPUp              = BuffOffensive
buffCategory NPFromDamage      = BuffSupport
buffCategory NPGen             = BuffSupport
buffCategory OffensiveResist   = BuffUtility
buffCategory Overcharge        = BuffSupport
buffCategory (Resist _)        = BuffUtility
buffCategory StarAbsorb        = BuffSupport
buffCategory (StarAffinity _)  = BuffSpecialist
buffCategory StarUp            = BuffSupport
buffCategory (Success _)       = BuffUtility
buffCategory SureHit           = BuffOffensive
buffCategory Taunt             = BuffDefensive
buffCategory StarsPerTurn      = BuffSupport

isDamage :: InstantEffect -> Boolean
isDamage Avenge = true
isDamage Damage = true
isDamage DamageThruDef = true
isDamage (DamageVs _) = true
isDamage DamagePoison = true
isDamage LastStand = true
isDamage _ = false

-- | Int field is duration, Number field is amount
data SkillEffect
    = Grant Target Int BuffEffect Amount
    | Debuff Target Int DebuffEffect Amount
    | To Target InstantEffect Amount
    | Bonus BonusEffect Boolean Amount
    | Chance Int SkillEffect
    | Chances Int Int SkillEffect
    | When String SkillEffect
    | Times Int SkillEffect
    | ToMax Amount SkillEffect

apAmount :: (Number -> Number -> Number) -> Amount -> Number
apAmount f (Range x y) = f x y
apAmount _ (Flat x) = x
apAmount _ Placeholder = 0.0
apAmount _ Full = 0.0

mapAmount :: (Number -> Number -> Amount) -> SkillEffect -> SkillEffect
mapAmount f eff = go eff
  where
    f' (Range a b) = f a b
    f' x = x
    go (Grant a b c d)  = Grant a b c $ f' d
    go (Debuff a b c d) = Debuff a b c $ f' d
    go (To a b c)       = To a b $ f' c
    go (Bonus a b c)    = Bonus a b $ f' c
    go (Chance a b)     = Chance a $ go b
    go (When a b)       = When a $ go b
    go (Times a b)      = Times a $ go b
    go (ToMax a b)      = ToMax (f' a) $ go b
    go (Chances a b c)  = case f (Int.toNumber a) (Int.toNumber b) of
                              Flat x      -> Chance (Int.floor x) $ go c
                              Range x y   -> Chances (Int.floor x)
                                                     (Int.floor y) $ go c
                              Placeholder -> go c
                              Full        -> go c

simplify :: SkillEffect -> SkillEffect
simplify (Chance _ ef)    = simplify ef
simplify (Chances _ _ ef) = simplify ef
simplify (When _ ef)      = simplify ef
simplify (Times _ ef)     = simplify ef
simplify (ToMax _ ef)   = simplify ef
simplify ef               = ef

demerit :: SkillEffect -> Boolean
demerit (Grant t _ _ _) = not $ allied t
demerit (Debuff t _ _ _) = allied t
demerit (To _ DemeritBuffs _) = true
demerit (To _ DemeritCharge _) = true
demerit (To _ DemeritDamage _) = true
demerit (To _ DemeritGauge _) = true
demerit (To _ DemeritHealth _) = true
demerit (To _ DemeritKill _) = true
demerit (To _ _ _) = false
demerit (Bonus _ _ _) = false
demerit (Chance _ ef) = demerit ef
demerit (Chances _ _ ef) = demerit ef
demerit (When _ ef) = demerit ef
demerit (Times _ ef) = demerit ef
demerit (ToMax _ ef) = demerit ef

instance _g_ :: Show SkillEffect where
    show = flip append "." <<< go
      where
        go = case _ of
            Grant t dur buff amt -> showBuff t amt buff <> turns dur
            Debuff t dur deb amt -> showDebuff t amt deb <> turns dur
            To t instant amt     -> showInstant t amt instant
            Bonus bonus perc amt -> showBonus amt perc bonus
            Chance 0 ef          -> "Chance to " <> uncap (go ef)
            Chance per ef        -> show per <> "% chance to " <> uncap (go ef)
            Chances x y ef       -> show x <> "~" <> show y <> "% chance to "
                                    <> uncap (go ef)
            When "attacking" ef  -> go ef <> " when attacking"
            When cond ef         -> "If " <> cond <> ": " <> uncap (go ef)
            Times 1 ef           -> go ef <> " (1 time)"
            Times times ef       -> go ef <> " (" <> show times <> " times)"
            ToMax amount ef      -> go ef <> " every turn (max " <> show amount
                                    <> ")"
        turns 0   = ""
        turns 1   = " for 1 turn"
        turns dur = " for " <> show dur <> " turns"

uncap :: String -> String
uncap s = case String.uncons s of
    Nothing           -> s
    Just {head, tail} -> String.toLower (String.singleton head) <> tail

data Amount
    = Placeholder
    | Full
    | Flat Number
    | Range Number Number

infix 1 Range as ~

instance _h_ :: Show Amount where
    show Placeholder = "X"
    show Full = ""
    show (Flat x) = Format.toString x
    show (Range x y) = Format.toString x <> "~" <> Format.toString y

toMin :: Amount -> Number
toMin Placeholder = 0.0
toMin Full = 0.0
toMin (Flat x) = x
toMin (Range x _) = x

toMax :: Amount -> Number
toMax Placeholder = 0.0
toMax Full = 0.0
toMax (Flat x) = x
toMax (Range _ b) = b

data Rank
    = Unknown | EX
    | APlusPlus | APlus | A | AMinus
    | BPlusPlus | BPlus | B | BMinus
    | CPlusPlus | CPlus | C | CMinus
                | DPlus | D
                | EPlus | E | EMinus
instance _b_ :: Show Rank where
    show Unknown   = ""
    show EX        = " EX"
    show APlusPlus = " A++"
    show APlus     = " A+"
    show A         = " A"
    show AMinus    = " A-"
    show BPlusPlus = " B++"
    show BPlus     = " B+"
    show B         = " B"
    show BMinus    = " B-"
    show CPlusPlus = " C++"
    show CPlus     = " C+"
    show C         = " C"
    show CMinus    = " C-"
    show DPlus     = " D+"
    show D         = " D"
    show EPlus     = " E+"
    show E         = " E"
    show EMinus    = " E-"

data Target = Someone
            | Self | Ally | Party | Enemy | Enemies | Others
            | AlliesType Trait | EnemyType Trait | EnemiesType Trait
            | Killer | Target
derive instance _a_ :: Eq Target

allied :: Target -> Boolean
allied Self = true
allied Ally = true
allied Party = true
allied Others = true
allied (AlliesType _) = true
allied _ = false

possessiveAndSubject :: Target -> { p :: String, s :: String }
possessiveAndSubject = case _ of
    Someone       -> { p: ""
                     , s: ""
                     }
    Self          -> { p: " own"
                     , s: " self"
                     }
    Ally          -> { p: " one ally's"
                     , s: " one ally"
                     }
    Party         -> { p: " party's"
                     , s: " party"
                     }
    Enemy         -> { p: " one enemy's"
                     , s: " one enemy"
                     }
    Enemies       -> { p: " all enemy"
                     , s: " all enemies"
                     }
    Others        -> { p: " allies' (excluding self)"
                     , s: " allies (excluding self)"
                     }
    AlliesType t  -> { p: " " <> show t <> " allies'"
                     , s: " " <> show t <> " allies"
                     }
    EnemyType t   -> { p: " one " <> show t <> " enemy's"
                     , s: " one " <> show t <> " enemy"
                     }
    EnemiesType t -> { p: " all " <> show t <> " enemy"
                     , s: " all " <> show t <> " enemies"
                     }
    Killer        -> { p: " killer's"
                     , s: " killer"
                     }
    Target        -> { p: " target's"
                     , s: " target"
                     }

data RangeInfo = RangeInfo Boolean Number Number
instance _i_ :: Show RangeInfo where
    show (RangeInfo true  from to) = places 2 from <> "% ~ " <> places 2 to <> "%"
    show (RangeInfo false from to) = places 2 from <>  " ~ " <> places 2 to
instance _j_ :: Eq RangeInfo where
    eq (RangeInfo _ a1 a2) (RangeInfo _ b1 b2) = a1 == b1 && a2 == b2

ranges :: ∀ f. Alternative f => Bind f => f SkillEffect -> f RangeInfo
ranges = bindFlipped toInfo
  where
    toInfo ef              = info (isPercent ef) <$> acc ef
    isPercent              = elem '%' <<< CodeUnits.toCharArray <<< show
    info isPerc {from, to} = RangeInfo isPerc from to
    acc (Grant _ _ _ x)    = go x
    acc (Debuff _ _ _ x)   = go x
    acc (To _ _ x)         = go x
    acc (Bonus _ _ x)      = go x
    acc (Chance _ ef)      = acc ef
    acc (Chances x y ef)   = pure {from: Int.toNumber x, to: Int.toNumber y}
                             <|> acc ef
    acc (When _ ef)        = acc ef
    acc (Times _ ef)       = acc ef
    acc (ToMax _ ef)       = acc ef
    go (x ~ y)             = pure {from: x, to: y}
    go _                   = empty

type Skill = { name   :: String
             , rank   :: Rank
             , icon   :: Icon
             , cd     :: Int
             , effect :: Array SkillEffect
             }

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ :: G.Generic BuffEffect _
derive instance _1_ :: Eq BuffEffect
derive instance _2_ :: Ord BuffEffect
instance _3_ :: G.Enum BuffEffect where
    succ = G.genericSucc
    pred = G.genericPred
instance _4_ :: G.Bounded BuffEffect where
    top = G.genericTop
    bottom = G.genericBottom
instance _5_ :: G.BoundedEnum BuffEffect where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _8_ :: G.Generic DebuffEffect _
derive instance _9_ :: Eq DebuffEffect
derive instance _10_ :: Ord DebuffEffect
instance _11_ :: G.Enum DebuffEffect where
    succ = G.genericSucc
    pred = G.genericPred
instance _12_ :: G.Bounded DebuffEffect where
    top = G.genericTop
    bottom = G.genericBottom
instance _13_ :: G.BoundedEnum DebuffEffect where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _14_ :: G.Generic InstantEffect _
derive instance _15_ :: Eq InstantEffect
derive instance _16_ :: Ord InstantEffect
instance _17_ :: G.Enum InstantEffect where
    succ = G.genericSucc
    pred = G.genericPred
instance _18_ :: G.Bounded InstantEffect where
    top = G.genericTop
    bottom = G.genericBottom
instance _19_ :: G.BoundedEnum InstantEffect where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _20_ :: G.Generic BonusEffect _
derive instance _21_ :: Eq BonusEffect
derive instance _22_ :: Ord BonusEffect
instance _23_ :: G.Enum BonusEffect where
    succ = G.genericSucc
    pred = G.genericPred
instance _24_ :: G.Bounded BonusEffect where
    top = G.genericTop
    bottom = G.genericBottom
instance _25_ :: G.BoundedEnum BonusEffect where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _26_ :: G.Generic Target _
instance _27_ :: Show Target where
    show = G.genericShow

derive instance _30_ :: Eq Amount

derive instance _31_ :: G.Generic BuffCategory _
derive instance _32_ :: Eq BuffCategory
derive instance _33_ :: Ord BuffCategory
instance _34_ :: G.Enum BuffCategory where
    succ = G.genericSucc
    pred = G.genericPred
instance _35_ :: G.Bounded BuffCategory where
    top = G.genericTop
    bottom = G.genericBottom
instance _36_ :: G.BoundedEnum BuffCategory where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum
instance _37_ :: Show BuffCategory where
    show = String.drop 4 <<< G.genericShow

derive instance _38_ :: Eq SkillEffect

derive instance _39_ :: Eq Rank
derive instance _40_ :: Ord Rank
derive instance _41_ :: G.Generic Rank _
instance _42_ :: G.Enum Rank where
    succ = G.genericSucc
    pred = G.genericPred
instance _43_ :: G.Bounded Rank where
    top = G.genericTop
    bottom = G.genericBottom
instance _44_ :: G.BoundedEnum Rank where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _46_ :: Ord RangeInfo
derive instance _47_ :: G.Generic RangeInfo _

derive instance _100_ :: Ord SkillEffect
derive instance _101_ :: Ord Target
derive instance _102_ :: Ord Amount
