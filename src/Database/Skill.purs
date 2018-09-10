module Database.Skill
  ( Amount(..), (~), toMin, toMax
  , Rank(..)
  , Target(..), allied
  , BuffEffect(..), BuffCategory(..), buffCategory
  , DebuffEffect(..)
  , InstantEffect(..), isDamage
  , BonusEffect(..)
  , ActiveEffect(..), demerit, simplify, ranges
  , Active
  , RangeInfo(..)
  , apAmount, mapAmount
  ) where

import Prelude
import Operators
import Generic as G

import Control.Alternative
import Control.Bind
import Control.Plus
import Data.Foldable
import Data.Number.Format
import Data.Int
import Data.Maybe
import Data.String
import Data.String.CodeUnits (toCharArray)
import Data.Tuple

import Database.Base
import Database.Icon
import Printing 

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
instance _c_ :: Show BuffEffect where
    show = showBuff Someone Placeholder

showBuff :: Target -> Amount -> BuffEffect -> String
showBuff target amount = case _ of
    AttackUp        -> increase "attack"
    AttackVs t    -> increase $ "attack" <> against t
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
    StarAffinity c  -> increase $ "C. Star generation" <> against c
    StarUp          -> increase "C. Star drop rate"
    Success debuff  -> success $ G.genericShow debuff
    SureHit         -> grant "Sure Hit"
    Taunt           -> "Draw attention of all enemies" <> to
    StarsPerTurn    -> "Gain " <> n <> " stars every turn" <> case target of
        Self -> " for yourself"
        _    -> ""
  where
    n   = show amount
    p:s = possessiveAndSubject target
    to  = if s == "" then "" else " to" <> s
    by  = " by " <> n <> "%"
    grant    x = "Grant" <> s <> " " <> x
    increase x = "Increase" <> p <> " " <> x <> by
    success  x = increase $ x <> " success rate"
    resist   x
      | amount == Full = "Grant" <> s <> " " <> x <> " immunity"
      | otherwise      = "Increase" <> p <> " " <> x <> " resistance" <> by
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
instance _d_ :: Show DebuffEffect where
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
    n   = show amount
    p:s = possessiveAndSubject target
    to  = if s == "" then "" else " to" <> s
    reduce   x = "Reduce" <> p <> " " <> x <> " by " <> n <> "%"
    unresist x = reduce $ x <> " resistance"
    damage   x = "Inflict " <> n <> " " <> x <> " damage" <> to <> " every turn"
    eachTurn x perTurn = "Inflict " <> x <> " status" <> to <> ", causing "
                         <> n <> "% chance to " <> perTurn <> " every turn"

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
instance _e_ :: Show InstantEffect where
    show = showInstant Someone Placeholder

isDamage :: InstantEffect -> Boolean
isDamage Avenge = true
isDamage Damage = true
isDamage DamageThruDef = true
isDamage (DamageVs _) = true
isDamage DamagePoison = true
isDamage LastStand = true
isDamage _ = false

showInstant :: Target -> Amount -> InstantEffect -> String
showInstant target amount = case _ of
    Avenge
      -> "Wait 1 turn, then deal " <> n
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
      -> not full ? append (n <> "% chance to ") $ "Instant-Kill " <> s
    GainStars
      -> "Gain " <> n <> " critical stars" <> case target of
          Self -> " for yourself"
          _    -> ""
  where
    n    = show amount
    p:s  = possessiveAndSubject target
    to   = if s == "" then "" else " to" <> s
    full = amount == Full

data BonusEffect
    = FriendPoints
    | QuestQuartz
    | QPGain
    | EXP
    | EXPPerc
    | MysticCode
    | Bond
instance _f_ :: Show BonusEffect where
  show = showBonus Placeholder

showBonus :: Amount -> BonusEffect -> String
showBonus amount = case _ of
    FriendPoints
      -> "Friend Points obtained from support becomes +" <> n
    QuestQuartz
      -> "Increase QP earned from completing quests by " <> n
    QPGain
      -> "Increase QP from enemy drops by " <> n <> "%"
    EXP
      -> "Increase Master EXP gain by " <> n
    EXPPerc
      -> "Increase Master EXP gain by " <> n <> "%"
    MysticCode
      -> "Increase Mystic Code EXP gain by " <> n
    Bond
      -> "Increase Bond Points gain by " <> n
  where
    n = show amount

-- | Int field is duration, Number field is amount
data ActiveEffect
    = Grant Target Int BuffEffect Amount
    | Debuff Target Int DebuffEffect Amount
    | To Target InstantEffect Amount
    | Bonus BonusEffect Amount
    | Chance Int ActiveEffect
    | Chances Int Int ActiveEffect
    | When String ActiveEffect
    | Times Int ActiveEffect
    | ToMax Amount ActiveEffect

apAmount :: (Number -> Number -> Number) -> Amount -> Number
apAmount f (Range a b) = f a b
apAmount _ (Flat a) = a
apAmount _ Placeholder = 0.0
apAmount _ Full = 0.0

mapAmount :: (Number -> Number -> Amount) -> ActiveEffect -> ActiveEffect
mapAmount f eff = go eff
  where
    f' (Range a b) = f a b
    f' a = a
    go (Grant a b c d)  = Grant a b c $ f' d
    go (Debuff a b c d) = Debuff a b c $ f' d
    go (To a b c)       = To a b $ f' c
    go (Bonus a b)      = Bonus a $ f' b
    go (Chance a b)     = Chance a $ go b
    go (When a b)       = When a $ go b
    go (Times a b)      = Times a $ go b
    go (ToMax a b)      = ToMax (f' a) $ go b
    go (Chances a b c)  = case f (toNumber a) (toNumber b) of
        Flat amt    -> Chance (floor amt) $ go c
        Range a' b' -> Chances (floor a') (floor b') $ go c
        Placeholder -> go c
        Full        -> go c

simplify :: ActiveEffect -> ActiveEffect
simplify (Chance _ ef)    = simplify ef
simplify (Chances _ _ ef) = simplify ef
simplify (When _ ef)      = simplify ef
simplify (Times _ ef)     = simplify ef
simplify (ToMax _ ef)   = simplify ef
simplify ef               = ef

demerit :: ActiveEffect -> Boolean
demerit (Grant t _ _ _) = not $ allied t
demerit (Debuff t _ _ _) = allied t
demerit (To _ DemeritBuffs _) = true
demerit (To _ DemeritCharge _) = true
demerit (To _ DemeritDamage _) = true
demerit (To _ DemeritGauge _) = true
demerit (To _ DemeritHealth _) = true
demerit (To _ DemeritKill _) = true
demerit (To _ _ _) = false
demerit (Bonus _ _) = false
demerit (Chance _ ef) = demerit ef
demerit (Chances _ _ ef) = demerit ef
demerit (When _ ef) = demerit ef
demerit (Times _ ef) = demerit ef
demerit (ToMax _ ef) = demerit ef

instance _g_ :: Show ActiveEffect where
  show = (_ <> ".") <<< go
    where
      go = case _ of
          Grant t dur buff amt -> showBuff t amt buff <> turns dur
          Debuff t dur deb amt -> showDebuff t amt deb <> turns dur
          To t instant amt     -> showInstant t amt instant
          Bonus bonus amt      -> showBonus amt bonus
          Chance 0 ef          -> "Chance to " <> uncap (go ef)
          Chance per ef        -> show per <> "% chance to " <> uncap (go ef)
          Chances a b ef       -> show a <> "~" <> show b <> "% chance to "
                                  <> uncap (go ef)
          When "attacking" ef  -> go ef <> " when attacking"
          When cond ef         -> "If " <> cond <> ": " <> uncap (go ef)
          Times 1 ef           -> go ef <> " (1 time)"
          Times times ef       -> go ef <> " (" <> show times <> " times)"
          ToMax amount ef -> go ef <> " every turn (max " <> show amount <> ")"
      turns 0   = ""
      turns 1   = " for 1 turn"
      turns dur = " for " <> show dur <> " turns"

uncap :: String -> String
uncap s = case uncons s of
    Just {head: x, tail: xs} -> toLower (singleton x) <> xs
    Nothing                  -> s

data Amount
    = Placeholder
    | Full
    | Flat Number
    | Range Number Number

infix 1 Range as ~

instance _h_ :: Show Amount where
  show Placeholder = "X"
  show Full = ""
  show (Flat x) = toString x
  show (Range a b) = toString a <> "~" <> toString b

toMin :: Amount -> Number
toMin Placeholder = 0.0
toMin Full = 0.0
toMin (Flat x) = x
toMin (Range a _) = a

toMax :: Amount -> Number
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
instance _b_ :: Show Rank where
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

possessiveAndSubject :: Target -> Tuple String String
possessiveAndSubject = case _ of
    Someone       -> ""
                   : ""
    Self          -> " own"
                   : " self"
    Ally          -> " one ally's"
                   : " one ally"
    Party         -> " party's"
                   : " party"
    Enemy         -> " one enemy's"
                   : " one enemy"
    Enemies       -> " all enemy"
                   : " all enemies"
    Others        -> " allies' (excluding self)"
                   : " allies (excluding self)"
    AlliesType t  -> " " <> show t <> " allies'"
                   : " " <> show t <> " allies"
    EnemyType t   -> " one " <> show t <> " enemy's"
                   : " one " <> show t <> " enemy"
    EnemiesType t -> " all " <> show t <> " enemy"
                   : " all " <> show t <> " enemies"
    Killer        -> " killer's"
                   : " killer"
    Target        -> " target's"
                   : " target"

data RangeInfo = RangeInfo Boolean Number Number
instance _i_ :: Show RangeInfo where
  show (RangeInfo true  from to) = print 2 from <> "% ~ " <> print 2 to <> "%"
  show (RangeInfo false from to) = print 2 from <>  " ~ " <> print 2 to
instance _j_ :: Eq RangeInfo where
  eq (RangeInfo _ a1 a2) (RangeInfo _ b1 b2) = a1 == b1 && a2 == b2

ranges :: ∀ f. Alternative f => Bind f => f ActiveEffect -> f RangeInfo
ranges = bindFlipped toInfo
  where
    toInfo eff = uncurry (RangeInfo $ isPercent eff) <$> acc eff
    isPercent = elem '%' <<< toCharArray <<< show
    acc (Grant _ _ _ a) = go a
    acc (Debuff _ _ _ a) = go a
    acc (To _ _ a) = go a
    acc (Bonus _ a) = go a
    acc (Chance _ ef) = acc ef
    acc (Chances a b ef) = pure (Tuple (toNumber a) (toNumber b)) <|> acc ef
    acc (When _ ef) = acc ef
    acc (Times _ ef) = acc ef
    acc (ToMax _ ef) = acc ef
    go (a ~ b) = pure $ Tuple a b
    go _ = empty

type Active = { name   :: String
              , icon   :: Icon
              , cd     :: Int
              , effect :: Array ActiveEffect
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
  show = drop 4 <<< G.genericShow

derive instance _38_ :: Eq ActiveEffect

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
