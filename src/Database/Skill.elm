module Database.Skill exposing
  ( Amount(..), toMin, toMax
  , Rank(..)
  , Target(..), Special(..), enumSpecial, allied
  , BuffEffect(..)
  , BuffCategory(..), enumBuffCategory, buffCategory
  , DebuffEffect(..)
  , InstantEffect(..), isDamage
  , BonusEffect(..)
  , SkillEffect(..), demerit, simplify
  , Skill
  , RangeInfo, OrdRangeInfo, ordRangeInfo
  , mapAmount
  )
{-| There are three types of skill effects in the game:
buffs, or positive status effects with a duration;
debuffs, or negative status effects with a duration;
and instant actions like gaining stars or reducing an enemy's NP gauge.

In CHALDEAS, Buffs are represented by `BuffEffect`s,
debuffs by `DebuffEffect`s, and instant actions by `InstantEffect`s.
This system is also used by passive skills, Noble Phantasm effects,
and Craft Essences.

For Craft Essences only, there are also `BonusEffect`s that increase gains
of Quartz Points, Bond, and so on. -}

import Database.Base exposing (..)

type BonusEffect
    = Bond
    | EXP
    | FriendPoints
    | MysticCode
    | QPDrop
    | QPQuest

type InstantEffect
    = ApplyAtRandom
    | Avenge
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
    | GaugeSpend
    | GaugeUp
    | Heal
    | Kill
    | LastStand
    | OverChance
    | Remove BuffEffect
    | RemoveBuffs
    | RemoveDebuffs
    | RemoveMental
    | SpecialDamage Special

type DebuffEffect
    = ApplyTrait Trait
    | AttackDown
    | BuffBlock
    | BuffFail
    | Burn
    | CardVuln Card
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

type BuffEffect
    = AttackUp
    | Performance Card
    | BuffUp
    | CritUp
    | DamageDown
    | DamageUp
    | DebuffResist
    | DebuffSuccess
    | DefenseUp
    | Evasion
    | GaugePerTurn
    | Guts
    | GutsPercent
    | HealPerTurn
    | HealingReceived
    | HealUp
    | HitCount
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
    | Special BuffEffect Special
    | StarAbsorb
    | StarUp
    | StarsPerTurn
    | Success DebuffEffect
    | SureHit
    | Taunt

type BuffCategory
    = BuffOffensive
    | BuffDefensive
    | BuffSupport
    | BuffUtility
    | BuffSpecialist

enumBuffCategory : List BuffCategory
enumBuffCategory =
    [ BuffOffensive
    , BuffDefensive
    , BuffSupport
    , BuffUtility
    , BuffSpecialist
    ]

buffCategory : BuffEffect -> BuffCategory
buffCategory a = case a of
    AttackUp        -> BuffOffensive
    Performance _   -> BuffOffensive
    BuffUp          -> BuffUtility
    CritUp          -> BuffOffensive
    DamageDown      -> BuffDefensive
    DamageUp        -> BuffOffensive
    DebuffResist    -> BuffUtility
    DebuffSuccess   -> BuffUtility
    DefenseUp       -> BuffDefensive
    Evasion         -> BuffDefensive
    GaugePerTurn    -> BuffSupport
    Guts            -> BuffDefensive
    GutsPercent     -> BuffDefensive
    HealingReceived -> BuffSupport
    HealPerTurn     -> BuffDefensive
    HealUp          -> BuffSupport
    HitCount        -> BuffSupport
    IgnoreInvinc    -> BuffOffensive
    Invincibility   -> BuffDefensive
    KillResist      -> BuffUtility
    KillUp          -> BuffUtility
    MaxHP           -> BuffDefensive
    MentalResist    -> BuffUtility
    MentalSuccess   -> BuffUtility
    NPUp            -> BuffOffensive
    NPFromDamage    -> BuffSupport
    NPGen           -> BuffSupport
    OffensiveResist -> BuffUtility
    Overcharge      -> BuffSupport
    Resist _        -> BuffUtility
    Special _ _     -> BuffSpecialist
    StarAbsorb      -> BuffSupport
    StarUp          -> BuffSupport
    StarsPerTurn    -> BuffSupport
    Success _       -> BuffUtility
    SureHit         -> BuffOffensive
    Taunt           -> BuffDefensive

isDamage : InstantEffect -> Bool
isDamage a = case a of
  Avenge          -> True
  Damage          -> True
  DamageThruDef   -> True
  DamageVs _      -> True
  DamagePoison    -> True
  LastStand       -> True
  SpecialDamage _ -> True
  _               -> False

{-| Int field is duration -}
type SkillEffect
    = Grant Target Int BuffEffect Amount
    | Debuff Target Int DebuffEffect Amount
    | To Target InstantEffect Amount
    | Bonus BonusEffect Bool Amount
    | Chance Int SkillEffect
    | Chances Int Int SkillEffect
    | When String SkillEffect
    | Times Int SkillEffect
    | ToMax Amount SkillEffect
    | After Int SkillEffect

mapAmount : (Float -> Float -> Amount) -> SkillEffect -> SkillEffect
mapAmount f eff =
  let
    f_ x = case x of
      Range a b -> f a b
      _         -> x
    go x = case x of
      Grant a b c d  -> Grant a b c <| f_ d
      Debuff a b c d -> Debuff a b c <| f_ d
      To a b c       -> To a b <| f_ c
      Bonus a b c    -> Bonus a b <| f_ c
      Chance a b     -> Chance a <| go b
      When a b       -> When a <| go b
      Times a b      -> Times a <| go b
      ToMax a b      -> ToMax (f_ a) <| go b
      After a b      -> After a <| go b
      Chances a b c  -> case f (toFloat a) (toFloat b) of
                          Flat y      -> Chance (floor y) <| go c
                          Range y z   -> Chances (floor y) (floor z) <| go c
                          Placeholder -> go c
                          Full        -> go c
  in
    go eff

simplify : SkillEffect -> SkillEffect
simplify a = case a of
  Chance _ ef    -> simplify ef
  Chances _ _ ef -> simplify ef
  When _ ef      -> simplify ef
  Times _ ef     -> simplify ef
  ToMax _ ef     -> simplify ef
  After _ ef     -> simplify ef
  _              -> a

demerit : SkillEffect -> Bool
demerit a = case a of
  Grant t _ _ _        -> not <| allied t
  Debuff t _ _ _       -> allied t
  To _ DemeritBuffs _  -> True
  To _ DemeritCharge _ -> True
  To _ DemeritDamage _ -> True
  To _ DemeritGauge _  -> True
  To _ DemeritHealth _ -> True
  To _ DemeritKill _   -> True
  To _ _ _             -> False
  Bonus _ _ _          -> False
  Chance _ ef          -> demerit ef
  Chances _ _ ef       -> demerit ef
  When _ ef            -> demerit ef
  Times _ ef           -> demerit ef
  ToMax _ ef           -> demerit ef
  After _ ef           -> demerit ef

type Amount
    = Placeholder
    | Full
    | Flat Float
    | Range Float Float

toMin : Amount -> Float
toMin a = case a of
  Placeholder -> 0
  Full -> 0
  Flat x -> x
  Range x _ -> x

toMax : Amount -> Float
toMax a = case a of
  Placeholder -> 0
  Full -> 0
  Flat x -> x
  Range _ y -> y

type Rank
    = Unknown | EX | APlusPlusPlus
    | APlusPlus | APlus | A | AMinus
    | BPlusPlus | BPlus | B | BMinus
    | CPlusPlus | CPlus | C | CMinus
                | DPlus | D
                | EPlus | E | EMinus

type Special
    = VsTrait Trait
    | VsClass Class
    | VsAlignment Alignment

enumSpecial : List Special
enumSpecial =
    List.map VsTrait enumTrait
    ++ List.map VsClass enumClass
    ++ List.map VsAlignment enumAlignment

type Target
    = Someone
    | Self | Ally | Party | Enemy | Enemies | Others
    | AlliesType Trait | EnemyType Trait | EnemiesType Trait
    | Killer | Target

allied : Target -> Bool
allied a = case a of
  Self         -> True
  Ally         -> True
  Party        -> True
  Others       -> True
  AlliesType _ -> True
  _            -> False

type alias RangeInfo =
    { percent : Bool
    , min     : Float
    , max     : Float
    }

type alias OrdRangeInfo = Float

ordRangeInfo : RangeInfo -> OrdRangeInfo
ordRangeInfo r = r.max

type alias Skill = { name   : String
                   , rank   : Rank
                   , icon   : Icon
                   , cd     : Int
                   , effect : List SkillEffect
                   }
