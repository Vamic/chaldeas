module Database.Skill exposing
  ( Amount(..), showAmount, toMin, toMax
  , Rank(..), showRank
  , Target(..), allied
  , BuffEffect(..), showBuffEffect
  , BuffCategory(..), enumBuffCategory, showBuffCategory, buffCategory
  , DebuffEffect(..), showDebuffEffect
  , InstantEffect(..), showInstantEffect, isDamage
  , BonusEffect(..), showBonusEffect
  , SkillEffect(..), showSkillEffect, demerit, simplify, ranges
  , Skill
  , RangeInfo, OrdRangeInfo, ordRangeInfo, showRangeInfo
  , mapAmount
  )

import Printing        exposing (..)
import Database.Base   exposing (..)

type BonusEffect
    = Bond
    | EXP
    | FriendPoints
    | MysticCode
    | QPDrop
    | QPQuest

type InstantEffect
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

type DebuffEffect
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

type BuffEffect
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

showBonusEffect : Bool -> Amount -> BonusEffect -> String
showBonusEffect isPerc amount a = 
  let
    n =
      if isPerc then 
        showAmount amount ++ "%"
      else
        showAmount amount
    gain x = "Increase " ++ x ++ " gained by " ++ n
  in
    case a of
      Bond         -> gain "Bond Points"
      EXP          -> gain "Master EXP"
      FriendPoints -> "Friend Points obtained from support becomes +" ++ n
      MysticCode   -> gain "Mystic Code EXP"
      QPDrop       -> "Increase QP from completing quests by " ++ n
      QPQuest      -> "Increase QP from enemy drops by " ++ n


showInstantEffect : Target -> Amount -> InstantEffect -> String
showInstantEffect target amount a = 
  let
    n = showAmount amount
    {p, s} = possessiveAndSubject target
    to = case s of
      "" -> ""
      _  -> " to" ++ s
    full = amount == Full
  in
    case a of
      Avenge        -> "At the end of the next turn, deal " ++ n
                       ++ "% of damage taken during that turn" ++ to
      BecomeHyde    -> "Transform into Hyde. Class: [Berserker]. Star Weight: 9. Star Rate: 5%. NP/Hit: 1.02%. NP/Defend: 5%. Alignment: Chaotic Evil. Lose [" ++ showTrait Brynhild ++ "] trait. Skills are more effective"
      Cooldowns     -> "Reduce" ++ p ++ " cooldowns by " ++ n
      Cure          -> "Remove" ++ p ++ " poison debuffs"
      Damage        -> "Deal " ++ n ++ "% damage" ++ to
      DamageThruDef -> "Deal " ++ n ++ "% damage" ++ to ++ ", ignoring defense"
      DamageVs t    -> "Deal " ++ n ++ "% extra damage to [" 
                       ++ showTrait t ++ "]"
      DamagePoison  -> "Deal " ++ n ++ "% extra damage to [Poisoned]"
      DemeritBuffs  -> "Remove" ++ p ++ " buffs"
      DemeritCharge -> "Increase" ++ s ++ " NP gauge by " ++ n
      DemeritGauge  -> "Decrease" ++ p ++ " NP gauge by " ++ n ++ "%"
      DemeritDamage -> "Deal " ++ n ++ " damage" ++ to
      DemeritKill   -> "Sacrifice" ++ s ++ " (can trigger Guts)"
      DemeritHealth -> "Deal " ++ n ++ " damage" ++ to 
                      ++ " down to a minimum of 1"
      GaugeDown     -> "Reduce" ++ p ++ " NP gauge by " ++ n
      GaugeUp       -> "Increase" ++ p ++ " NP gauge by " ++ n ++ "%"
      Heal          -> "Restore " ++ (if full then "all" else n) ++ " HP" ++ to
      LastStand     -> "Deal up to " ++ n ++ "% damage based on missing health" 
                       ++ to
      OverChance    -> "Gain " ++ n ++ "% chance to apply Overcharge buffs"
      RemoveBuffs   -> "Remove" ++ p ++ " buffs"
      RemoveDebuffs -> "Remove" ++ p ++ " debuffs"
      RemoveMental  -> "Remove" ++ p ++ " mental debuffs"
      Kill          -> "Instant-Kill " ++ s ++ 
                       if not full then n ++ "% chance to " else ""
      GainStars     -> "Gain " ++ n ++ " critical stars" ++ case target of
                         Self -> " for yourself"
                         _    -> ""

showDebuffEffect : Target -> Amount -> DebuffEffect -> String
showDebuffEffect target amount a =
  let
    n      = showAmount amount
    {p, s} = possessiveAndSubject target
    to     = case s of
      "" -> ""
      _  -> " to" ++ s
    reduce   x = "Reduce" ++ p ++ " " ++ x ++ " by " ++ n ++ "%"
    unresist x = reduce <| x ++ " resistance"
    damage   x = "Inflict " ++ n ++ " " ++ x ++ " damage" ++ to ++ " every turn"
    eachTurn x perTurn = "Inflict " ++ x ++ " status" ++ to ++ ", causing "
                         ++ n ++ "% chance to " ++ perTurn ++ " every turn"
  in
    case a of
      ApplyTrait t -> "Apply [" ++ showTrait t ++ "]" ++ to
      AttackDown   -> reduce "attack"
      BuffBlock    -> "Inflict Buff Block status" ++ to
      BuffFail     -> reduce "attack buff success rate"
      Burn         -> damage "Burn"
      Charm        -> "Charm" ++ s
      CharmVuln    -> unresist "Charm"
      Confusion    -> eachTurn "Confusion" "Seal skills"
      CritChance   -> reduce "critical attack chance"
      CritDown     -> reduce "critical damage"
      Curse        -> damage "Curse"
      DamageVuln   -> "Increase" ++ s ++ " damage taken by " ++ n
      DeathDown    -> unresist "Instant-Death"
      DebuffVuln   -> unresist "debuff"
      DefenseDown  -> reduce "defense"
      Fear         -> eachTurn "Fear" "be Stunned"
      HealthLoss   -> "Decrease" ++ p ++ " HP by " ++ n ++ " per turn"
      MentalVuln   -> unresist "mental debuff"
      NPDown       -> reduce "NP Damage"
      Poison       -> damage "Poison"
      SealNP       -> "Seal" ++ p ++ " NP"
      SealSkills   -> "Seal" ++ p ++ " skills"
      StarDown     -> reduce "C. Star drop rate"
      Stun         -> "Stun" ++ s
      StunBomb     -> "Stun" ++ s ++ " after 1 turn"

showBuffEffect : Target -> Amount -> BuffEffect -> String
showBuffEffect target amount a = 
  let
    n       = showAmount amount
    {p, s}  = possessiveAndSubject target
    to      = case s of
      "" -> ""
      _  -> " to" ++ s
    by      = " by " ++ n ++ "%"
    grant    x = "Grant" ++ s ++ " " ++ x
    increase x = "Increase" ++ p ++ " " ++ x ++ by
    success  x = increase <| x ++ " success rate"
    resist   x = case amount of
      Full -> "Grant" ++ s ++ " " ++ x ++ " immunity"
      _    -> "Increase" ++ p ++ " " ++ x ++ " resistance" ++ by
    against : String -> String
    against x = " against [" ++ x ++ "]"
  in
    case a of
      AttackUp        -> increase "attack"
      AttackVs t      -> increase <| "attack" ++ against (showTrait t)
      AlignAffinity x -> increase <| "attack" ++ against (showAlignment x)
      Performance c   -> increase <| showCard c ++ " performance"
      BuffUp          -> success "buff"
      CritUp          -> increase "critical damage"
      ClassAffinity c -> increase <| "damage" ++ against (showClass c)
      DamageDown      -> "Reduce" ++ p ++ " damage taken by " ++ n
      DamageUp        -> "Increase" ++ p ++ " damage by " ++ n
      DebuffResist    -> resist "debuff"
      DebuffSuccess   -> success "debuff"
      DefenseUp       -> increase "defense"
      DefenseVs t   -> increase <| "defense" ++ against (showTrait t)
      Evasion         -> grant "Evasion"
      GaugePerTurn    -> "Charge" ++ p ++ " NP gauge" ++ by ++ " every turn"
      Guts            -> "Grant" ++ s ++ " Guts with " ++ n ++ " HP"
      GutsPercent     -> "Grant" ++ s ++ " Guts with " ++ n ++ "% HP"
      HealingReceived -> increase "healing received"
      HealPerTurn     -> "Restore " ++ n ++ " HP" ++ to
      HealUp          -> increase "healing power"
      IgnoreInvinc    -> grant "Ignore Invincibility"
      Invincibility   -> grant "Invincibility"
      KillResist      -> resist "Instant-Kill"
      KillUp          -> success "Instant-Kill"
      MaxHP           -> "Increase" ++ p ++ " Max HP by " ++ n
      MentalResist    -> resist "mental debuff"
      MentalSuccess   -> success "mental debuff"
      NPUp            -> increase "NP Damage"
      NPFromDamage    -> increase "NP generation rate when taking damage"
      NPGen           -> increase "NP generation rate"
      OffensiveResist -> resist "offensive debuff"
      Overcharge      -> "Overcharge" ++ p ++ " NP by " ++ n ++ " stages"
      Resist debuff   -> resist <| Debug.toString debuff ++ " debuff"
      StarAbsorb      -> increase "C. Star absorption"
      StarAffinity c  -> increase <| "C. Star drop" ++ against (showClass c)
      StarUp          -> increase "C. Star drop rate"
      Success debuff  -> success <| Debug.toString debuff
      SureHit         -> grant "Sure Hit"
      Taunt           -> "Draw attention of all enemies" ++ to
      StarsPerTurn    -> "Gain " ++ n ++ " stars every turn" ++ case target of
                           Self -> " for yourself"
                           _    -> ""

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

showBuffCategory : BuffCategory -> String
showBuffCategory = Debug.toString >> String.dropLeft 4

buffCategory : BuffEffect -> BuffCategory
buffCategory a = case a of
    AttackUp        -> BuffOffensive
    AttackVs _      -> BuffSpecialist
    AlignAffinity _ -> BuffSpecialist
    Performance _   -> BuffOffensive
    BuffUp          -> BuffUtility
    CritUp          -> BuffOffensive
    ClassAffinity _ -> BuffSpecialist
    DamageDown      -> BuffDefensive
    DamageUp        -> BuffOffensive
    DebuffResist    -> BuffUtility
    DebuffSuccess   -> BuffUtility
    DefenseUp       -> BuffDefensive
    DefenseVs _     -> BuffSpecialist
    Evasion         -> BuffDefensive
    GaugePerTurn    -> BuffSupport
    Guts            -> BuffDefensive
    GutsPercent     -> BuffDefensive
    HealingReceived -> BuffSupport
    HealPerTurn     -> BuffDefensive
    HealUp          -> BuffSupport
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
    StarAbsorb      -> BuffSupport
    StarAffinity _  -> BuffSpecialist
    StarUp          -> BuffSupport
    Success _       -> BuffUtility
    SureHit         -> BuffOffensive
    Taunt           -> BuffDefensive
    StarsPerTurn    -> BuffSupport

isDamage : InstantEffect -> Bool
isDamage a = case a of
  Avenge        -> True
  Damage        -> True
  DamageThruDef -> True
  DamageVs _    -> True
  DamagePoison  -> True
  LastStand     -> True
  _             -> False

-- | Int field is duration
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

showSkillEffect : SkillEffect -> String
showSkillEffect a = 
  let
    uncap s = case String.uncons s of
      Nothing -> s
      Just (head, tail) -> String.toLower (String.fromChar head) ++ tail
    go b = case b of
        Grant t dur buff amt -> showBuffEffect t amt buff ++ turns dur
        Debuff t dur deb amt -> showDebuffEffect t amt deb ++ turns dur
        To t instant amt     -> showInstantEffect t amt instant
        Bonus bonus perc amt -> showBonusEffect perc amt bonus
        Chance 0 ef          -> "Chance to " ++ uncap (go ef)
        Chance per ef        -> String.fromInt per ++ "% chance to " 
                                ++ uncap (go ef)
        Chances x y ef       -> String.fromInt x ++ "~" ++ String.fromInt y 
                                ++ "% chance to " ++ uncap (go ef)
        When "attacking" ef  -> go ef ++ " when attacking"
        When cond ef         -> "If " ++ cond ++ ": " ++ uncap (go ef)
        Times 1 ef           -> go ef ++ " (1 time)"
        Times times ef       -> go ef ++ " (" ++ String.fromInt times 
                                ++ " times)"
        ToMax amount ef      -> go ef ++ " every turn (max " 
                                ++ showAmount amount ++ ")"
    turns b = case b of
      0 -> ""
      1 -> " for 1 turn"
      _ -> " for " ++ String.fromInt b ++ " turns"
  in
    go a ++ "."

formatString : Float -> String
formatString x = String.fromFloat x

type Amount
    = Placeholder
    | Full
    | Flat Float
    | Range Float Float

showAmount : Amount -> String
showAmount a = case a of
  Placeholder -> "X"
  Full        -> ""
  Flat x      -> formatString x
  Range x y   -> formatString x ++ "~" ++ formatString y

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
    = Unknown | EX
    | APlusPlus | APlus | A | AMinus
    | BPlusPlus | BPlus | B | BMinus
    | CPlusPlus | CPlus | C | CMinus
                | DPlus | D
                | EPlus | E | EMinus

showRank : Rank -> String
showRank a = case a of
  Unknown   -> ""
  EX        -> " EX"
  APlusPlus -> " A++"
  APlus     -> " A+"
  A         -> " A"
  AMinus    -> " A-"
  BPlusPlus -> " B++"
  BPlus     -> " B+"
  B         -> " B"
  BMinus    -> " B-"
  CPlusPlus -> " C++"
  CPlus     -> " C+"
  C         -> " C"
  CMinus    -> " C-"
  DPlus     -> " D+"
  D         -> " D"
  EPlus     -> " E+"
  E         -> " E"
  EMinus    -> " E-"

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

possessiveAndSubject : Target -> { p : String, s : String }
possessiveAndSubject a = case a of
  Someone       -> { p = ""
                   , s = ""
                   }
  Self          -> { p = " own"
                   , s = " self"
                   }
  Ally          -> { p = " one ally's"
                   , s = " one ally"
                   }
  Party         -> { p = " party's"
                   , s = " party"
                   }
  Enemy         -> { p = " one enemy's"
                   , s = " one enemy"
                   }
  Enemies       -> { p = " all enemy"
                   , s = " all enemies"
                   }
  Others        -> { p = " allies' (excluding self)"
                   , s = " allies (excluding self)"
                   }
  AlliesType t  -> { p = " " ++ showTrait t ++ " allies'"
                   , s = " " ++ showTrait t ++ " allies"
                   }
  EnemyType t   -> { p = " one " ++ showTrait t ++ " enemy's"
                   , s = " one " ++ showTrait t ++ " enemy"
                   }
  EnemiesType t -> { p = " all " ++ showTrait t ++ " enemy"
                   , s = " all " ++ showTrait t ++ " enemies"
                   }
  Killer        -> { p = " killer's"
                   , s = " killer"
                   }
  Target        -> { p = " target's"
                   , s = " target"
                   }

type alias RangeInfo = 
    { percent : Bool
    , min     : Float
    , max     : Float
    }

showRangeInfo : RangeInfo -> String
showRangeInfo r =
    places 2 r.min ++ "% ~ " ++ places 2 r.max ++ if r.percent then "" else "%"

type alias OrdRangeInfo = String

ordRangeInfo : RangeInfo -> OrdRangeInfo
ordRangeInfo r = String.fromFloat r.min ++ String.fromFloat r.max

ranges : List SkillEffect -> List RangeInfo
ranges = 
  let
    toInfo ef              = List.map (info <| isPercent ef) <| acc ef
    isPercent              = showSkillEffect >> String.contains "%"
    info isPerc {from, to} = RangeInfo isPerc from to
    acc a = case a of
      Grant _ _ _ x  -> go x
      Debuff _ _ _ x -> go x
      To _ _ x       -> go x
      Bonus _ _ x    -> go x
      Chance _ ef    -> acc ef
      Chances x y ef -> {from = Basics.toFloat x, to = Basics.toFloat y} 
                        :: acc ef
      When _ ef      -> acc ef
      Times _ ef     -> acc ef
      ToMax _ ef     -> acc ef
    go a = case a of
      Range x y -> [{from = x, to = y}]
      _         -> []
  in 
    List.concatMap toInfo

type alias Skill = { name   : String 
                   , rank   : Rank
                   , icon   : Icon
                   , cd     : Int
                   , effect : List SkillEffect
                   }
