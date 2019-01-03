module Class.Show exposing (..)

{-| String representations.

# `Database.Base`
@docs alignment, attribute, card, class, stat, trait, icon, material
# `Database.Servant`
@docs deck, phantasmType
# `Database.Skill`
@docs amount, bonusEffect, buffCategory, buffEffect, debuffEffect, instantEffect, nameBuffEffect, nameDebuffEffect, possessiveAndSubject, rangeInfo, rank, skillEffect, special
# `Persist.Preferences`
@docs preference
# `Site.Base`
@docs filterTab, section
# `Sorting`
@docs sortBy
-}

import StandardLibrary     exposing (..)
import Database.Base       exposing (..)
import Database.Skill      exposing (..)
import Database.Servant    exposing (Deck(..), PhantasmType(..))
import Persist.Preferences exposing (..)
import Printing            exposing (..)
import Site.Base           exposing (..)
import Sorting             exposing (..)

-------
-- BASE
-------

alignment : Alignment -> String
alignment a = case a of
  Lawful   -> "Lawful"
  Neutral  -> "Neutral"
  Chaotic  -> "Chaotic"
  Good     -> "Good"
  Balanced -> "Balanced"
  Evil     -> "Evil"
  Mad      -> "Mad"
  Summer   -> "Summer"
  Bride    -> "Bride"

attribute : Attribute -> String
attribute a = case a of
  Mankind -> "Humanity"
  Earth   -> "Earth"
  Heaven  -> "Heaven"
  Star    -> "Star"

card : Card -> String
card a = case a of
  Quick  -> "Quick"
  Arts   -> "Arts"
  Buster -> "Buster"

class : Class -> String
class a = case a of
  Saber     -> "Saber"
  Archer    -> "Archer"
  Lancer    -> "Lancer"
  Caster    -> "Caster"
  Rider     -> "Rider"
  Assassin  -> "Assassin"
  Berserker -> "Berserker"
  Shielder  -> "Shielder"
  Ruler     -> "Ruler"
  Avenger   -> "Avenger"

stat : Stat -> String
stat {atk, hp} = "ATK: " ++ String.fromInt atk ++ ", HP: " ++ String.fromInt hp

trait : Trait -> String
trait a = case a of
  Arthur -> "Arthur"
  Beast  -> "Beast"
  BrynhildsBeloved -> "Brynhild's Beloved"
  DemiServant -> "Demi-servant"
  Demon -> "Demon"
  Demonic -> "Demonic"
  Divine -> "Divine"
  Dragon -> "Dragon"
  Female -> "Female"
  HeavenOrEarth -> "Heaven or Earth"
  GreekMythMale -> "Greek Mythology Male"
  Humanoid -> "Humanoid"
  Human -> "Human"
  Male -> "Male"
  Nonbinary -> "Nonbinary"
  PseudoServant -> "Pseudo-Servant"
  Riding -> "Riding"
  Roman -> "Roman"
  King -> "King"
  Saberface -> "Saberface"
  Undead -> "Undead"
  EnumaElish -> "Weak to Enuma Elish"

icon : Icon -> String
icon a = case a of
  IconAllUp -> "AllUp"
  IconArtsQuickUp -> "ArtsQuickUp"
  IconArtsUp -> "ArtsUp"
  IconAvenger -> "Avenger"
  IconBeamUp -> "BeamUp"
  IconBeamDown -> "BeamDown"
  IconBubbles -> "Bubbles"
  IconBullseye -> "Bullseye"
  IconBusterArtsUp -> "BusterArtsUp"
  IconBusterUp -> "BusterUp"
  IconCircuits -> "Circuits"
  IconClockUp -> "ClockUp"
  IconCrosshairUp -> "CrosshairUp"
  IconDamageUp -> "DamageUp"
  IconDarkMagic -> "DarkMagic"
  IconDash -> "Dash"
  IconDiamonds -> "Diamonds"
  IconDodge -> "Dodge"
  IconEclipse -> "Eclipse"
  IconExclamationUp -> "ExclamationUp"
  IconExclamationDown -> "ExclamationDown"
  IconFace -> "Face"
  IconFire -> "Fire"
  IconFlex -> "Flex"
  IconGoddess -> "Goddess"
  IconHeal -> "Heal"
  IconHealTurn -> "HealTurn"
  IconHealUp -> "HealUp"
  IconHeart -> "Heart"
  IconHoodUp -> "HoodUp"
  IconHoodDown -> "HoodDown"
  IconHoodX -> "HoodX"
  IconHorse -> "Horse"
  IconHPUp -> "HPUp"
  IconKneel -> "Kneel"
  IconMagicCircle -> "MagicCircle"
  IconMask -> "Mask"
  IconMissing -> "Missing"
  IconMystic -> "Mystic"
  IconNiffin -> "Niffin"
  IconNoble -> "Noble"
  IconNobleRedUp -> "NobleRedUp"
  IconNobleTurn -> "NobleTurn"
  IconNobleUp -> "NobleUp"
  IconPotion -> "Potion"
  IconQuartz -> "Quartz"
  IconQuickBusterUp -> "QuickBusterUp"
  IconQuickUp -> "QuickUp"
  IconRainbow -> "Rainbow"
  IconReaperUp -> "ReaperUp"
  IconRoad -> "Road"
  IconShield -> "Shield"
  IconShieldBreak -> "ShieldBreak"
  IconShieldDown -> "ShieldDown"
  IconShieldUp -> "ShieldUp"
  IconSpotlight -> "Spotlight"
  IconStaffUp -> "StaffUp"
  IconStar -> "Star"
  IconStarHaloUp -> "StarHaloUp"
  IconStarTurn -> "StarTurn"
  IconStarUp -> "StarUp"
  IconStun -> "Stun"
  IconSwordDown -> "SwordDown"
  IconSwordUp -> "SwordUp"
  IconSwordShieldUp -> "SwordShieldUp"
  IconSwords -> "Swords"
  IconSun -> "Sun"
  IconSunUp -> "SunUp"
  IconTeeth -> "Teeth"
  IconYinYang -> "YinYang"

material : Material -> String
material a = case a of
  CrystallizedLore -> "Crystallized Lore"
  Piece c -> class c ++ " Piece"
  Monument c -> class c ++ " Monument"
  GemOf c -> "Gem of " ++ class c
  MagicGemOf c -> "Magic Gem of " ++ class c
  SecretGemOf c -> "Secret Gem of " ++ class c
  QP -> "QP"
  BlackBeastGrease -> "Black Beast Grease"
  ClawOfChaos -> "Claw of Chaos"
  CursedBeastGallstone -> "Cursed Beast Gallstone"
  DeadlyPoisonousNeedle -> "Deadly Poisonous Needle"
  DragonFang -> "Dragon Fang"
  DragonsReverseScale -> "Dragon's Reverse Scale"
  EternalGear -> "Eternal Gear"
  EvilBone -> "Evil Bone"
  FoolsChain -> "Fool's Chain"
  ForbiddenPage -> "Forbidden Page"
  GhostLantern -> "Ghost Lantern"
  GreatKnightMedal -> "Great Knight Medal"
  HeartOfTheForeignGod -> "Heart of the Foreign God"
  HomunculusBaby -> "Homunculus Baby"
  LampOfEvilSealing -> "Lamp of Evil-Sealing"
  MeteorHorseshoe -> "Meteor Horseshoe"
  OctupletCrystals -> "Octuplet Crystals"
  PhoenixFeather -> "Phoenix Feather"
  PrimordialLanugo -> "Primordial Lanugo"
  ProofOfHero -> "Proof of Hero"
  ScarabOfWisdom -> "Scarab of Wisdom"
  SeedOfYggdrasil -> "Seed of Yggdrasil"
  SerpentJewel -> "Serpent Jewel"
  ShellOfReminiscence -> "Shell of Reminiscence"
  SpiritRoot -> "Spirit Root"
  TearstoneOfBlood -> "Tearstone of Blood"
  VoidsDust -> "Void's Dust"
  WarhorsesYoungHorn -> "Warhorse's Young Horn"

----------
-- SERVANT
----------

deck : Deck -> String
deck (Deck a b c d e) =
    [a, b, c, d, e]
    |> List.map (card >> String.left 1)
    >> String.concat

phantasmType : PhantasmType -> String
phantasmType a = case a of
  SingleTarget -> "Single-Target"
  MultiTarget  -> "Multi-Target"
  Support      -> "Support"

--------
-- SKILL
--------

buffCategory : BuffCategory -> String
buffCategory a = case a of
  BuffOffensive  -> "Offensive"
  BuffDefensive  -> "Defensive"
  BuffSupport    -> "Support"
  BuffUtility    -> "Utility"
  BuffSpecialist -> "Specialist"

amount : Amount -> String
amount a = case a of
  Placeholder -> "X"
  Full        -> ""
  Flat x      -> String.fromFloat x
  Range x y   -> String.fromFloat x ++ "~" ++ String.fromFloat y

rank : Rank -> String
rank a = case a of
  Unknown   -> ""
  EX        -> " EX"
  APlusPlusPlus -> " A+++"
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

rangeInfo : RangeInfo -> String
rangeInfo r =
    places 2 r.min ++ "% ~ " ++ places 2 r.max ++ if r.percent then "" else "%"

special : Special -> String
special a = (\s -> "[" ++ s ++ "]") <| case a of
  VsTrait x     -> trait x
  VsClass x     -> class x
  VsAlignment x -> alignment x

bonusEffect : Bool -> Amount -> BonusEffect -> String
bonusEffect isPerc amt a =
  let
    n =
      if isPerc then
        amount amt ++ "%"
      else
        amount amt
    gain x = "Increase " ++ x ++ " gained by " ++ n
  in
    case a of
      Bond         -> gain "Bond Points"
      EXP          -> gain "Master EXP"
      FriendPoints -> "Friend Points obtained from support becomes +" ++ n
      MysticCode   -> gain "Mystic Code EXP"
      QPDrop       -> "Increase QP from completing quests by " ++ n
      QPQuest      -> "Increase QP from enemy drops by " ++ n

instantEffect : Target -> Amount -> InstantEffect -> String
instantEffect target amt a =
  let
    n = amount amt
    {p, s} = possessiveAndSubject target
    to = case s of
      "" -> ""
      _  -> " to" ++ s
    full = amt == Full
  in
    case a of
      ApplyAtRandom -> "Apply " ++ n ++ " random effect"
                       ++ (if amt /= Flat 1 then "s" else "") ++ " from below:"
      Avenge        -> "At the end of the next turn, deal " ++ n
                       ++ "% of damage taken during that turn" ++ to
      BecomeHyde    -> "Transform into Hyde. Class: [Berserker]. Star Weight: 9. Star Rate: 5%. NP/Hit: 1.02%. NP/Defend: 5%. Alignment: Chaotic Evil. Lose [" ++ trait BrynhildsBeloved ++ "] trait. Skills are more effective"
      Cooldowns     -> "Reduce" ++ p ++ " cooldowns by " ++ n
      Cure          -> "Remove" ++ p ++ " poison debuffs"
      Damage        -> "Deal " ++ n ++ "% damage" ++ to
      DamageThruDef -> "Deal " ++ n ++ "% damage" ++ to ++ ", ignoring defense"
      DamageVs t    -> "Deal " ++ n ++ "% extra damage to ["
                       ++ trait t ++ "]"
      DamagePoison  -> "Deal " ++ n ++ "% extra damage to [Poisoned]"
      DemeritBuffs  -> "Remove" ++ p ++ " buffs"
      DemeritCharge -> "Increase" ++ s ++ " NP gauge by " ++ n
      DemeritGauge  -> "Decrease" ++ p ++ " NP gauge by " ++ n ++ "%"
      DemeritDamage -> "Deal " ++ n ++ " damage" ++ to
      DemeritKill   -> "Sacrifice" ++ s ++ " (can trigger Guts)"
      DemeritHealth -> "Deal " ++ n ++ " damage" ++ to
                      ++ " down to a minimum of 1"
      GaugeDown     -> "Reduce" ++ p ++ " NP gauge by " ++ n
      GaugeSpend    -> "Costs " ++ n ++ "% of" ++ p ++ " NP gauge to use"
      GaugeUp       -> "Increase" ++ p ++ " NP gauge by " ++ n ++ "%"
      Heal          -> "Restore " ++ (if full then "all" else n) ++ " HP" ++ to
      LastStand     -> "Deal up to " ++ n ++ "% damage based on missing health"
                       ++ to
      OverChance    -> "Gain " ++ n ++ "% chance to apply Overcharge buffs"
      Remove ef     -> "Remove" ++ p ++ " " ++ nameBuffEffect ef ++ " buffs"
      RemoveBuffs   -> "Remove" ++ p ++ " buffs"
      RemoveDebuffs -> "Remove" ++ p ++ " debuffs"
      RemoveMental  -> "Remove" ++ p ++ " mental debuffs"
      Kill          -> (if full then identity else (++) <| n ++ "% chance to ") 
                       <| "Instant-Kill" ++ s
      GainStars     -> "Gain " ++ n ++ " critical stars" ++ case target of
                         Self -> " for yourself"
                         _    -> ""
      SpecialDamage x -> "Deal " ++ n ++ "% extra damage to " ++ special x

debuffEffect : Target -> Amount -> DebuffEffect -> String
debuffEffect target amt a =
  let
    n      = amount amt
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
      ApplyTrait t -> "Apply [" ++ trait t ++ "]" ++ to
      AttackDown   -> reduce "attack"
      BuffBlock    -> "Inflict Buff Block status" ++ to
      BuffFail     -> reduce "attack buff success rate"
      Burn         -> damage "Burn"
      CardVuln c   -> reduce <| "defense against " ++ card c ++ " cards"
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

nameDebuffEffect : DebuffEffect -> String
nameDebuffEffect a = case a of
  ApplyTrait _ -> "ApplyTrait"
  AttackDown   -> "AttackDown"
  BuffBlock    -> "BuffBlock"
  BuffFail     -> "BuffFail"
  Burn         -> "Burn"
  CardVuln _   -> "CardVuln"
  Charm        -> "Charm"
  CharmVuln    -> nameDebuffEffect DebuffVuln
  Confusion    -> "Confusion"
  CritChance   -> "CritChance"
  CritDown     -> "CritDown"
  Curse        -> "Curse"
  DamageVuln   -> "DamageVuln"
  DeathDown    -> "DeathDown"
  DebuffVuln   -> "DebuffVuln"
  DefenseDown  -> "DefenseDown"
  Fear         -> "Fear"
  HealthLoss   -> "HealthLoss"
  MentalVuln   -> nameDebuffEffect DebuffVuln
  NPDown       -> "NPDown"
  Poison       -> "Poison"
  SealNP       -> "SealNP"
  SealSkills   -> "SealSkills"
  StarDown     -> "StarDown"
  Stun         -> "Stun"

buffEffect : Target -> Amount -> BuffEffect -> String
buffEffect target amt a =
  let
    n       = amount amt
    {p, s}  = possessiveAndSubject target
    to      = case s of
      "" -> ""
      _  -> " to" ++ s
    by      = " by " ++ n ++ "%"
    grant    x = "Grant" ++ s ++ " " ++ x
    increase x = "Increase" ++ p ++ " " ++ x ++ by
    success  x = increase <| x ++ " success rate"
    resist   x = case amt of
      Full -> "Grant" ++ s ++ " " ++ x ++ " immunity"
      _    -> "Increase" ++ p ++ " " ++ x ++ " resistance" ++ by
    against : String -> String
    against x = " against [" ++ x ++ "]"
  in
    case a of
      AttackUp        -> increase "attack"
      Performance c   -> increase <| card c ++ " performance"
      BuffUp          -> success "buff"
      CritUp          -> increase "critical damage"
      DamageDown      -> "Reduce" ++ p ++ " damage taken by " ++ n
      DamageUp        -> "Increase" ++ p ++ " damage by " ++ n
      DebuffResist    -> resist "debuff"
      DebuffSuccess   -> success "debuff"
      DefenseUp       -> increase "defense"
      Evasion         -> grant "Evasion"
      GaugePerTurn    -> "Charge" ++ p ++ " NP gauge" ++ by ++ " every turn"
      Guts            -> "Grant" ++ s ++ " Guts with " ++ n ++ " HP"
      GutsPercent     -> "Grant" ++ s ++ " Guts with " ++ n ++ "% HP"
      HealingReceived -> increase "healing received"
      HealPerTurn     -> "Restore " ++ n ++ " HP" ++ to
      HealUp          -> increase "healing power"
      HitCount        -> "Multiply number of hits per attack by " ++ n ++ "%"
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
      Resist debuff   -> resist <| nameDebuffEffect debuff
      Special ef x    -> buffEffect target amt ef ++  " against " ++ special x
      StarAbsorb      -> increase "C. Star absorption"
      StarUp          -> increase "C. Star drop rate"
      Success debuff  -> success <| nameDebuffEffect debuff
      SureHit         -> grant "Sure Hit"
      Taunt           -> "Draw attention of all enemies" ++ to
      StarsPerTurn    -> "Gain " ++ n ++ " stars every turn"

nameBuffEffect : BuffEffect -> String
nameBuffEffect a = case a of
  AttackUp        -> "AttackUp"
  Performance c   -> "Performance " ++ card c
  BuffUp          -> "BuffUp"
  CritUp          -> "CritUp"
  DamageDown      -> "DamageDown"
  DamageUp        -> "DamageUp"
  DebuffResist    -> "DebuffResist"
  DebuffSuccess   -> "DebuffSuccess"
  DefenseUp       -> "DefenseUp"
  Evasion         -> "Evasion"
  GaugePerTurn    -> "GaugePerTurn"
  Guts            -> "Guts"
  GutsPercent     -> nameBuffEffect Guts
  HealingReceived -> "HealingReceived"
  HealPerTurn     -> "HealPerTurn"
  HealUp          -> "HealUp"
  HitCount        -> "HitCount"
  IgnoreInvinc    -> "IgnoreInvinc"
  Invincibility   -> "Invincibility"
  KillResist      -> "KillResist"
  KillUp          -> "KillUp"
  MaxHP           -> "MaxHP"
  MentalResist    -> nameBuffEffect DebuffResist
  MentalSuccess   -> nameBuffEffect DebuffSuccess
  NPUp            -> "NPUp"
  NPFromDamage    -> "NPFromDamage"
  NPGen           -> "NPGen"
  OffensiveResist -> nameBuffEffect DebuffResist
  Overcharge      -> "Overcharge"
  Resist _        -> nameBuffEffect DebuffResist
  Special ef _    -> nameBuffEffect ef
  StarAbsorb      -> "StarAbsorb"
  StarUp          -> "StarUp"
  Success _       -> nameBuffEffect DebuffSuccess
  SureHit         -> "SureHit"
  Taunt           -> "Taunt"
  StarsPerTurn    -> "StarsPerTurn"

skillEffect : SkillEffect -> String
skillEffect =
  let
    uncap s = case String.uncons s of
      Nothing           -> s
      Just (head, tail) -> String.toLower (String.fromChar head) ++ tail
    addPeriod s = if String.endsWith ":" s then s else s ++ "."
    go a = case a of
      Grant t dur buff amt -> buffEffect t amt buff ++ turns dur
      Debuff t dur deb amt -> debuffEffect t amt deb ++ turns dur
      To t instant amt     -> instantEffect t amt instant
      Bonus bonus perc amt -> bonusEffect perc amt bonus
      Chance 0 ef          -> "Chance to " ++ uncap (go ef)
      Chance per ef        -> String.fromInt per ++ "% chance to "
                              ++ uncap (go ef)
      Chances x y ef       -> String.fromInt x ++ "~" ++ String.fromInt y
                              ++ "% chance to " ++ uncap (go ef)
      When "attacking" ef  -> go ef ++ " when attacking"
      When cond ef         -> "If " ++ cond ++ ": " ++ uncap (go ef)
      Times 1 ef           -> go ef ++ " (1 time)"
      Times times ef       -> go ef ++ " (" ++ String.fromInt times ++ " times)"
      ToMax amt ef         -> go ef ++ " every turn (max " ++ amount amt ++ ")"
      After 1 ef           -> "After 1 turn: " ++ go ef
      After amt ef         -> "After " ++ String.fromInt amt ++ " turns: " 
                              ++ go ef
    turns a = case a of
      0 -> ""
      1 -> " for 1 turn"
      _ -> " for " ++ String.fromInt a ++ " turns"
  in
    go >> addPeriod

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
  AlliesType t  -> { p = " " ++ trait t ++ " allies'"
                   , s = " " ++ trait t ++ " allies"
                   }
  EnemyType t   -> { p = " one " ++ trait t ++ " enemy's"
                   , s = " one " ++ trait t ++ " enemy"
                   }
  EnemiesType t -> { p = " all " ++ trait t ++ " enemy"
                   , s = " all " ++ trait t ++ " enemies"
                   }
  Killer        -> { p = " killer's"
                   , s = " killer"
                   }
  Target        -> { p = " target's"
                   , s = " target"
                   }

--------------
-- PREFERENCES
--------------

preference : Preference -> String
preference a = case a of
  AddSkills    -> "Add skills to NP damage"
  Artorify     -> "Artorify"
  ExcludeSelf  -> "Exclude self-applied effects"
  HideClasses  -> "Hide (Class) in names"
  MaxAscension -> "Show all at max ascension"
  NightMode    -> "Night Mode"
  ShowTables   -> "Show skill and NP tables"
  Thumbnails   -> "Thumbnails"

----------
-- Base
----------

section : Section -> String
section a = case a of
  SectionBrowse   -> "Browse"
  SectionSettings -> "Settings"
  SectionSortBy   -> "Sort By"
  SectionInclude  -> "Include"
  SectionFilter   -> "Filter"

filterTab : FilterTab -> String
filterTab a = case a of
  FilterAction       -> "Action"
  FilterAlignment    -> "Alignment"
  FilterAttribute    -> "Attribute"
  FilterAvailability -> "Availability"
  FilterBonus        -> "Bonus"
  FilterBuff c       -> "Buff (" ++ buffCategory c ++ ")"
  FilterCard         -> "NP Card"
  FilterClass        -> "Class"
  FilterDamage       -> "Damage"
  FilterDebuff       -> "Debuff"
  FilterDeck         -> "Deck"
  FilterEventBonus   -> "Event Bonus"
  FilterMaterial     -> "Material"
  FilterPassiveSkill -> "Passive Skill"
  FilterPhantasm     -> "NP Type"
  FilterRarity       -> "Rarity"
  FilterSource       -> "Source"
  FilterTrait        -> "Trait"

----------
-- SORTING
----------

sortBy : SortBy -> String
sortBy a = case a of
  ATK        -> "ATK"
  HP         -> "HP"
  ID         -> "ID"
  NPArts     -> "NP Gain per Arts card"
  NPDeck     -> "NP Gain per full deck"
  NPDmg      -> "NP Damage"
  NPDmgOver  -> "NP Damage + Overcharge"
  NPSpec     -> "NP Special Damage"
  NPSpecOver -> "NP Special + Overcharge"
  Rarity     -> "Rarity"
  StarDeck   -> "Stars per full deck"
  StarQuick  -> "Stars per Quick card"
  StarWeight -> "Star Weight"
