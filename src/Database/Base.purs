-- | Basic Enum datatypes to represent game information.
module Database.Base
  ( Alignment(..)
  , Attribute(..)
  , Card(..)
  , Class(..)
  , Stat(..), showStat, addStats
  , Trait(..)
  , Icon(..)
  ) where

import StandardLibrary
import Generic     as G
import Data.String as String

-- Note: 'No alignment', as in the case of Nursery Rhyme, is an empty array
-- rather than a separate alignment.
data Alignment 
    = Lawful | Neutral | Chaotic | Good | Balanced | Evil
    | Mad | Summer | Bride

-- | Determines the "attribute modifier" in `Database.Calculator`. 
-- | Currently used only for filters.
-- Note: There is a fifth category in the game, "Beast", 
-- which is not represented here because only enemies can have it.
data Attribute = Mankind | Earth | Heaven | Star

-- | The building blocks of a Servant's Deck and NP type.
-- Note: "Extra" cards, also known as EX, are not represented as they only
-- occur at the end of a Brave Combo.
data Card = Quick | Arts | Buster

-- | Determines the "class attack bonus" and "triangle modifier" in
-- | in `Database.Calculator`.
-- Note: MoonCancer and AlterEgo are not represented as they are JP-only.
data Class 
    = Saber | Archer | Lancer | Caster | Rider | Assassin | Berserker
    | Shielder | Ruler | Avenger

-- | Craft Essences have minimum and maximum Stats.
-- | Servants have minimum, maximum, and grail Stats.
type Stat = { atk :: Int, hp :: Int }
showStat :: Stat -> String
showStat {atk, hp} = "ATK: " <> show atk <> ", HP: " <> show hp

-- | Used for Fou +ATK and +DEF bonuses.
addStats :: Stat -> Stat -> Stat
addStats x y = {atk: x.atk + y.atk, hp: x.hp + y.hp}

-- | Traits are miscellaneous flags attached to Servants. Since many traits are
-- | unlabeled, it can be difficult to figure out which are JP-only and which 
-- | translations are accurate to EN.
data Trait
    = Arthur
    | Beast
    | Brynhild
    | DemiServant
    | Demon
    | Demonic
    | Divine
    | Dragon
    | HeavenOrEarth
    | Female
    | GreekMythMale
    | Humanoid
    | Human
    | King
    | Male
    | Mecha
    | Nonbinary
    | PseudoServant
    | Riding
    | Roman
    | Saberface
    | ThreatToHumanity
    | Undead
    | EnumaElish

instance _b_ :: Show Trait where
    show Brynhild         = "Brynhild's Beloved"
    show DemiServant      = "Demi-servant"
    show HeavenOrEarth    = "Heaven/Earth"
    show GreekMythMale    = "Greek Mythology Males"
    show PseudoServant    = "Pseudo-Servant"
    show ThreatToHumanity = "Threat to Humanity"
    show EnumaElish       = "Weak to Enuma Elish"
    show x                = G.genericShow x

-- | Corresponding images are found in the [img/Skill](../../img/Skill/) folder.
-- | Every Icon is the name of an image file prefixed with 'Icon'. For example,
-- | `IconAllUp` corresponds to `AllUp.png`.
data Icon
    = IconAllUp
    | IconArtsQuickUp
    | IconArtsUp
    | IconAvenger
    | IconBeamUp
    | IconBeamDown
    | IconBubbles
    | IconBullseye
    | IconBusterArtsUp
    | IconBusterUp
    | IconCircuits
    | IconDamageUp
    | IconDarkMagic
    | IconDash
    | IconDiamonds
    | IconDodge
    | IconEclipse
    | IconExclamationUp
    | IconExclamationDown
    | IconFace
    | IconFire
    | IconFlex
    | IconGoddess
    | IconHeal
    | IconHealTurn
    | IconHealUp
    | IconHeart
    | IconHoodUp
    | IconHoodDown
    | IconHoodX
    | IconHorse
    | IconHPUp
    | IconKneel
    | IconMagicCircle
    | IconMask
    | IconMissing
    | IconMystic
    | IconNiffin
    | IconNoble
    | IconNobleRedUp
    | IconNobleTurn
    | IconNobleUp
    | IconPotion
    | IconQuartz
    | IconQuickBusterUp
    | IconQuickUp
    | IconRainbow
    | IconReaperUp
    | IconRoad
    | IconShield
    | IconShieldBreak
    | IconShieldDown
    | IconShieldUp
    | IconSpotlight
    | IconStaffUp
    | IconStar
    | IconStarHaloUp
    | IconStarTurn
    | IconStarUp
    | IconStun
    | IconSwordDown
    | IconSun
    | IconSunUp
    | IconSwordUp
    | IconSwordShieldUp
    | IconCrosshairUp
    | IconTeeth
    | IconYinYang

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ :: G.Generic Trait _
derive instance _1_ :: Eq Trait
derive instance _2_ :: Ord Trait
instance _3_ :: G.Enum Trait where
    succ = G.genericSucc
    pred = G.genericPred
instance _4_ :: G.Bounded Trait where
    top = G.genericTop
    bottom = G.genericBottom
instance _5_ :: G.BoundedEnum Trait where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _13_ :: Eq Class
derive instance _14_ :: Ord Class
derive instance _15_ :: G.Generic Class _
instance _16_ :: Show Class where
    show = G.genericShow
instance _17_ :: G.Enum Class where
    succ = G.genericSucc
    pred = G.genericPred
instance _18_ :: G.Bounded Class where
    top = G.genericTop
    bottom = G.genericBottom
instance _19_ :: G.BoundedEnum Class where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _20_ :: Eq Alignment
derive instance _21_ :: Ord Alignment
derive instance _22_ :: G.Generic Alignment _
instance _23_ :: Show Alignment where
    show = G.genericShow
instance _24_ :: G.Enum Alignment where
    succ = G.genericSucc
    pred = G.genericPred
instance _25_ :: G.Bounded Alignment where
    top = G.genericTop
    bottom = G.genericBottom
instance _26_ :: G.BoundedEnum Alignment where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _27_ :: Eq Attribute
derive instance _28_ :: Ord Attribute
derive instance _29_ :: G.Generic Attribute _
instance _30_ :: Show Attribute where
    show = G.genericShow
instance _31_ :: G.Enum Attribute where
    succ = G.genericSucc
    pred = G.genericPred
instance _32_ :: G.Bounded Attribute where
    top = G.genericTop
    bottom = G.genericBottom
instance _33_ :: G.BoundedEnum Attribute where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum

derive instance _34_ :: Eq Card
derive instance _35_ :: Ord Card
derive instance _36_ :: G.Generic Card _
instance _37_ :: G.Enum Card where
    succ = G.genericSucc
    pred = G.genericPred
instance _38_ :: G.Bounded Card where
    top = G.genericTop
    bottom = G.genericBottom
instance _39_ :: G.BoundedEnum Card where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum
instance _40_ :: Show Card where
    show = G.genericShow

derive instance _41_ :: G.Generic Icon _
derive instance _42_ :: Eq Icon
derive instance _43_ :: Ord Icon
instance _44_ :: Show Icon where
    show = String.drop 4 <<< G.genericShow
instance _45_ :: G.Enum Icon where
    succ = G.genericSucc
    pred = G.genericPred
instance _46_ :: G.Bounded Icon where
    top = G.genericTop
    bottom = G.genericBottom
instance _47_ :: G.BoundedEnum Icon where
    cardinality = G.genericCardinality
    toEnum = G.genericToEnum
    fromEnum = G.genericFromEnum
