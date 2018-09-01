module Database.Icon (Icon(..)) where

import Prelude
import Operators
import Data.Enum
import Data.Generic.Rep
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum
import Data.Generic.Rep.Show
import Data.String

data Icon
    = IconAllUp
    | IconArtsUp
    | IconAvenger
    | IconBeamUp
    | IconBeamDown
    | IconBubbles
    | IconBullseye
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
    | IconNobleTurn
    | IconNobleUp
    | IconPotion
    | IconQuickUp
    | IconReaperUp
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
    | IconCrosshairUp
    | IconTeeth
    | IconYinYang

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Generic Icon _
derive instance _1_ ∷ Eq Icon
derive instance _2_ ∷ Ord Icon
instance _3_ ∷ Show Icon where
  show = drop 4 ∘ genericShow
instance _4_ ∷ Enum Icon where
  succ = genericSucc
  pred = genericPred
instance _5_ ∷ Bounded Icon where
  top = genericTop
  bottom = genericBottom
instance _6_ ∷ BoundedEnum Icon where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
