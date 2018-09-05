module Database.Icon (Icon(..)) where

import Prelude
import Generic as G

import Data.String

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

derive instance _0_ ∷ G.Generic Icon _
derive instance _1_ ∷ Eq Icon
derive instance _2_ ∷ Ord Icon
instance _3_ ∷ Show Icon where
  show = drop 4 <<< G.genericShow
instance _4_ ∷ G.Enum Icon where
  succ = G.genericSucc
  pred = G.genericPred
instance _5_ ∷ G.Bounded Icon where
  top = G.genericTop
  bottom = G.genericBottom
instance _6_ ∷ G.BoundedEnum Icon where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum
