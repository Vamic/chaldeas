module Database.Icon (Icon(..)) where

import Prelude
import Data.Enum         
import Data.Generic.Rep    
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum 
import Data.Generic.Rep.Show 

data Icon 
  = IconArtsUp
  | IconAvenger
  | IconBeamUp
  | IconBeamDown
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
  | IconFire
  | IconGoddess
  | IconGuts
  | IconHeal
  | IconHeart
  | IconHoodUp
  | IconHorse
  | IconMagicCircle
  | IconMask
  | IconMissing
  | IconNiffin
  | IconNoble
  | IconNobleTurn
  | IconNobleUp
  | IconPotion
  | IconQuickUp
  | IconShield
  | IconShieldUp
  | IconSpotlight
  | IconStarTurn
  | IconStarUp
  | IconStun
  | IconSun
  | IconSwordUp
  | IconTargetUp
  | IconTeeth

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Generic Icon _
derive instance _1_ ∷ Eq Icon
derive instance _2_ ∷ Ord Icon
instance _3_ ∷ Show Icon where
  show = genericShow
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
