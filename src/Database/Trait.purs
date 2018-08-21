module Database.Trait where

import Prelude

import Data.Enum           
import Data.Generic.Rep    
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum 
import Data.Generic.Rep.Show 

data Trait = Arthur 
           | Beast 
           | Brynhildr
           | DemiServant
           | Demon
           | Demonic
           | Divine
           | Dragon
           | EarthOrSky
           | Female
           | GreekMyth
           | HeavenOrEarth
           | Humanoid
           | King
           | Male
           | Mecha
           | PseudoServant
           | Riding
           | Roman
           | Saberface
           | ThreatToHumanity
           | Undead
           | EnumaElish

instance showTrait ∷ Show Trait where
  show = case _ of
    Brynhildr        → "Brynhildr's Beloved"
    DemiServant      → "Demi-servant"
    EarthOrSky       → "Earth or Sky"
    GreekMyth        → "Greek Mythology Males"
    HeavenOrEarth    → "Heaven or Earth"
    PseudoServant    → "Pseudo-Servant"
    ThreatToHumanity → "Threat to Humanity"
    EnumaElish       → "Weak to Enuma Elish"
    a                → genericShow a

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Generic Trait _
derive instance _1_ ∷ Eq Trait
derive instance _2_ ∷ Ord Trait
instance _3_ ∷ Enum Trait where
  succ = genericSucc
  pred = genericPred
instance _4_ ∷ Bounded Trait where
  top = genericTop
  bottom = genericBottom
instance _5_ ∷ BoundedEnum Trait where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
