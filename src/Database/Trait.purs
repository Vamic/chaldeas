module Database.Trait where

import Prelude

import Data.Enum           
import Data.Generic.Rep    
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum 
import Data.Generic.Rep.Show 


data Class = Saber | Archer | Lancer | Caster | Rider | Assassin | Berserker
           | Shielder | Ruler | Avenger | MoonCancer | AlterEgo

data Trait = Arthur 
           | Beast 
           | Brynhildr
           | DemiServant
           | Demon
           | Demonic
           | Divine
           | Dragon
           | HeavenOrEarth
           | Female
           | GreekMyth
           | Humanoid
           | King
           | Magical
           | Male
           | Mecha
           | PseudoServant
           | Riding
           | Roman
           | Saberface
           | ThreatToHumanity
           | Undead
           | EnumaElish

instance _a_ ∷ Show Trait where
  show = case _ of
    Brynhildr        → "Brynhildr's Beloved"
    DemiServant      → "Demi-servant"
    HeavenOrEarth       → "Heaven or Earth"
    GreekMyth        → "Greek Mythology Males"
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

derive instance _13_ ∷ Eq Class
derive instance _14_ ∷ Ord Class
derive instance _15_ ∷ Generic Class _
instance _16_ ∷ Show Class where
  show = genericShow
instance _17_ ∷ Enum Class where
  succ = genericSucc
  pred = genericPred
instance _18_ ∷ Bounded Class where
  top = genericTop
  bottom = genericBottom
instance _19_ ∷ BoundedEnum Class where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
