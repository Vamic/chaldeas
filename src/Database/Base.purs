module Database.Base
  ( Alignment(..), showAlignment
  , Attribute(..)
  , Card(..)
  , Class(..)
  , Stat(..), showStat
  , Trait(..)
  ) where

import Prelude
import Operators

import Data.Enum
import Data.Generic.Rep
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum
import Data.Generic.Rep.Show
import Data.Tuple

data Alignment = Lawful | Neutral | Chaotic | Good | Balanced | Evil
               | Mad | Summer | Bride

showAlignment ∷ Tuple Alignment Alignment -> String
showAlignment = case _ of
    Neutral:Balanced -> "True Neutral"
    a:b             -> show a <> " " <> show b

data Attribute = Mankind | Earth | Heaven | Star

data Card = Arts | Buster | Quick

data Class = Saber | Archer | Lancer | Caster | Rider | Assassin | Berserker
           | Shielder | Ruler | Avenger | MoonCancer | AlterEgo

type Stat = { atk ∷ Int, hp ∷ Int }
showStat ∷ Stat -> String
showStat {atk, hp} = "ATK: " <> show atk <> ", HP: " <> show hp

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
    | GreekMyth
    | Humanoid
    | Human
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

instance _b_ ∷ Show Trait where
  show = case _ of
    Brynhild        -> "Brynhild's Beloved"
    DemiServant      -> "Demi-servant"
    HeavenOrEarth    -> "Heaven or Earth"
    GreekMyth        -> "Greek Mythology Males"
    PseudoServant    -> "Pseudo-Servant"
    ThreatToHumanity -> "Threat to Humanity"
    EnumaElish       -> "Weak to Enuma Elish"
    a                -> genericShow a

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

derive instance _20_ ∷ Eq Alignment
derive instance _21_ ∷ Ord Alignment
derive instance _22_ ∷ Generic Alignment _
instance _23_ ∷ Show Alignment where
  show = genericShow
instance _24_ ∷ Enum Alignment where
  succ = genericSucc
  pred = genericPred
instance _25_ ∷ Bounded Alignment where
  top = genericTop
  bottom = genericBottom
instance _26_ ∷ BoundedEnum Alignment where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance _27_ ∷ Eq Attribute
derive instance _28_ ∷ Ord Attribute
derive instance _29_ ∷ Generic Attribute _
instance _30_ ∷ Show Attribute where
  show = genericShow
instance _31_ ∷ Enum Attribute where
  succ = genericSucc
  pred = genericPred
instance _32_ ∷ Bounded Attribute where
  top = genericTop
  bottom = genericBottom
instance _33_ ∷ BoundedEnum Attribute where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance _34_ ∷ Eq Card
derive instance _35_ ∷ Ord Card
derive instance _36_ ∷ Generic Card _
instance _37_ ∷ Enum Card where
  succ = genericSucc
  pred = genericPred
instance _38_ ∷ Bounded Card where
  top = genericTop
  bottom = genericBottom
instance _39_ ∷ BoundedEnum Card where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
instance _40_ ∷ Show Card where
  show = genericShow
