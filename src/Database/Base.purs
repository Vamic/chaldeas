module Database.Base
  ( Alignment(..), showAlignment
  , Attribute(..)
  , Card(..)
  , Class(..)
  , Stat(..), showStat
  , Trait(..)
  ) where

import Prelude
import Operators ((:))
import Generic as G

import Data.Tuple (Tuple)


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
    a                -> G.genericShow a

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ G.Generic Trait _
derive instance _1_ ∷ Eq Trait
derive instance _2_ ∷ Ord Trait
instance _3_ ∷ G.Enum Trait where
  succ = G.genericSucc
  pred = G.genericPred
instance _4_ ∷ G.Bounded Trait where
  top = G.genericTop
  bottom = G.genericBottom
instance _5_ ∷ G.BoundedEnum Trait where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum

derive instance _13_ ∷ Eq Class
derive instance _14_ ∷ Ord Class
derive instance _15_ ∷ G.Generic Class _
instance _16_ ∷ Show Class where
  show = G.genericShow
instance _17_ ∷ G.Enum Class where
  succ = G.genericSucc
  pred = G.genericPred
instance _18_ ∷ G.Bounded Class where
  top = G.genericTop
  bottom = G.genericBottom
instance _19_ ∷ G.BoundedEnum Class where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum

derive instance _20_ ∷ Eq Alignment
derive instance _21_ ∷ Ord Alignment
derive instance _22_ ∷ G.Generic Alignment _
instance _23_ ∷ Show Alignment where
  show = G.genericShow
instance _24_ ∷ G.Enum Alignment where
  succ = G.genericSucc
  pred = G.genericPred
instance _25_ ∷ G.Bounded Alignment where
  top = G.genericTop
  bottom = G.genericBottom
instance _26_ ∷ G.BoundedEnum Alignment where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum

derive instance _27_ ∷ Eq Attribute
derive instance _28_ ∷ Ord Attribute
derive instance _29_ ∷ G.Generic Attribute _
instance _30_ ∷ Show Attribute where
  show = G.genericShow
instance _31_ ∷ G.Enum Attribute where
  succ = G.genericSucc
  pred = G.genericPred
instance _32_ ∷ G.Bounded Attribute where
  top = G.genericTop
  bottom = G.genericBottom
instance _33_ ∷ G.BoundedEnum Attribute where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum

derive instance _34_ ∷ Eq Card
derive instance _35_ ∷ Ord Card
derive instance _36_ ∷ G.Generic Card _
instance _37_ ∷ G.Enum Card where
  succ = G.genericSucc
  pred = G.genericPred
instance _38_ ∷ G.Bounded Card where
  top = G.genericTop
  bottom = G.genericBottom
instance _39_ ∷ G.BoundedEnum Card where
  cardinality = G.genericCardinality
  toEnum = G.genericToEnum
  fromEnum = G.genericFromEnum
instance _40_ ∷ Show Card where
  show = G.genericShow
