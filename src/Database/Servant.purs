module Database.Servant where

import Prelude
import Operators

import Data.Foldable           
import Data.Generic.Rep    
import Data.Generic.Rep.Show 
import Data.Tuple            (Tuple)

import Database.Skill
import Database.Passive
import Database.Trait

data Attribute = Mankind | Earth | Sky | Star

data Class = Saber | Archer | Lancer | Caster | Rider | Assassin | Berserker
           | Ruler | Avenger | MoonCancer | AlterEgo

data Card = Arts | Buster | Quick

data Deck = Deck Card Card Card Card Card

type Ratings = { damage     ∷ Int
               , np         ∷ Int
               , critical   ∷ Int
               , utility    ∷ Int
               , support    ∷ Int
               , durability ∷ Int
               }

type Stat = { atk ∷ Int, hp ∷ Int }

type Stats = { base ∷ Stat, max ∷ Stat, grail ∷ Stat }

type Gen = { starAbsorb ∷ Int
           , starGen    ∷ Number
           , npPerHit   ∷ Number
           , npAttacked ∷ Int
           }

type NoblePhantasm = { name   ∷ String 
                     , desc   ∷ String
                     , rank   ∷ Rank
                     , card   ∷ Card
                     , kind   ∷ String
                     , hits   ∷ Int
                     , effect ∷ Array ActiveEffect
                     , over   ∷ Array ActiveEffect
                     }

isMulti ∷ NoblePhantasm → Boolean
isMulti {effect} = any aoe $ getTarget ↤ effect
  where aoe Allies  = true
        aoe Party   = true
        aoe Enemies = true
        aoe _       = false

type Hits = { a ∷ Int, b ∷ Int, q ∷ Int, ex ∷ Int }

data Alignment = Lawful | Neutral | Chaotic | Good | Evil 
               | Mad | Summer | Bride

showAlignment ∷ Tuple Alignment Alignment → String
showAlignment = case _ of
    Neutral:Neutral → "True Neutral"
    a:b             → show a ⧺ " " ⧺ show b

newtype Servant = Servant { name     ∷ String
                          , rarity   ∷ Int
                          , class    ∷ Class
                          , attr     ∷ Attribute
                          , deck     ∷ Deck
                          , stats    ∷ Stats
                          , ratings  ∷ Ratings
                          , actives  ∷ Array Active
                          , passives ∷ Array (Tuple Passive Rank)
                          , phantasm ∷ NoblePhantasm
                          , gen      ∷ Gen
                          , hits     ∷ Hits
                          , traits   ∷ Array Trait
                          , death    ∷ Number
                          , align    ∷ Tuple Alignment Alignment
                          }
                          
hasActive ∷ ActiveEffect → Servant → Boolean
hasActive active (Servant s) = any (eq active) 
                             $ s.phantasm.effect ⧺ (s.actives ≫= _.effect) 

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Generic Alignment _
instance _1_ ∷ Show Alignment where
  show = genericShow

derive instance _2_ ∷ Generic Class _
instance _3_ ∷ Show Class where
  show = genericShow
