module Export (servants) where

import Prelude
import Operators
import Data.String.CodeUnits (toCharArray)

import Database  hiding (servants)
import Database  as D

servants ∷ Unit -> Array _
servants = \_ -> D.servants <#> \s@{align: alignA:alignB} -> 
    { name:          s.name
    , rarity:        s.rarity
    , class:         show s.class
    , attribute:     show s.attr
    , deck:          toCharArray $ show s.deck
    , stats:         s.stats
    , activeSkills:  exportActive <$> s.actives
    , passiveSkills: exportPassive <$> s.passives
    , noblePhantasm: exportPhantasm s.phantasm
    , starWeight:    s.gen.starWeight 
    , starRate:      s.gen.starRate
    , npAtk:         s.gen.npAtk
    , npDef:         s.gen.npDef
    , hits:          s.hits
    , traits:        show <$> s.traits
    , deathRate:     s.death
    , alignment:     [show alignA, show alignB]
    , limited:       s.limited
    }
  where
    exportAmount (Flat a)    = { from: a, to: a }
    exportAmount (a ~ b)     = { from: a, to: b }
    exportAmount Full        = { from: 0.0, to: 0.0 }
    exportAmount Placeholder = { from: 0.0, to: 0.0 }
    exportActive {name, icon, cd, effect} =
        { name
        , icon: show icon
        , cd 
        , effect: exportEffect <$> effect
        }
    exportPassive {name, rank, icon, effect} =
        { name: name ++ " " ++ show rank
        , icon: show icon
        , effect: exportEffect <$> effect
        }
    exportPhantasm {name, desc, rank, card, kind, hits, effect, over} =
        { name
        , desc
        , rank: show rank
        , card: show card
        , classification: kind
        , hits
        , effect: exportEffect <$> effect
        , over: exportEffect <$> over
        }
    exportEffect (Grant target duration effect amount) =
        { target: show target
        , duration
        , effect: show effect
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    exportEffect (Debuff target duration effect amount) =
        { target: show target
        , duration
        , effect: show effect
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    exportEffect (To target effect amount) =
        { target: show target
        , duration: (-1)
        , effect: show effect
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    exportEffect (Chance chance activeEffect) = 
        (exportEffect activeEffect) { chance = {from: chance, to: chance} }  
    exportEffect (Chances a b activeEffect) = 
        (exportEffect activeEffect) { chance = {from: a, to: b} }  
    exportEffect (When condition activeEffect) =
        baseEffect { effect = "If " ++ condition ++ ": " ++ baseEffect.effect }
      where baseEffect = exportEffect activeEffect
