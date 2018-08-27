module Export (servants) where

import Prelude
import Operators
import Data.String.CodeUnits (toCharArray)

import Database  hiding (servants)
import Database  as D

servants ∷ Unit -> Array _
servants = \_ -> D.servants <#> \(Servant s@{align: alignA:alignB}) -> 
    { name:          s.name
    , rarity:        s.rarity
    , class:         show s.class
    , attribute:     show s.attr
    , deck:          toCharArray $ show s.deck
    , stats:         s.stats
    , ratings:       s.ratings
    , activeSkills:  exportActive <$> s.actives
    , passiveSkills: exportPassive <$> s.passives
    , noblePhantasm: exportPhantasm s.phantasm
    , starWeight:    s.gen.starWeight 
    , starRate:      s.gen.starRate
    , npPerHit:      s.gen.npPerHit
    , npPerDefend:   s.gen.npPerDefend
    , hits:          s.hits
    , traits:        show <$> s.traits
    , deathRate:     s.death
    , alignment:     [show alignA, show alignB]
    }
  where
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
        , amount
        , chance: 100
        }
    exportEffect (Debuff target duration effect amount) =
        { target: show target
        , duration
        , effect: show effect
        , amount
        , chance: 100
        }
    exportEffect (To target effect amount) =
        { target: show target
        , duration: (-1)
        , effect: show effect
        , amount
        , chance: 100
        }
    exportEffect (Chance chance activeEffect) = 
        (exportEffect activeEffect) { chance = chance }  
    exportEffect (When condition activeEffect) =
        baseEffect { effect = "If " ++ condition ++ ": " ++ baseEffect.effect }
      where baseEffect = exportEffect activeEffect
