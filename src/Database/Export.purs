module Export (craftEssences, servants) where

import Prelude
import Data.Nullable
import Data.String.CodeUnits
import Data.Tuple

import Database as D
import Database

servants ∷ Unit -> Array _
servants = \_ -> D.servants <#> \s'@(Servant s@{align: Tuple alignA alignB}) ->
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
    , free:          s.free
    , bond:          toNullable $ getBond s'
    }

craftEssences ∷ Unit -> Array _
craftEssences = \_ -> D.craftEssences <#> \(CraftEssence ce) ->
    { name:    ce.name
    , id:      ce.id
    , rarity:  ce.rarity
    , icon:    show ce.icon
    , stats:   ce.stats
    , effect:  exportEffect <$> ce.effect
    , limited: ce.limited
    , bond:    toNullable ce.bond
    }

exportAmount ∷ Amount -> _
exportAmount (Flat a)    = { from: a, to: a }
exportAmount (a ~ b)     = { from: a, to: b }
exportAmount Full        = { from: 0.0, to: 0.0 }
exportAmount Placeholder = { from: 0.0, to: 0.0 }

exportActive ∷ Active -> _
exportActive {name, icon, cd, effect} =
    { name
    , icon: show icon
    , cd
    , effect: exportEffect <$> effect
    }

exportPassive ∷ Passive -> _
exportPassive {name, rank, icon, effect} =
    { name: name <> " " <> show rank
    , icon: show icon
    , effect: exportEffect <$> effect
    }

exportPhantasm ∷ NoblePhantasm -> _
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

exportEffect ∷ ActiveEffect -> _
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
exportEffect (Bonus bonus amount) =
    { target: show Self
    , duration: 0
    , effect: show bonus
    , amount: exportAmount amount
    , chance: {from: 100, to: 100}
    }
exportEffect (Chance chance activeEffect) =
    (exportEffect activeEffect) { chance = {from: chance, to: chance} }
exportEffect (Chances a b activeEffect) =
    (exportEffect activeEffect) { chance = {from: a, to: b} }
exportEffect (ToMax a activeEffect) = 
    baseEffect { effect = baseEffect.effect <> " every turn up to " <> show a }
  where baseEffect = exportEffect activeEffect
exportEffect (When condition activeEffect) =
    baseEffect { effect = "If " <> condition <> ": " <> baseEffect.effect }
  where baseEffect = exportEffect activeEffect
exportEffect (Times 1 activeEffect) =
    baseEffect { effect = baseEffect.effect <> " (1 time)" }
  where baseEffect = exportEffect activeEffect
exportEffect (Times times activeEffect) =
    baseEffect { effect = baseEffect.effect <> " (" <> show times <> " times)" }
  where baseEffect = exportEffect activeEffect
