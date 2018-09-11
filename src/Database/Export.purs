module Export (craftEssences, servants) where

import Prelude
import Data.Nullable
import Data.String.CodeUnits

import Database as D
import Database

servants :: Unit -> Array _
servants = \_ -> D.servants <#> \s'@(Servant s) ->
    { name:          s.name
    , rarity:        s.rarity
    , class:         show s.class
    , attribute:     show s.attr
    , deck:          toCharArray $ show s.deck
    , stats:         s.stats
    , activeSkills:  exportSkill <$> s.skills
    , passiveSkills: exportSkill <$> s.passives
    , noblePhantasm: exportPhantasm s.phantasm
    , starWeight:    s.gen.starWeight
    , starRate:      s.gen.starRate
    , npAtk:         s.gen.npAtk
    , npDef:         s.gen.npDef
    , hits:          s.hits
    , traits:        show <$> s.traits
    , deathRate:     s.death
    , alignment:     show <$> s.align
    , limited:       s.limited
    , free:          s.free
    , bond:          toNullable $ getBond s'
    }

craftEssences :: Unit -> Array _
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

exportAmount :: Amount -> _
exportAmount (Flat x)    = { from: x, to: x }
exportAmount (x ~ y)     = { from: x, to: y }
exportAmount Full        = { from: 0.0, to: 0.0 }
exportAmount Placeholder = { from: 0.0, to: 0.0 }

exportSkill :: Skill -> _
exportSkill {name, icon, cd, effect} =
    { name
    , icon: show icon
    , cd
    , effect: exportEffect <$> effect
    }

exportPhantasm :: NoblePhantasm -> _
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

exportEffect :: SkillEffect -> _
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
exportEffect (Chance chance effect) = (exportEffect effect) 
    { chance = {from: chance, to: chance} }
exportEffect (Chances x y effect)   = (exportEffect effect) 
    { chance = {from: x, to: y} }
exportEffect (ToMax x effect) = modEffect (exportEffect effect) <<<
    flip append $ " every turn up to " <> show x
exportEffect (When condition effect) = modEffect (exportEffect effect) <<<
    append $ "If " <> condition <> ": "
exportEffect (Times 1 effect) = modEffect (exportEffect effect) $
    flip append " (1 time)"
exportEffect (Times times effect) = modEffect (exportEffect effect) <<<
    flip append $ " (" <> show times <> " times)"

modEffect :: ∀ a. { effect :: String | a } -> (String -> String) 
          -> { effect :: String | a }
modEffect base f = base { effect = f base.effect }
