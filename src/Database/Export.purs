-- | The functions in this module can be invoked from JavaScript to export 
-- | database information as simple objects. 
-- | Since these functions are never called by CHALDEAS code, build commands 
-- | must manually specify this module's inclusion with `--module Export`.
-- | The `npm run build` command does so automatically.
-- Note: `\_ ->` ensures that exported values are created locally and never
-- accessed by PureScript in order to preserve the guarantee of immutability.
module Export (craftEssences, servants) where

import Prelude
import Data.Nullable
import Data.String.CodeUnits
import Foreign

import Database as D
import Database

export :: ∀ a. a -> Foreign
export = unsafeToForeign

exportAll :: ∀ a. Array a -> (Unit -> Array Foreign)
exportAll xs = \_ -> export <$> xs

servants :: Unit -> Array Foreign
servants = exportAll $ D.servants <#> \s'@(Servant s) ->
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

craftEssences :: Unit -> Array Foreign
craftEssences = exportAll $ D.craftEssences <#> \(CraftEssence ce) ->
    { name:    ce.name
    , id:      ce.id
    , rarity:  ce.rarity
    , icon:    show ce.icon
    , stats:   ce.stats
    , effect:  exportEffect <$> ce.effect
    , limited: ce.limited
    , bond:    toNullable ce.bond
    }

exportAmount :: Amount -> Foreign
exportAmount (Flat x)    = export { from: x, to: x }
exportAmount (x ~ y)     = export { from: x, to: y }
exportAmount Full        = export { from: 0.0, to: 0.0 }
exportAmount Placeholder = export { from: 0.0, to: 0.0 }

exportSkill :: Skill -> Foreign
exportSkill {name, icon, cd, effect} = 
    export { name
           , icon: show icon
           , cd
           , effect: exportEffect <$> effect
           }

exportPhantasm :: NoblePhantasm -> Foreign
exportPhantasm {name, desc, rank, card, kind, hits, effect, over} = 
    export { name
           , desc
           , rank: show rank
           , card: show card
           , classification: kind
           , hits
           , effect: exportEffect <$> effect
           , over: exportEffect <$> over
           }

exportEffect :: SkillEffect -> Foreign
exportEffect = export <<< go
  where
    go (Grant target duration effect amount) =
        { target: show target
        , duration
        , effect: show effect
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    go (Debuff target duration effect amount) =
        { target: show target
        , duration
        , effect: show effect
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    go (To target effect amount) =
        { target: show target
        , duration: (-1)
        , effect: show effect
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    go (Bonus bonus amount) =
        { target: show Self
        , duration: 0
        , effect: show bonus
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    go (Chance chance effect) = (go effect) 
        { chance = {from: chance, to: chance} }
    go (Chances x y effect) = (go effect) 
        { chance = {from: x, to: y} }
    go (ToMax x effect) = modEffect (go effect) <<<
        flip append $ " every turn up to " <> show x
    go (When condition effect) = modEffect (go effect) <<<
        append $ "If " <> condition <> ": "
    go (Times 1 effect) = modEffect (go effect) $
        flip append " (1 time)"
    go (Times times effect) = modEffect (go effect) <<<
        flip append $ " (" <> show times <> " times)"

modEffect :: ∀ a. { effect :: String | a } -> (String -> String) 
          -> { effect :: String | a }
modEffect base f = base { effect = f base.effect }
