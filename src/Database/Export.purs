-- | The functions in this module can be invoked from JavaScript to export
-- | database information as simple objects.
-- | Since these functions are never called by CHALDEAS code, build commands
-- | must manually specify this module's inclusion with `--module Export`.
-- | The `npm run build` command does so automatically.
-- Note: `\_ ->` ensures that exported values are created locally and never
-- accessed by PureScript in order to preserve the guarantee of immutability.
module Export (craftEssences, servants) where

import StandardLibrary
import Data.String.CodeUnits as CodeUnits
import Data.Nullable         as Nullable

import Foreign (Foreign, unsafeToForeign)

import Database as DB

export :: ∀ a. a -> Foreign
export = unsafeToForeign

exportAll :: ∀ a. Array a -> (Unit -> Array Foreign)
exportAll xs = \_ -> export <$> xs

servants :: Unit -> Array Foreign
servants = exportAll $ DB.servants <#> \s'@(DB.Servant s) ->
    { name:          s.name
    , rarity:        s.rarity
    , class:         show s.class
    , attribute:     show s.attr
    , deck:          CodeUnits.toCharArray $ show s.deck
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
    , bond:          Nullable.toNullable $ DB.getBond s'
    }

craftEssences :: Unit -> Array Foreign
craftEssences = exportAll $ DB.craftEssences <#> \(DB.CraftEssence ce) ->
    { name:    ce.name
    , id:      ce.id
    , rarity:  ce.rarity
    , icon:    show ce.icon
    , stats:   ce.stats
    , effect:  exportEffect <$> ce.effect
    , limited: ce.limited
    , bond:    Nullable.toNullable ce.bond
    }

exportAmount :: DB.Amount -> Foreign
exportAmount (DB.Flat x)    = export { from: x, to: x }
exportAmount (DB.Range x y) = export { from: x, to: y }
exportAmount DB.Full        = export { from: 0.0, to: 0.0 }
exportAmount DB.Placeholder = export { from: 0.0, to: 0.0 }

exportSkill :: DB.Skill -> Foreign
exportSkill {name, icon, cd, effect} =
    export { name
           , icon: show icon
           , cd
           , effect: exportEffect <$> effect
           }

exportPhantasm :: DB.NoblePhantasm -> Foreign
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

exportEffect :: DB.SkillEffect -> Foreign
exportEffect = export <<< go
  where
    go (DB.Grant target duration effect amount) =
        { target: show target
        , duration
        , effect: show effect
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    go (DB.Debuff target duration effect amount) =
        { target: show target
        , duration
        , effect: show effect
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    go (DB.To target effect amount) =
        { target: show target
        , duration: (-1)
        , effect: show effect
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    go (DB.Bonus bonus amount) =
        { target: show DB.Self
        , duration: 0
        , effect: show bonus
        , amount: exportAmount amount
        , chance: {from: 100, to: 100}
        }
    go (DB.Chance chance effect) = (go effect)
        { chance = {from: chance, to: chance} }
    go (DB.Chances x y effect) = (go effect)
        { chance = {from: x, to: y} }
    go (DB.ToMax x effect) = modEffect (go effect) <<<
        flip append $ " every turn up to " <> show x
    go (DB.When condition effect) = modEffect (go effect) <<<
        append $ "If " <> condition <> ": "
    go (DB.Times 1 effect) = modEffect (go effect) $
        flip append " (1 time)"
    go (DB.Times times effect) = modEffect (go effect) <<<
        flip append $ " (" <> show times <> " times)"

modEffect :: ∀ a. { effect :: String | a } -> (String -> String)
          -> { effect :: String | a }
modEffect base f = base { effect = f base.effect }
