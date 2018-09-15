-- | Calculates information for sorting based on datamined formulas.
module Database.Calculator (npPer, starsPer, npDamage, matchSum) where

import Prelude
import Operators

import Control.MonadZero
import Data.Array
import Data.Enum
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Tuple

import Database.Model

-- | Formula source: [Beast's Lair: Mining for Bits, by Kyte](http://blogs.nrvnqsr.com/entry.php/3306-How-much-NP-do-I-get-in-combat)
npPer :: Servant -> Card -> Number
npPer s'@(Servant s) card = toNumber s.hits.arts
                          * offensiveNPRate
                          * ( firstCardBonus + (cardNpValue * (1.0 + cardMod)) )
                          * enemyServerMod
                          * ( 1.0 + npChargeRateMod )
                          * criticalModifier
                          * overkillModifier
  where
    offensiveNPRate  = s.gen.npAtk
    firstCardBonus   = 1.0
    cardNpValue      = 1.5 * case card of
                                 Arts   -> 3.0
                                 Quick  -> 1.0
                                 Buster -> 0.0
    cardMod          = matchSum buffs $ Performance card
    enemyServerMod   = 1.0
    npChargeRateMod  = matchSum buffs NPGen
    criticalModifier = 1.0
    overkillModifier = 1.0
    buffs            = passiveBuffs s'

-- | Formula source: [Beast's Lair: Mining for Bits, by Kyte](http://blogs.nrvnqsr.com/entry.php/3307-How-many-crit-stars-do-I-get-in-combat)
starsPer :: Servant -> Card -> Number
starsPer s'@(Servant s) card = toNumber s.hits.quick 
                             * min 3.0 ( baseStarRate 
                                       + firstCardBonus 
                                       + ( cardStarValue * (1.0 + cardMod) )
                                       + serverRate 
                                       + starDropMod 
                                       - enemyStarDropMod 
                                       + criticalModifier
                                       )
                             * overkillModifier
                             + overkillAdd
  where
    baseStarRate     = s.gen.starRate / 100.0
    firstCardBonus   = 0.2
    cardStarValue    = case card of
                           Quick  -> 1.3
                           Arts   -> 0.15
                           Buster -> 0.0
    cardMod          = matchSum buffs $ Performance card
    serverRate       = 0.0
    starDropMod      = matchSum buffs StarUp
    enemyStarDropMod = 0.0
    criticalModifier = 0.0
    overkillModifier = 1.0
    overkillAdd      = 0.0
    buffs            = passiveBuffs s'

-- | Formula source: [Beast's Lair: Mining for Bits, by Kyte](http://blogs.nrvnqsr.com/entry.php/3309-How-is-damage-calculated)
npDamage :: Boolean -> Boolean -> Servant -> Number
npDamage special maxOver (Servant s@{phantasm:{card, effect, over, first}}) = 
    case npDamageMultiplier of
        0.0 -> 0.0
        _   -> servantAtk
             * npDamageMultiplier
             * ( firstCardBonus + (cardDamageValue * (1.0 + cardMod)) )
             * classAtkBonus
             * triangleModifier
             * attributeModifier
             * randomModifier
             * 0.23
             * ( 1.0 + atkMod - defMod )
             * criticalModifier
             * extraCardModifier
             * ( 1.0 - specialDefMod )
             * ( 1.0 
               + powerMod 
               + selfDamageMod 
               + (critDamageMod * isCrit) 
               + (npDamageMod * isNP)
               )
             * ( 1.0 + ((superEffectiveModifier - 1.0) * isSuperEffective) )
             + dmgPlusAdd 
             + selfDmgCutAdd 
             + ( servantAtk * busterChainMod )
  where
    --------------------
    -- FROM YOUR SERVANT
    --------------------
    servantAtk = toNumber s.stats.max.atk
    classAtkBonus = case s.class of
                        Berserker -> 1.1
                        Ruler     -> 1.1
                        Avenger   -> 1.1
                        Lancer    -> 1.05
                        Archer    -> 0.95
                        Caster    -> 0.90
                        Assassin  -> 0.90
                        _         -> 1.0
    triangleModifier = (_ + 1.0) <<< sum $ 
                       matchSum buffs <<< ClassAffinity <$> specials
    attributeModifier = 1.0
    -------------
    -- FROM CARDS
    -------------
    firstCardBonus = 0.0
    cardDamageValue = case card of
                          Quick  -> 0.8
                          Arts   -> 1.0
                          Buster -> 1.5
    busterChainMod = 0.0
    extraCardModifier = 1.0
    -----------
    -- FROM RNG
    -----------
    randomModifier = 1.0
    criticalModifier = 1.0
    ---------------------
    -- FROM NP PROPERTIES
    ---------------------
    npDamageMultiplier = sum $ map (matchSum instants) <<<
                         special ? cons LastStand $ [Damage, DamageThruDef]
    superEffectiveModifier = (_ + 1.0 + matchSum instants DamagePoison) <<<
                             fromMaybe 0.0 <<< maximum $
                             matchSum instants <<< DamageVs <$> specials
    isSuperEffective = 1.0
    -------------
    -- FROM BUFFS
    -------------
    cardMod = matchSum buffs $ Performance card
    atkMod = (_ + matchSum buffs AttackUp) <<< fromMaybe 0.0 <<< maximum $
             matchSum buffs <<< AttackVs <$> specials
    defMod = matchSum debuffs DefenseDown
    specialDefMod = 0.0
    powerMod = 0.0
    selfDamageMod = 0.0
    isCrit = 0.0
    critDamageMod = 0.0
    isNP = 1.0
    npDamageMod = matchSum buffs NPUp
    dmgPlusAdd = 100.0 * (matchSum buffs DamageUp + matchSum debuffs DamageVuln)
    selfDmgCutAdd = 0.0
    -- INTERNAL
    specials :: ∀ a. BoundedEnum a => Array a
    specials
      | special   = enumArray
      | otherwise = []
    npStrength
      | s.free || s.rarity < 4 = toMax
      | otherwise              = toMin
    overStrength
      | maxOver   = toMax
      | otherwise = toMin
    skillFs = simplify <$> (s.skills >>= _.effect)
                        <> (s.passives >>= _.effect)
    npFs    = simplify <$> effect
    overFs  = simplify <$> over
    firstFs = fromFoldable $ guard first *> head overFs
    buffs = (skillFs >>= go toMax)
         <> (npFs >>= go npStrength)
         <> (firstFs >>= go overStrength)
      where
        go f (Grant t _ buff n)
          | selfable t = [(buff ^ f n / 100.0)]
        go _ _ = []
    debuffs = (skillFs >>= go toMax)
           <> (npFs >>= go npStrength)
           <> (firstFs >>= go overStrength)
      where
        go f (Debuff t _ debuff n)
          | not $ allied t = [(debuff ^ f n / 100.0)]
        go _ _ = []
    instants = (skillFs >>= go toMax)
            <> (npFs    >>= go npStrength)
            <> (overFs  >>= go overStrength)
      where
        go f (To t instant n)
          | not $ allied t = [(instant ^ f n / 100.0)]
        go _ _ = []

-- | Obtains all self-granted always-active buff effects from passive skills.
-- | Returns an array of (Buff, Strength%) pairs.
passiveBuffs :: Servant -> Array (Tuple BuffEffect Number)
passiveBuffs (Servant {passives}) = passives >>= _.effect >>= go <<< simplify
  where
    go (Grant t _ buff n)
      | selfable t = [(buff ^ toMax n / 100.0)]
    go _ = []

-- | Attacker vs. Defender. Currently not in use. 
-- TODO figure out how to use this
attributeBonus :: Attribute -> Attribute -> Number
attributeBonus Mankind Heaven  = 1.1
attributeBonus Heaven  Earth   = 1.1
attributeBonus Earth   Mankind = 1.1
attributeBonus Mankind Earth   = 0.9
attributeBonus Earth   Heaven  = 0.9
attributeBonus Heaven  Mankind = 0.9
{-
attributeBonus Star    Beast   = 1.1
attributeBonus Beast   Star    = 1.1
attributeBonus Beast   Beast   = 0.0
-}
attributeBonus _       _       = 1.0

-- | If a skill's target is not `Self`, `Ally`, or `Party`, 
-- | it cannot be self-applied and therefore should not be used in calculations.
selfable :: Target -> Boolean
selfable Self   = true
selfable Ally   = true
selfable Party  = true
selfable _      = false

-- | Sums up all effects of a certain type from a Servant's skills 
-- | in (Effect, Strength%) format.
matchSum :: ∀ a b f. Foldable f => Functor f => Eq a => Semiring b 
         => f (Tuple a b) -> a -> b
matchSum xs k = sum $ go <$> xs
  where
    go (k1 ^ v) | k == k1 = v
    go _ = zero
