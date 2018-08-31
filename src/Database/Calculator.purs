module Database.Calculator (npPerArts, starsPerQuick, npDamage) where

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
    
npPerArts ∷ Servant -> Number
npPerArts s 
    = toNumber s.hits.arts
    * offensiveNPRate 
    * (firstCardBonus + (cardNpValue * (1.0 + cardMod))) 
    * enemyServerMod 
    * (1.0 + npChargeRateMod) 
    * criticalModifier 
    * overkillModifier
  where
    offensiveNPRate  = s.gen.npAtk
    firstCardBonus   = 0.0
    cardNpValue      = 3.0
    cardMod          = matchSum buffs ArtsUp
    enemyServerMod   = 1.0
    npChargeRateMod  = matchSum buffs NPGen
    criticalModifier = 1.0
    overkillModifier = 1.0
    buffs            = passiveBuffs s

starsPerQuick ∷ Servant -> Number
starsPerQuick s
    = toNumber s.hits.quick
    * min 3.0
        ( baseStarRate + firstCardBonus + (cardStarValue * (1.0 + cardMod)) 
        + serverRate + starDropMod - enemyStarDropMod + criticalModifier
        ) 
    * overkillModifier 
    + overkillAdd
  where
    baseStarRate     = s.gen.starRate / 100.0
    firstCardBonus   = 0.0
    cardStarValue    = 0.8
    cardMod          = matchSum buffs QuickUp
    serverRate       = 0.0
    starDropMod      = matchSum buffs StarUp
    enemyStarDropMod = 0.0
    criticalModifier = 0.0
    overkillModifier = 1.0
    overkillAdd      = 0.0
    buffs            = passiveBuffs s

npDamage ∷ Boolean -> Servant -> Number
npDamage special s@{phantasm:{card, effect, over, first}}
    = if npDamageMultiplier == 0.0 then 0.0 else 
      servantAtk 
    * npDamageMultiplier 
    * (firstCardBonus + (cardDamageValue * (1.0 + cardMod))) 
    * classAtkBonus 
    * triangleModifier 
    * attributeModifier 
    * randomModifier 
    * 0.23 
    * (1.0 + atkMod - defMod) 
    * criticalModifier 
    * extraCardModifier 
    * (1.0 - specialDefMod) 
    * (1.0 + powerMod + selfDamageMod 
      + (critDamageMod * isCrit)
      + (npDamageMod * isNP)
      ) 
    * (1.0 + ((superEffectiveModifier - 1.0) * isSuperEffective)) 
    + dmgPlusAdd + selfDmgCutAdd + (servantAtk * busterChainMod)
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
    triangleModifier = add 1.0 ∘ sum 
                     $ matchSum buffs ∘ DamageAffinity <$> specials
    attributeModifier = 1.0
    -------------
    -- FROM CARDS
    -------------
    firstCardBonus = 0.0
    cardDamageValue = case card of
        Arts   -> 1.0
        Buster -> 1.5
        Quick  -> 0.8
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
    npDamageMultiplier = sum $ matchSum instants 
                     <$> (special ? cons LastStand $ [Damage, DamageThruDef])
    superEffectiveModifier = add 1.0 ∘ add (matchSum instants DamagePoison)
                           ∘ fromMaybe 0.0 ∘ maximum 
                           $ matchSum instants ∘ DamageVs <$> specials
    isSuperEffective = 1.0
    -------------
    -- FROM BUFFS
    -------------
    cardMod = matchSum buffs $ case card of
        Arts   -> ArtsUp
        Buster -> BusterUp
        Quick  -> QuickUp
    atkMod = add (matchSum buffs AttackUp) ∘ fromMaybe 0.0 ∘ maximum 
           $ matchSum buffs ∘ AttackUpVs <$> specials
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
    specials ∷ ∀ a. BoundedEnum a => Array a
    specials
      | special   = enumArray
      | otherwise = []
    npStrength
      | s.free || s.rarity < 4 = toMax
      | otherwise              = toMin
    skillFs = simplify <$> (s.actives >>= _.effect) 
                         ++ (s.passives >>= _.effect)
    npFs    = simplify <$> effect
    overFs  = simplify <$> over
    firstFs = fromFoldable $ guard first >> head overFs
    buffs = ((skillFs ++ firstFs) >>= go) ++ (npFs >>= goNP)
      where
        go (Grant t _ buff a) 
          | selfable t = [ Tuple buff $ toMax a / 100.0 ]
        go _ = []
        goNP (Grant t _ buff a)
          | selfable t = [ Tuple buff $ npStrength a / 100.0 ]
        goNP _ = []
    debuffs = ((skillFs ++ firstFs) >>= go) ++ (npFs >>= goNP)
      where
        go (Debuff t _ debuff a) 
          | not $ allied t = [ Tuple debuff $ toMax a / 100.0 ]
        go _ = []
        goNP (Debuff t _ debuff a)
          | not $ allied t = [ Tuple debuff $ npStrength a / 100.0 ]
        goNP _ = []
    instants = ((skillFs ++ overFs) >>= go) ++ (npFs >>= goNP)
      where
        go (To t instant a) 
          | not $ allied t = [ Tuple instant $ toMax a / 100.0 ]
        go _ = []
        goNP (To t instant a)
          | not $ allied t = [ Tuple instant $ npStrength a / 100.0 ]
        goNP _ = []

passiveBuffs ∷ Servant -> Array (Tuple BuffEffect Number)
passiveBuffs {passives} = passives >>= _.effect >>= go ∘ simplify
  where 
    go (Grant t _ buff a) 
      | selfable t = [ Tuple buff $ toMax a / 100.0 ]
    go _ = []
    
selfable ∷ Target -> Boolean
selfable Self   = true
selfable Ally   = true
selfable Allies = true
selfable Party  = true
selfable _      = false

matchSum ∷ ∀ a b. Eq a => Semiring b => Array (Tuple a b) -> a -> b
matchSum xs k = sum $ go <$> xs
  where
    go (Tuple k1 v) | k == k1 = v
    go _ = zero
