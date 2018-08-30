module Sorting where

import Prelude
import Operators

import Data.Map as M

import Control.MonadZero
import Data.Array
import Data.Enum
import Data.Foldable
import Data.Formatter.Number
import Data.Generic.Rep
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum
import Data.Generic.Rep.Show
import Data.Int
import Data.Maybe
import Data.Map (Map)
import Data.Ord
import Data.Profunctor.Strong
import Data.Tuple

import Database

data SortBy = ID
            | Rarity
            | ATK
            | HP
            | StarRate
            | Hits
            | NPGain
            | NPArts
            | GrailATK
            | GrailHP
            | NPDamage
            | SpecialNP

instance _a_ ∷ Show SortBy where
  show StarRate   = "Star Rate"
  show NPDamage   = "NP Damage"
  show SpecialNP  = "NP Special Damage"
  show NPGain     = "NP Gain/Hit"
  show NPArts     = "NP Gain/Arts card"
  show GrailATK   = "Grail ATK"
  show GrailHP    = "Grail HP"
  show a = genericShow a

print ∷ Int -> Number -> String
print places = format $ Formatter { comma: true
                                  , before: 0
                                  , after: places
                                  , abbreviations: false
                                  , sign: false
                                  }
print' ∷ Int -> String
print' = print 0 ∘ toNumber

toSort ∷ SortBy -> Servant -> Number
toSort ID {id} = -1.0 * toNumber id
toSort Rarity {rarity} = toNumber rarity
toSort NPDamage s = npDamage false s 
toSort SpecialNP s = npDamage true s
toSort NPGain {gen:{npAtk}} = npAtk
toSort ATK {stats:{max:{atk}}} = toNumber atk
toSort HP {stats:{max:{hp}}} = toNumber hp
toSort GrailATK {stats:{grail:{atk}}} = toNumber atk
toSort GrailHP {stats:{grail:{hp}}} = toNumber hp
toSort StarRate {gen:{starRate}} = starRate
toSort NPArts {gen:{npAtk}, hits:{arts}} = 2.0 * npAtk * toNumber arts
toSort Hits {hits:{arts,buster,quick,ex}} 
    = toNumber $ arts + buster + quick + ex

doSort ∷ SortBy -> Array Servant -> Array (Tuple String Servant)
doSort Rarity = map (Tuple "") ∘ sortWith \s -> show (5 - s.rarity) ++ s.name
doSort a = map showSort ∘ sortWith sorter
  where
    sorter   = toSort a
    showSort 
      | a == SpecialNP || a == NPDamage = \s -> (flip Tuple) s 
          $ (if s.free || s.rarity < 4 then "NP5: " else "NP1: ") 
         ++ (output ∘ abs $ sorter s)
      | otherwise = uncurry Tuple ∘ (output ∘ abs ∘ sorter &&& identity)
    output = case a of
        NPGain   -> (_ ++ "%") ∘ print 2
        StarRate -> (_ ++ "%") ∘ print 2
        NPArts   -> (_ ++ "%") ∘ print 2
        _        -> print 0

sorted ∷ Map SortBy (Array (Tuple String Servant))
sorted = M.fromFoldable $ go <$> enumArray
  where
    go sorter = Tuple sorter $ doSort sorter servants

getSort ∷ SortBy -> Array (Tuple String Servant)
getSort sorter = fromMaybe [] $ M.lookup sorter sorted

classAtk ∷ Class -> Number
classAtk Berserker = 1.1
classAtk Ruler = 1.1
classAtk Avenger = 1.1
classAtk Lancer = 1.05
classAtk Archer = 0.95
classAtk Caster = 0.90
classAtk Assassin = 0.90
classAtk _ = 1.0

selfable ∷ Target -> Boolean
selfable Self   = true
selfable Ally   = true
selfable Allies = true
selfable Party  = true
selfable _      = false

matchSum ∷ ∀ a b. Eq a => Semiring b => 
           Array (Tuple a b) -> a -> b
matchSum xs k = sum $ go <$> xs
  where
    go (Tuple k1 v) | k == k1 = v
    go _ = zero
         
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
    -- FROM YOUR SERVANT
    servantAtk = toNumber s.stats.max.atk
    classAtkBonus = classAtk s.class
    triangleModifier = add 1.0 ∘ sum 
                     $ matchSum buffs ∘ DamageAffinity <$> specials
    attributeModifier = 1.0
    -- FROM CARDS
    firstCardBonus = 0.0
    cardDamageValue = case card of
        Arts   -> 1.0
        Buster -> 1.5
        Quick  -> 0.8
    busterChainMod = 0.0
    extraCardModifier = 1.0
    -- FROM RNG
    randomModifier = 1.0
    criticalModifier = 1.0
    -- FROM NP PROPERTIES
    npDamageMultiplier = sum $ matchSum instants 
                     <$> (special ? cons LastStand $ [Damage, DamageThruDef])
    superEffectiveModifier = add 1.0 ∘ add (matchSum instants DamagePoison)
                           ∘ fromMaybe 0.0 ∘ maximum 
                           $ matchSum instants ∘ DamageVs <$> specials
    isSuperEffective = 1.0
    -- FROM BUFFS
    cardMod = matchSum buffs $ case card of
        Arts   -> ArtsUp
        Buster -> BusterUp
        Quick  -> QuickUp
    atkMod = add (matchSum buffs AttackUp) ∘ fromMaybe 0.0 ∘ maximum 
           $ matchSum buffs ∘ AttackUpVs <$> specials
    defMod = matchSum debuffs DefenseDown
    specialDefMod = 0.0
    powerMod = 0.0 -- TODO
    selfDamageMod = 0.0
    isCrit = 0.0
    critDamageMod = 0.0
    isNP = 1.0
    npDamageMod = matchSum buffs NPUp
    dmgPlusAdd = 100.0 * (matchSum buffs DamageUp + matchSum debuffs DamageVuln)
    selfDmgCutAdd = 0.0
    
-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _7_ ∷ Generic SortBy _
derive instance _8_ ∷ Eq SortBy
derive instance _9_ ∷ Ord SortBy
instance _11_ ∷ Enum SortBy where
  succ = genericSucc
  pred = genericPred
instance _12_ ∷ Bounded SortBy where
  top = genericTop
  bottom = genericBottom
instance _13_ ∷ BoundedEnum SortBy where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
