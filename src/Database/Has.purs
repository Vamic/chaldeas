module Database.Has (class HasEffects, getEffects, class Has, has) where

import StandardLibrary
import Generic as G

import Database.Model

phantasmEffects :: NoblePhantasm -> Array SkillEffect
phantasmEffects {effect, over} = effect <> over

class Show a <= HasEffects a where
    getEffects :: a -> Array SkillEffect

instance _a_ :: HasEffects Servant where
    getEffects (Servant s) = filter (not <<< demerit) $ simplify
                         <$> s.phantasm.effect
                          <> s.phantasm.over
                          <> (s.skills >>= _.effect)
instance _b_ :: HasEffects CraftEssence where
    getEffects (CraftEssence ce) = filter (not <<< demerit) $
                                   simplify <$> ce.effect


class (Show a, G.BoundedEnum b, Show b) <= Has a b where
    has :: b -> Boolean -> a -> Boolean

instance _c_ :: Has Servant Material where
    has x
      | ignoreMat x = const $ const false
      | otherwise   = const $ elem x <<< getMaterials
instance _d_ :: Has Servant Trait where
    has x _ (Servant s) = x `elem` s.traits
instance _e_ :: Has Servant Alignment where
    has x _ (Servant s) = x `elem` s.align
instance _f_ :: Has Servant PhantasmType where
    has SingleTarget _ (Servant s) = any match $ phantasmEffects s.phantasm
      where
        match (To Enemy Damage _) = true
        match (To Enemy DamageThruDef _) = true
        match _ = false
    has MultiTarget _ (Servant s) = any match $ phantasmEffects s.phantasm
      where
        match (To Enemies Damage _) = true
        match (To Enemies DamageThruDef _) = true
        match _ = false
    has Support x s = not (has SingleTarget x s) && not (has MultiTarget x s)
instance _g_ :: Has Servant Class where
    has x _ (Servant s) = x == s.class
instance _h_ :: Has Servant Attribute where
    has x _ (Servant s) = x == s.attr
instance _i_ :: Has Servant Deck where
    has x _ (Servant s) = x == s.deck
instance _j_ :: Has Servant Card where
    has x _ (Servant s) = x == s.phantasm.card

instance _k_ :: HasEffects a => Has a BuffEffect where
    has x noSelf = any match <<< getEffects where
        match (Grant t _ y _) = x == y && (not noSelf || t /= Self)
        match _ = false
instance _l_ :: HasEffects a => Has a DebuffEffect where
    has x _ = any match <<< getEffects where
        match (Debuff t _ y _) = x == y
        match _ = false
instance _m_ :: HasEffects a => Has a InstantEffect where
    has BecomeHyde _ = const false
    has x noSelf     = any match <<< getEffects where
        match (To t y _) = x == y && (not noSelf || t /= Self)
        match _ = false
instance _n_ :: HasEffects a => Has a BonusEffect where
    has x _ = any match <<< getEffects where
        match (Bonus y _ _) = x == y
        match _ = false
