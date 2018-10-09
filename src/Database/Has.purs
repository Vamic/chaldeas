module Database.Has (class HasEffects, getEffects, class Has, has) where

import StandardLibrary
import Generic as G

import Database.Model

phantasmEffects :: Target -> NoblePhantasm -> Boolean
phantasmEffects target {effect, over} = any match $ effect <> over
  where
    match (To x Damage _)        = x == target
    match (To x DamageThruDef _) = x == target
    match _                      = false

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
    has SingleTarget _ (Servant s) = phantasmEffects Enemy s.phantasm
    has MultiTarget  _ (Servant s) = phantasmEffects Enemies s.phantasm
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
        match (Debuff _ _ y _) = x == y
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
