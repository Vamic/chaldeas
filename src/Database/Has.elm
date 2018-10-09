module Database.Has exposing (..)

import Maybe.Extra as Maybe

import StandardLibrary       exposing (..)
import Database.Base         exposing (..)
import Database.Skill        exposing (..)
import Database.Servant      exposing (..)
import Database.CraftEssence exposing (..)

type alias Has a b = 
    { show : b -> String
    , has  : Bool -> a -> List b
    }

hasMaterial : Has Servant Material
hasMaterial = 
    Has showMaterial << always <|
    getMaterials >> List.filter (not << ignoreMat)

hasTrait : Has Servant Trait
hasTrait = 
    Has showTrait << always <| 
    .traits

hasAlignment : Has Servant Alignment
hasAlignment = 
    Has showAlignment << always <| 
    .align

hasPhantasmType : Has Servant PhantasmType
hasPhantasmType = 
    Has showPhantasmType << always <|
    .phantasm >> phantasmType >> List.singleton

hasClass : Has Servant Class
hasClass = 
    Has showClass << always <| 
    .class >> List.singleton

hasAttribute : Has Servant Attribute
hasAttribute = 
    Has showAttribute << always <| 
    .attr >> List.singleton

hasDeck : Has Servant Deck
hasDeck =
    Has showDeck << always <| 
    .deck >> List.singleton

hasCard : Has Servant Card
hasCard = 
    Has showCard << always <|
    .phantasm >> .card >> List.singleton

hasPassive : Has Servant Skill
hasPassive =
    Has .name << always <|
    .passives

sEffects : Servant -> List SkillEffect
sEffects s = 
    s.phantasm.effect 
    ++ s.phantasm.over 
    ++ List.concatMap .effect s.skills

hasEffect : (b -> String) -> (Bool -> SkillEffect -> Maybe b)
         -> (a -> List SkillEffect) 
         -> Has a b
hasEffect show match f = Has show <| \noSelf x ->
    f x
    |> List.map simplify
    >> List.filter (not << demerit)
    >> List.map (match noSelf)
    >> Maybe.values

hasBuffEffect : (a -> List SkillEffect) -> Has a BuffEffect
hasBuffEffect = 
    hasEffect (showBuffEffect Someone Placeholder) <| 
    \noSelf a -> case a of
      Grant t _ y _ -> if not noSelf || t /= Self then Just y else Nothing
      _             -> Nothing

hasDebuffEffect : (a -> List SkillEffect) -> Has a DebuffEffect
hasDebuffEffect = 
    hasEffect (showDebuffEffect Someone Placeholder) <| 
    \_ a -> case a of
      Debuff _ _ y _ -> Just y
      _              -> Nothing

hasInstantEffect : (a -> List SkillEffect) -> Has a InstantEffect
hasInstantEffect = 
    hasEffect (showInstantEffect Someone Placeholder) <| 
    \noSelf a -> case a of
      To t y _ -> if not noSelf || t /= Self then Just y else Nothing
      _        -> Nothing

hasBonusEffect : (a -> List SkillEffect) -> Has a BonusEffect
hasBonusEffect = 
    hasEffect (showBonusEffect False Placeholder) <| 
    \_ a -> case a of
      Bonus y _ _ -> Just y
      _           -> Nothing
