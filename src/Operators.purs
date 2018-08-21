module Operators where

import Prelude

import Data.Array    (difference, elem, notElem, intersect, union)
import Data.Enum     (class BoundedEnum, enumFromTo)
import Data.Foldable (class Foldable)
import Data.Functor  (mapFlipped)
import Data.Map      (Map, fromFoldable)
import Data.Maybe    (Maybe(..))
import Data.Ord      (greaterThanOrEq, lessThanOrEq)
import Data.Tuple    (Tuple(..))

infix  0 Tuple           as :
infix  0 justIf          as ??
infixl 1 bind            as ≫=
infixl 1 advance         as ≫
infixr 2 disj            as ∨
infixr 3 conj            as ∧
infixl 4 mapFlipped      as ↦
infixl 4 map             as ↤
infix  4 eq              as ≡
infix  4 notEq           as ≠
infix  4 elem            as ∈
infix  4 notElem         as ∉
infix  4 lessThanOrEq    as ≤
infix  4 greaterThanOrEq as ≥
infix  5 difference      as ∖
infixr 5 append          as ⧺
infixr 6 intersect       as ∩
infixl 6 union           as ∪
infixr 7 mod             as %
infixr 7 unzeroMod       as %%
infixr 7 multimap        as ↤∘
infixr 9 compose         as ∘
infixr 9 doIf            as ?

m ∷ ∀ f k v. Ord k ⇒ Foldable f ⇒ f (Tuple k v) → Map k v
m = fromFoldable

unzeroMod ∷ ∀ a. EuclideanRing a ⇒ Eq a ⇒ a → a → a
unzeroMod a b = if modded ≡ zero then b else modded
  where 
    modded = a % b

doIf ∷ ∀ a. Boolean → (a → a) → (a → a)
doIf true  = identity
doIf false = const identity

multimap :: ∀ a f b c. Functor f ⇒ (c → b) → (a → f c) → a → f b
multimap f = (map f ∘ _)

justIf ∷ ∀ a. Boolean → a → Maybe a
justIf true = Just
justIf false = const Nothing

advance ∷ ∀ a b c. Bind a ⇒ Applicative a ⇒ a c → b → a b
advance a b = a ≫= \_ → pure b

enumArray ∷ ∀a. BoundedEnum a ⇒ Array a
enumArray = enumFromTo bottom top
