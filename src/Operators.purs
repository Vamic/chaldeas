module Operators where

import Prelude

import Data.Enum     (class BoundedEnum, enumFromTo)
import Data.Foldable (class Foldable)
import Data.Map      (Map, fromFoldable)
import Data.Tuple    (Tuple(..))

infix  0 Tuple           as :
infixl 1 advance         as >>
infixr 5 append          as ++
infixr 9 compose         as ∘
infixr 9 doIf            as ?

m ∷ ∀ f k v. Ord k => Foldable f => f (Tuple k v) -> Map k v
m = fromFoldable

doIf ∷ ∀ a. Boolean -> (a -> a) -> (a -> a)
doIf true  = identity
doIf false = const identity

advance :: ∀ a b c. Bind a ⇒ a c → a b → a b
advance a b = a >>= \_ -> b

enumArray ∷ ∀ a. BoundedEnum a => Array a
enumArray = enumFromTo bottom top
