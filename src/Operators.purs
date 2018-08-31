module Operators where

import Prelude

import Data.Enum       (class BoundedEnum, enumFromTo)
import Data.Tuple      (Tuple(..))

infix  0 Tuple           as :
infixr 9 compose         as ∘
infixr 9 doIf            as ?

doIf ∷ ∀ a. Boolean -> (a -> a) -> (a -> a)
doIf true  = identity
doIf false = const identity

enumArray ∷ ∀ a. BoundedEnum a => Array a
enumArray = enumFromTo bottom top
