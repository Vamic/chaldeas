module Operators where

import Prelude

import Data.Either (fromRight)
import Data.Enum (class BoundedEnum, enumFromTo)
import Data.Tuple (Tuple(..))
import Data.String.Regex (regex, replace)
import Partial.Unsafe (unsafePartial)

infix  0 Tuple           as :
infixr 9 doIf            as ?

doIf ∷ ∀ a. Boolean -> (a -> a) -> (a -> a)
doIf true  = identity
doIf false = const identity

enumArray ∷ ∀ a. BoundedEnum a => Array a
enumArray = enumFromTo bottom top

unCamel ∷ String -> String
unCamel = replace reg "$1 $2"
  where
    reg = unsafePartial $ fromRight $ regex "([a-z])([A-Z])" mempty
