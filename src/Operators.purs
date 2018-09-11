module Operators 
  ( enumArray
  , unCamel
  , ymd
  , (:)
  , (?), doIf
  , maybeDo
  ) where

import Prelude

import Data.Enum (class BoundedEnum, enumFromTo, toEnum)
import Data.Date (Date, exactDate)
import Data.Date.Component(Month)
import Data.Function.Memoize (memoize)
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)

infix  0 Tuple   as :
infixr 9 doIf    as ?

doIf :: ∀ a. Boolean -> (a -> a) -> (a -> a)
doIf true  = identity
doIf false = const identity

maybeDo :: ∀ a. (a -> Maybe a) -> a -> a
maybeDo f x = fromMaybe x $ f x

enumArray :: ∀ a. BoundedEnum a => Array a
enumArray = flip memoize unit \_ -> enumFromTo bottom top

ymd :: Int -> Month -> Int -> Date
ymd y m d = unsafePartial $ fromJust do
    y' <- toEnum y
    d' <- toEnum d
    exactDate y' m d'

camel :: Regex
camel = unsafeRegex "([a-z])([A-Z])" mempty

unCamel :: String -> String
unCamel = replace camel "$1 $2"
