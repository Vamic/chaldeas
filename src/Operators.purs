-- | Small helper functions.
-- As a rule of thumb, the idea is that anything exported by this module would
-- not look out-of-place in Prelude.
module Operators 
  ( enumArray
  , unCamel
  , compareThen
  , ymd
  , (^)
  , (?), doIf
  , maybeDo
  , filterOut
  , intersperse
  ) where

import Prelude

import Data.Array (filter, uncons, (:))
import Data.Enum (class BoundedEnum, enumFromTo, toEnum)
import Data.Date (Date, exactDate)
import Data.Date.Component(Month)
import Data.Foldable (notElem)
import Data.Function.Memoize (memoize)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (Pattern(..), stripPrefix, stripSuffix)
import Data.String.CodePoints (fromCodePointArray, toCodePointArray)
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Regex.Flags (global)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

-- Since (^) == `Tuple`, it is recommended that its usage should be surrounded
-- by parentheses in order to resemble tuple notation in other languages. 
-- (a ^ b)` is much more reminiscent of `(a, b)` than `a ^ b`.
infixr 0 Tuple   as ^
infixr 9 doIf    as ?

-- | ```
-- | doIf true  = identity
-- | doIf false = const identity
-- | ```
doIf :: ∀ a. Boolean -> (a -> a) -> (a -> a)
doIf true  = identity
doIf false = const identity

-- | `maybeDo f x = fromMaybe x $ f x`
maybeDo :: ∀ a. (a -> Maybe a) -> a -> a
maybeDo f x = fromMaybe x $ f x

-- | `enumArray = enumFromTo bottom top`
enumArray :: ∀ a. BoundedEnum a => Array a
enumArray = flip memoize unit \_ -> enumFromTo bottom top

-- | Builds a `Date` out of a year, month, and day.
ymd :: Int -> Month -> Int -> Date
ymd y m d = unsafePartial fromJust do
    y' <- toEnum y
    d' <- toEnum d
    exactDate y' m d'

-- | Converts `NightMode` into "Night Mode" etc.
unCamel :: String -> String
unCamel = replace camel "$1 $2" <<< 
          maybeDo (stripPrefix (Pattern "(") >=> stripSuffix (Pattern ")"))
camel :: Regex
camel = unsafeRegex "([a-z])([A-Z])|([A-Z])([A-Z][a-z])" global

-- | Compares by a first function, then by a second if the first yielded EQ.
compareThen :: ∀ a b c. Ord b => Ord c => (a -> b) -> (a -> c) -> a -> a 
            -> Ordering
compareThen f g x y = case compare (f x) (f y) of
                          EQ -> compare (g x) (g y)
                          a  -> a

-- | Removes all characters in a `Pattern` from a `String`.
filterOut :: Pattern -> String -> String
filterOut (Pattern p) = fromCodePointArray <<< filter (flip notElem ps) <<< 
                        toCodePointArray
  where
    ps = toCodePointArray p

-- | Adds an element between every element in an array. 
-- | `intersperse 0 [1,2,3] == [1,0,2,0,3]`
intersperse :: ∀ a. a -> Array a -> Array a
intersperse _ x@[_] = x
intersperse sep xs = case uncons xs of
    Nothing -> xs
    Just {head, tail} -> head : sep : intersperse sep tail
