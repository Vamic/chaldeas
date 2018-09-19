-- | A "classy prelude" that augments Prelude with other necessary modules
-- | and includes a few missing helper functions such as `intersperse`.
module StandardLibrary
  ( module Prelude
  , module Control.Alternative
  , module Control.Bind
  , module Control.MonadZero
  , module Control.Plus
  , module Data.Array
  , module Data.Either
  , module Data.Enum
  , module Data.Foldable
  , module Data.Function.Memoize
  , module Data.Map
  , module Data.Maybe
  , module Data.Number
  , module Data.Ord
  , module Data.String
  , module Data.String.Regex
  , module Data.Traversable
  , module Data.Tuple
  , module Effect
  , module Effect.Aff
  , module Effect.Class

  , (^)
  , (?), doIf
  , consAfter
  , enumArray
  , compareThen
  , maybeDo
  , filterOut
  , intersperse
  ) where

-----------
--- EXPORTS
-----------

import Prelude

import Control.Alternative (class Alternative, (<|>))
import Control.Bind (bindFlipped)
import Control.MonadZero (guard)
import Control.Plus (empty)
import Data.Array (cons, delete, difference, drop, dropWhile, filter, group, groupBy, head, index, init, intersect, last, mapMaybe, nub, nubBy, nubEq, nubByEq, partition, range, replicate, reverse, snoc, sort, sortBy, sortWith, span, tail, take, takeEnd, takeWhile, uncons, union, unionBy, unzip, zip, zipWith, (!!), (..), (:), (\\))
import Data.Either (Either(Left, Right), choose, either, hush, isLeft, isRight)
import Data.Enum (class BoundedEnum, enumFromTo, toEnum)
import Data.Foldable (class Foldable, all, and, any, elem, find, fold, foldl, foldr, for_, intercalate, length, maximum, maximumBy, minimum, minimumBy, notElem, null, oneOf, oneOfMap, or, product, sequence_, sum, traverse_)
import Data.Maybe (Maybe(Just,Nothing), maybe, fromMaybe, fromMaybe', isJust, isNothing)
import Data.Map (Map)
import Data.Number (infinity)
import Data.Ord (abs, signum)
import Data.String (Pattern(..), Replacement(..))
import Data.String.Regex (Regex)
import Data.Traversable (scanr, scanl, traverse)
import Data.Function.Memoize (memoize)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)

--------------------
--- HELPER FUNCTIONS
--------------------

import Data.String.CodePoints (fromCodePointArray, toCodePointArray)

-- Since (^) == `Tuple`, it is recommended that its usage should be surrounded
-- by parentheses in order to resemble tuple notation in other languages.
-- (a ^ b)` is much more reminiscent of `(a, b)` than `a ^ b`.
infixr 0 Tuple as ^
infixr 9 doIf  as ?

-- infixr 4 type Tuple as ^

consAfter :: ∀ a. a -> Array a -> Array a
consAfter = flip snoc

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

-- | Compares by a first function, then by a second if the first yielded EQ.
compareThen :: ∀ a b c. Ord b => Ord c => (a -> b) -> (a -> c) -> a -> a
            -> Ordering
compareThen f g x y = case compare (f x) (f y) of
                          EQ -> compare (g x) (g y)
                          a  -> a

-- | Removes all characters in a `Pattern` from a `String`.
filterOut :: Pattern -> String -> String
filterOut (Pattern p) = fromCodePointArray <<<
                        filter (flip notElem ps) <<<
                        toCodePointArray
  where
    ps = toCodePointArray p

-- | Adds an element between every element in an
-- | `intersperse 0 [1,2,3] == [1,0,2,0,3]`
intersperse :: ∀ a. a -> Array a -> Array a
intersperse _ x@[_] = x
intersperse sep xs = case uncons xs of
    Nothing -> xs
    Just {head, tail} -> head : sep : intersperse sep tail
