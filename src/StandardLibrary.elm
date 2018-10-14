module StandardLibrary exposing
  ( Value
  , compareWith
  , consAfter
  , curry
  , doIf
  , dict
  , enumToOrd
  , flip
  , maybeDo
  , on
  , pure
  , removeWith
  , stripPrefix, stripSuffix
  )

{-| A "classy prelude" that fills in holes in `Basic`,
including several functions that were removed in Elm 0.19. -}

import Dict    exposing (Dict)
import Json.Encode as E
import List.Extra  as List

type alias Value = E.Value

{-| Compares two arguments with the supplied projection function. -}
compareWith : a -> a -> (a -> comparable) -> Order
compareWith x y f = on compare f x y

{-| Appends an element to the end of a list.
Useful combined with `doIf` to conditionally append an HTML element. -}
consAfter : a -> List a -> List a
consAfter = List.singleton >> flip (++)

{-| Transforms a function that accepts a single tuple argument
into one that accepts two arguments. -}
curry : ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

{-| Conditionally performs a transformation. 

    doIf True  f x = f x
    doIf False f x = x
-}
doIf : Bool -> (a -> a) -> (a -> a)
doIf a = case a of
  True  -> identity
  False -> always identity

{-| Creates a `Dict` using a function that generates (key, value) pairs. -}
dict : List a -> (a -> (comparable, b)) -> Dict comparable b
dict xs = flip List.map xs >> Dict.fromList

{-| Creates a comparable projection function for enumerated types. -}
enumToOrd : List a -> (a -> Int)
enumToOrd xs x = Maybe.withDefault (-1) <| List.findIndex ((==) x) xs

{-| Flips the arguments of a function. -}
flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

{-| Performs a transformation only if it returns a `Just` value. -}
maybeDo : (a -> Maybe a) -> a -> a
maybeDo f x = Maybe.withDefault x <| f x

{-| Applies a function to both arguments. -}
on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f a b = op (f a) (f b)

{-| `(_, Cmd.none)` -}
pure : a -> (a, Cmd b)
pure x = (x, Cmd.none)

{-| Removes an element from a list using the supplied equality function. -}
removeWith : (a -> a -> Bool) -> a -> List a -> List a
removeWith eq x xs = case xs of
  []      -> []
  y :: ys ->
    if eq x y then
      ys
    else
      y :: removeWith eq x ys

{-| Removes a string from the beginning of another string. -}
stripPrefix : String -> String -> String
stripPrefix pattern s = 
    if String.startsWith pattern s then
      String.dropLeft (String.length pattern) s
    else 
      s

{-| Removes a string from the end of another string. -}
stripSuffix : String -> String -> String
stripSuffix pattern s = 
    if String.endsWith pattern s then
      String.dropRight (String.length pattern) s
    else 
      s
