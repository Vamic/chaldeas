module StandardLibrary exposing
  ( Value
  , compareWith
  , consAfter
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

import Dict    exposing (Dict)
import Json.Encode        as E
import List.Extra         as List

type alias Value = E.Value

compareWith : a -> a -> (a -> comparable) -> Order
compareWith x y f = on compare f x y

consAfter : a -> List a -> List a
consAfter = List.singleton >> flip (++)

doIf : Bool -> (a -> a) -> (a -> a)
doIf a = case a of
  True  -> identity
  False -> always identity

dict : List a -> (a -> (comparable, b)) -> Dict comparable b
dict xs = Dict.fromList << flip List.map xs

enumToOrd : List a -> a -> Int
enumToOrd xs x = Maybe.withDefault (-1) <| List.findIndex ((==) x) xs

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

maybeDo : (a -> Maybe a) -> a -> a
maybeDo f x = Maybe.withDefault x <| f x

on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f a b = op (f a) (f b)

pure : a -> (a, Cmd b)
pure x = (x, Cmd.none)

removeWith : (a -> a -> Bool) -> a -> List a -> List a
removeWith eq x xs = case xs of
  []      -> []
  y :: ys ->
    if eq x y then
      ys
    else
      y :: removeWith eq x ys

stripPrefix : String -> String -> String
stripPrefix pattern s = 
    if String.startsWith pattern s then
      String.dropLeft (String.length pattern) s
    else 
      s

stripSuffix : String -> String -> String
stripSuffix pattern s = 
    if String.endsWith pattern s then
      String.dropRight (String.length pattern) s
    else 
      s
