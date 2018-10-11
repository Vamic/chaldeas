module Wiki exposing (Wiki, field, list, fromString)

import List.Extra  as List
import Maybe.Extra as Maybe
import Dict  exposing (Dict)
import Regex exposing (Regex)

import StandardLibrary exposing (..)
import MaybeRank       exposing (..)
type alias Wiki =
    { fields : Dict String (List String)
    , lists  : Dict String (List (List String))
    }

field : Wiki -> String -> Maybe (List String)
field = .fields >> flip Dict.get

list : Wiki -> String -> Maybe (List (List String))
list = .lists >> flip Dict.get

fromString : String -> MaybeRank -> Wiki
fromString text rank = 
  let
    lists  = Dict.fromList <| parseLists text
    firstMatch = 
        .submatches
        >> List.head
        >> Maybe.join
        >> Maybe.withDefault ""
    fields = 
        rankedSection rank text
        |> Regex.replace wikiLink firstMatch
        >> String.split "|"
        >> List.map parseFields
        >> Maybe.values
        >> List.reverse
        >> Dict.fromList
  in
    Wiki fields lists

wikiLink : Regex
wikiLink = 
    Regex.fromString "\\[\\[[^\\|\\]]+\\|([^\\]]+)\\]\\]"
    |> Maybe.withDefault Regex.never

wikiTag : Regex
wikiTag = 
    Regex.fromString "<[^\\s>]+>"
    |> Maybe.withDefault Regex.never

splitLines : String -> List String
splitLines = splitAll ["<br/>","<br>","<Br>","<Br/>","\n"]

unSpace : String -> String
unSpace = String.replace " *" "*" >> String.replace "* " "*"

splitAll : (List String) -> String -> List String
splitAll delims x = List.foldl (List.concatMap << String.split) [x] delims

splitAround : String -> String -> Maybe (String, String)
splitAround needle haystack = case String.split needle haystack of
  []      -> Nothing
  [_]     -> Nothing
  x :: xs -> Just (x, String.join needle xs)

maybeSplit : String -> String -> (String, String)
maybeSplit needle haystack = 
    splitAround needle haystack 
    |> Maybe.withDefault (haystack, "")

type Side = Before | After

oneOf : List (Maybe a) -> Maybe a
oneOf a = case a of
  []           -> Nothing
  Just x :: _  -> Just x
  _      :: xs -> oneOf xs

splitAny : Side -> List String -> String -> Maybe String
splitAny side needles haystack = 
  let
    maybeSplitted = 
        oneOf <<
        flip List.map needles <| \needle ->
        flip Maybe.map (splitAround needle haystack) <| \splitted ->
        (String.length needle, splitted)
  in
    flip Maybe.map maybeSplitted <| 
    \(len, (before, after)) -> case side of
      Before -> before
      After  -> String.dropLeft len after

-- Notation 2: Monadic Booga-Do

bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind = flip Maybe.andThen

guard : Bool -> (() -> Maybe a) -> Maybe a
guard continue = if continue then ((|>) ()) else always Nothing

parseCol : Int -> Int -> List String -> Maybe (List String)
parseCol row col lines = 
    let search = "|" ++ String.fromInt row ++ String.fromInt col in
    bind (List.find (String.left 3 >> (==) search) lines) <| \entry ->
    bind (splitAround "=" entry) <| \(_, after) ->
    let val = unSpace <| String.trim after in
    parseCol row (col + 1) lines
    |> Maybe.map ((::) val)
    >> Maybe.withDefault [val]
    >> Just

parseRow : Int -> List String -> Maybe (List (List String))
parseRow row lines =
    bind (parseCol row 1 lines) <| \cols ->
    parseRow (row + 1) lines
    |> Maybe.map ((::) cols)
    >> Maybe.withDefault [cols]
    >> Just

parseRows : String -> List (List String)
parseRows = 
    splitLines
    >> parseRow 1
    >> Maybe.withDefault []

parseLists : String -> List (String, List (List String))
parseLists text = 
    Maybe.withDefault [] <<
    bind (splitAround "==" text) <| \(_, headerStart) ->
    bind (splitAround "==" headerStart) <| \(beforeHeader, afterHeader) ->
    let header = String.trim beforeHeader in
    let (beforeSection, afterSection) = maybeSplit "==" afterHeader in
    Just <| (header, parseRows beforeSection) :: parseLists afterSection

parseFields : String -> Maybe (String, List String)
parseFields text =
    bind (List.head <| String.indices "=" text) <| \assignment ->
    bind (splitAround "=" text) <| \(before, after)  ->
    let afterLines = splitLines after in
    guard (not <| String.isEmpty before) <| \_ ->
    Just << 
    (\x -> (String.trim <| String.toLower before, x)) <<
    List.filter (not << String.isEmpty) <<
    flip List.map afterLines <|
    stripPrefix "="
    >> maybeDo (splitAny Before ["}}","/"])
    >> maybeDo (splitAny After ["EN:"])
    >> Regex.replace wikiTag (always " ")
    >> String.trim
    >> stripPrefix "#tip-text:"
    
rankedSection : MaybeRank -> String -> String
rankedSection rank text = 
    Maybe.withDefault text <<
    bind (splitAny After [MaybeRank.show rank ++ "="] text) <|
    splitAny Before ["|-|", "/onlyinclude"]
