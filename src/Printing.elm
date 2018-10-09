module Printing exposing 
  ( stars
  , places, commas 
  , filterOut
  , fileName, urlName
  , unCamel
  , prettify
  )

import Regex exposing (Regex)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)

import StandardLibrary exposing (..)

stars : Bool -> Int -> String
stars padded rarity =
  if padded then
    String.join "  " <| List.repeat rarity "★" 
  else
    List.repeat rarity '★'
    |> List.intersperse ' '
    >> String.fromList

commas : Float -> String
commas = format { usLocale | decimals = 0 }

places : Int -> Float -> String
places decimals =
    format { usLocale | decimals = decimals, thousandSeparator = "" }


filterOut : String -> String -> String
filterOut pattern = 
    String.filter <| 
    String.fromChar >> flip String.contains pattern >> not

fileName : String -> String
fileName = filterOut "?:/"

urlName : String -> String
urlName = String.filter <| (/=) ' '

unCamel : String -> String
unCamel = 
  let
    replacer : Regex.Match -> String
    replacer {match, submatches} = case submatches of
      Just x :: Just y :: _ -> x ++ " " ++ y
      _                     -> match
  in 
    Regex.replace camel replacer
    >> String.replace " The " " the "
    >> String.replace " Of " " of "

camel : Regex
camel = 
    Maybe.withDefault Regex.never <| 
    Regex.fromString "([a-z])([A-Z])|([A-Z])([A-Z][a-z])"

prettify : String -> String
prettify a = case a of
  "Fergus mac Roich" -> "Fergus mac Róich"
  "Mugashiki—Shinkuu Myou" -> "Mugashiki—Shinkuu Myōu"
  "Heroic Portrait: Scathach" -> "Heroic Portrait: Scáthach"
  "Cu Chulainn" -> "Cú Chulainn"
  "Cu Chulainn (Prototype)" -> "Cú Chulainn (Prototype)"
  "Cu Chulainn (Alter)" -> "Cú Chulainn (Alter)"
  "Cu Chulainn (Caster)" -> "Cú Chulainn (Caster)"
  "Elisabeth Bathory" -> "Elisabeth Báthory"
  "Elisabeth Bathory (Halloween)" -> "Elisabeth Báthory (Halloween)"
  "Scathach" -> "Scáthach"
  "Scathach (Assassin)" -> "Scáthach (Assassin)"
  "Angra Mainyu" -> "Aŋra Mainiiu"
  "Edmond Dantes" -> "Edmond Dantès"
  "Leonardo da Vinci" -> "Leonardo Da Vinci"
  "Wisdom of Dun Scaith" -> "Wisdom of Dún Scáith"
  _ -> a
