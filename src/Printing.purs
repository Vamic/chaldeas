-- | Helper functions for outputting to Strings.
module Printing 
  ( places, commas
  , fileName, urlName
  , roundTo
  , unCamel
  , prettify
  ) where

import StandardLibrary
import Data.String.Regex.Flags  as Flags
import Data.Int                 as Int
import Math                     as Math
import Data.String.Regex        as Regex
import Data.String              as String
import Data.String.Regex.Unsafe as Unsafe

import Data.Formatter.Number (Formatter(..), format)

-- | Prints a `Number` with some number of decimal places.
places :: Int -> Number -> String
places x = format $ Formatter { comma: true
                              , before: 0
                              , after: x
                              , abbreviations: false
                              , sign: false
                              }

commas :: Int -> String
commas = places 0 <<< Int.toNumber

-- | Removes illegal special characters from file names.
fileName :: String -> String
fileName = filterOut (Pattern "?:/")

-- | Removes spaces from names in order to use them in URLs.
urlName :: String -> String
urlName = filterOut (Pattern "  ")

-- | Rounds a `Number` to some decimal precision.
roundTo :: Int -> Number -> Number
roundTo x = (_ / zeroes) <<< Math.round <<< (_ * zeroes)
  where zeroes = Int.toNumber $ Int.pow 10 x

-- | Converts `NightMode` into "Night Mode" etc.
unCamel :: String -> String
unCamel = memoize $
          String.replaceAll (Pattern " The ") (Replacement " the ") <<<
          String.replaceAll (Pattern " Of ") (Replacement " of ") <<<
          Regex.replace camel "$1 $2" <<< maybeDo unParen
  where
    unParen = String.stripPrefix (Pattern "(") >=>
              String.stripSuffix (Pattern ")")
camel :: Regex
camel = Unsafe.unsafeRegex "([a-z])([A-Z])|([A-Z])([A-Z][a-z])" Flags.global

-- | Adds in fancy diacritics to Servant and Craft Essence names.
prettify :: String -> String
prettify "Fergus mac Roich" = "Fergus mac Róich"
prettify "Mugashiki—Shinkuu Myou" = "Mugashiki—Shinkuu Myōu"
prettify "Heroic Portrait: Scathach" = "Heroic Portrait: Scáthach"
prettify "Cu Chulainn" = "Cú Chulainn"
prettify "Cu Chulainn (Prototype)" = "Cú Chulainn (Prototype)"
prettify "Cu Chulainn (Alter)" = "Cú Chulainn (Alter)"
prettify "Cu Chulainn (Caster)" = "Cú Chulainn (Caster)"
prettify "Elisabeth Bathory" = "Elisabeth Báthory"
prettify "Elisabeth Bathory (Halloween)" = "Elisabeth Báthory (Halloween)"
prettify "Scathach" = "Scáthach"
prettify "Scathach (Assassin)" = "Scáthach (Assassin)"
prettify "Angra Mainyu" = "Aŋra Mainiiu"
prettify "Edmond Dantes" = "Edmond Dantès"
prettify "Leonardo da Vinci" = "Leonardo Da Vinci"
prettify "Wisdom of Dun Scaith" = "Wisdom of Dún Scáith"
prettify x = x
