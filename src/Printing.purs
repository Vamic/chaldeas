module Printing where

import Prelude
import Data.Formatter.Number
import Data.Int (pow, toNumber)
import Math (round)

places :: Int -> Number -> String
places x = format $ Formatter { comma: true
                              , before: 0
                              , after: x
                              , abbreviations: false
                              , sign: false
                              }

roundTo :: Int -> Number -> Number
roundTo x = (_ / zeroes) <<< round <<< (_ * zeroes)
  where zeroes = toNumber $ pow 10 x

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
