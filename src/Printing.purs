module Printing where

import Prelude
import Data.Formatter.Number
import Data.Int (pow, toNumber)
import Math (round)

print ∷ Int -> Number -> String
print places = format $ Formatter { comma: true
                                  , before: 0
                                  , after: places
                                  , abbreviations: false
                                  , sign: false
                                  }

roundTo ∷ Int -> Number -> Number
roundTo places = (_ / zeroes) <<< round <<< (_ * zeroes)
  where zeroes = toNumber $ pow 10 places
