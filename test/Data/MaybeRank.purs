-- | Utility data structures for testing.
module Test.Data.MaybeRank (MaybeRank(..)) where

import StandardLibrary

import Data.String as String

import Database (Rank(..), Servant)

data MaybeRank = Unranked
               | Pure     Rank
               | Upgrade  Rank
               | Unique   Servant Rank
derive instance _0_ :: Eq MaybeRank
derive instance _1_ :: Ord MaybeRank
instance _2_ :: Show MaybeRank where
    show Unranked           = "--"
    show (Pure rank)        = "Rank" <> show rank
    show (Upgrade Unknown)  = "NP"
    show (Upgrade rank)     = "Rank '" <> String.trim (show rank) <> "'"
    show (Unique s Unknown) = show s
    show (Unique s rank)
      | show s == "Mash Kyrielight"     = "Lord Camelot"
      | show s == "Henry Jekyll & Hyde" = "Rank" <> show rank <> " (Hyde)"
      | otherwise                       = show s
