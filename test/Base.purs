module Test.Base 
  ( MaybeRank(..)
  , addRank
  , indices
  ) where

import Prelude 

import Data.String as String
import Database as DB

import Control.Alt ((<|>))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((&&&))
import Data.String (Pattern(..))
import Data.Tuple (Tuple)

data MaybeRank = Unranked
               | Pure     DB.Rank
               | Upgrade  DB.Rank
               | Unique   DB.Servant DB.Rank
               
instance _0_ :: Show MaybeRank where
  show Unranked              = "--"
  show (Pure rank)           = "Rank" <> show rank
  show (Upgrade DB.Unknown)  = "NP"
  show (Upgrade rank)        = "Rank '" <> String.trim (show rank) <> "'"
  show (Unique s DB.Unknown) = show s
  show (Unique s rank)
    | show s == "Henry Jekyll & Hyde" = "Rank" <> show rank <> " (Hyde)"
    | otherwise                       = show s

indices :: Array String -> String -> Maybe Int
indices patterns s = foldl ((<|>)) Nothing $ 
                     flip String.indexOf s <<< Pattern <$> patterns

addRank :: ∀ a. (a -> MaybeRank) -> a -> Tuple MaybeRank a
addRank f = f &&& identity
