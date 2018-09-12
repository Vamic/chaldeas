module Test.Base 
  ( MaybeRank(..), RankedSkill(..)
  , addRank
  , indices
  ) where

import Prelude 
import Operators (compareThen)

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
derive instance _0_ :: Eq MaybeRank
derive instance _1_ :: Ord MaybeRank
instance _2_ :: Show MaybeRank where
  show Unranked              = "--"
  show (Pure rank)           = "Rank" <> show rank
  show (Upgrade DB.Unknown)  = "NP"
  show (Upgrade rank)        = "Rank '" <> String.trim (show rank) <> "'"
  show (Unique s DB.Unknown) = show s
  show (Unique s rank)
    | show s == "Henry Jekyll & Hyde" = "Rank" <> show rank <> " (Hyde)"
    | otherwise                       = show s

data RankedSkill = RankedSkill DB.Skill MaybeRank
derive instance _3_ :: Eq RankedSkill
instance _4_ :: Ord RankedSkill where
  compare = compareThen show \(RankedSkill _ rank) -> rank
instance _5_ :: Show RankedSkill where
  show (RankedSkill skill _) = skill.name

indices :: Array String -> String -> Maybe Int
indices patterns s = foldl ((<|>)) Nothing $ 
                     flip String.indexOf s <<< Pattern <$> patterns

addRank :: ∀ a. (a -> MaybeRank) -> a -> Tuple MaybeRank a
addRank f = f &&& identity
