module Test.Base 
  ( class CleanShow
  , cleanShow
  , MaybeRank(..)
  , showRank, unRank, addRank, getRank
  , indices
  ) where

import Prelude 
import Operators (enumArray)

import Data.Array as Array
import Data.String as String
import Data.Maybe as Maybe
import Database as DB

import Control.Alt ((<|>))
import Data.Foldable (foldl)
import Data.Function.Memoize (memoize)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((&&&))
import Data.String (Pattern(..))
import Data.Tuple (Tuple)



-- | Shows strings without quoting them.
class CleanShow a where
    cleanShow :: a -> String
instance _0_ :: CleanShow String where
    cleanShow = identity
instance _1_ :: CleanShow DB.Servant where
    cleanShow = show
instance _2_ :: CleanShow DB.CraftEssence where
    cleanShow = show
instance _3_ :: CleanShow DB.ActiveEffect where
    cleanShow = show
instance _4_ :: CleanShow DB.RangeInfo where
    cleanShow = show

data MaybeRank = Unranked
               | Pure     DB.Rank
               | Upgrade  DB.Rank
               | Unique   DB.Servant DB.Rank
               
showRank :: MaybeRank -> Maybe String
showRank Unranked              = Nothing
showRank (Pure rank)           = Just $ show rank
showRank (Upgrade DB.Unknown)  = Just "NP"
showRank (Upgrade rank)        = Just $ "'" <> show rank <> "'"
showRank (Unique s DB.Unknown) = Just $ "(" <> show s <> ")"
showRank (Unique s rank)       = Just $ show rank <> " (" <> nameS s <> ")"
    where 
      nameS (DB.Servant {name:"Henry Jekyll & Hyde"}) = "Hyde"
      nameS (DB.Servant {name}) = name

unRank :: MaybeRank -> String -> String
unRank mRank s = Maybe.fromMaybe s do
    rank <- showRank mRank
    String.stripSuffix (Pattern $ " " <> rank) s

indices :: Array String -> String -> Maybe Int
indices patterns s = foldl ((<|>)) Nothing $ 
                     flip String.indexOf s <<< Pattern <$> patterns

getRank :: String -> MaybeRank
getRank = memoize go
  where
    go s = Maybe.fromMaybe Unranked $ 
           Array.find match (Unique  <$> DB.servants <*> enumArray) <|>
           Array.find match (Pure    <$> enumArray) <|>
           Array.find match (Upgrade <$> enumArray) 
      where
        match mRank = Maybe.isJust do
            rank <- showRank mRank
            String.stripSuffix (Pattern $ " " <> rank) s

addRank :: ∀ a. (a -> MaybeRank) -> a -> Tuple MaybeRank a
addRank f = f &&& identity
