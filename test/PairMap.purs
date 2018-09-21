module Test.PairMap where

import StandardLibrary
import Data.Map as Map

import Data.List (List)

type PairMap a b c = Map a (Map b c)

insert :: ∀ a b c. Ord a => Ord b
       => (a : b) -> c -> PairMap a b c -> PairMap a b c
insert (a : b) c = Map.alter (Just <<< go) a
  where
    go Nothing    = Map.singleton b c
    go (Just sub) = Map.insert b c sub

fromFoldable :: ∀ a b c f. Ord a => Ord b => Foldable f
             => f ((a : b) : c) -> PairMap a b c
fromFoldable = foldl (flip $ uncurry insert) Map.empty

lookup :: ∀ a b c. Ord a => Ord b => a -> b -> PairMap a b c -> Maybe c
lookup a b pm = Map.lookup a pm >>= Map.lookup b

union :: ∀ a b c. Ord a => Ord b
       => PairMap a b c -> PairMap a b c -> PairMap a b c
union = Map.unionWith Map.union

unions :: ∀ a b c f. Ord a => Ord b => Foldable f
       => f (PairMap a b c) -> PairMap a b c
unions = foldl union Map.empty

flatten :: ∀ a b c. Ord a => Ord b => PairMap a b c -> List (a : b : c)
flatten xs = do
    a1 : a2 <- Map.toUnfoldableUnordered xs
    b1 : b2 <- Map.toUnfoldableUnordered a2
    pure $ a1 : b1 : b2
