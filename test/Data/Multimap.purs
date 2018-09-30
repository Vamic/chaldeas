module Test.Data.Multimap where

import StandardLibrary
import Data.Map as Map
import Data.List as List
import Data.Set as Set

import Data.Set (Set)
import Data.Unfoldable (class Unfoldable)

type Multimap k v = Map k (Set v)

insert :: ∀ k v. Ord k => Ord v => k -> v -> Multimap k v -> Multimap k v
insert k v = Map.alter (Just <<< go) k
  where
    go Nothing   = Set.singleton v
    go (Just vs) = Set.insert v vs

fromFoldable :: ∀ k v f. Ord k => Ord v => Foldable f
             => f (k : v) -> Multimap k v
fromFoldable = foldl (flip $ uncurry insert) mempty

union :: ∀ k v. Ord k => Ord v => Multimap k v -> Multimap k v -> Multimap k v
union = Map.unionWith Set.union

unions :: ∀ k v f. Ord k => Ord v => Foldable f
       => f (Multimap k v) -> Multimap k v
unions = foldl union mempty

toUnfoldable :: ∀ k v f. Ord k => Ord v => Unfoldable f
             => Multimap k v -> f (k : v)
toUnfoldable x = List.toUnfoldable do
    (k : vs) <- Map.toUnfoldableUnordered x
    v        <- Set.toUnfoldable vs
    pure (k : v)
