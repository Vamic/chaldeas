-- | Fixed rates for leveling `Servant`s based on their rarity.
module MyServant.Leveling 
  ( maxLevel
  , ascendCost
  , ascendWishlist
  , skillCost
  ) where

import StandardLibrary
import Data.Array.NonEmpty as NonEmpty

import MyServant
import Database

maxLevel :: Servant -> Int
maxLevel (Servant {rarity: 1}) = 60
maxLevel (Servant {rarity: 3}) = 70
maxLevel (Servant {rarity: 4}) = 80
maxLevel (Servant {rarity: 5}) = 90
maxLevel (Servant {rarity: _}) = 65

ascendCost :: Servant -> Int -> Int
ascendCost (Servant {name: "Mash Kyrielight"}) = const 0
ascendCost (Servant {rarity}) = (_ * 1000) <<< fromMaybe 0 <<< index costs
  where
    costs = case rarity of
                1 -> [ 10,  30,   90,  300]
                3 -> [ 30, 100,  300,  900]
                4 -> [ 50, 150,  500, 1500]
                5 -> [100, 300, 1000, 3000]
                _ -> [15, 45, 150, 450]

ascendMats :: Servant -> Int -> Array (Material : Int)
ascendMats (Servant {ascendUp: Ascension a b c d}) i = join $ case i of
    0 -> [a, b, c, d]
    1 -> [b, c, d]
    2 -> [c, d]
    3 -> [d]
    _ -> []
ascendMats _ _ = []

atAscension :: MyServant -> Int
atAscension (MyServant {level, base:(Servant {rarity})}) = case rarity of
    5
      | level <= 50 -> 0
      | level <= 60 -> 1
      | level <= 70 -> 2
      | level <= 80 -> 3
      | otherwise   -> 4
    4
      | level <= 40 -> 0
      | level <= 50 -> 1
      | level <= 60 -> 2
      | level <= 70 -> 3
      | otherwise   -> 4
    3
      | level <= 30 -> 0
      | level <= 40 -> 1
      | level <= 50 -> 2
      | level <= 60 -> 3
      | otherwise   -> 4
    1
      | level <= 20 -> 0
      | level <= 30 -> 1
      | level <= 40 -> 2
      | level <= 50 -> 3
      | otherwise   -> 4
    _
      | level <= 25 -> 0
      | level <= 35 -> 1
      | level <= 45 -> 2
      | level <= 55 -> 3
      | otherwise   -> 4

ascendWishlist :: Array MyServant -> Array (Material : Int)
ascendWishlist xs = map reduce <<< groupBy (eq `on` fst) <<< sortWith fst $
                    xs >>= go
  where
    go x       = ascendMats (getBase x) $ atAscension x
    reduce xss = fst (NonEmpty.head xss) : sum (snd <$> xss)

skillCost :: Servant -> Int -> Int
skillCost (Servant {rarity}) = (_ * 1000) <<< fromMaybe 0 <<< index costs
  where
    costs = case rarity of
                1 -> [ 10,  20,   60,   80,  200,  250,   500,   600,  1000]
                3 -> [ 50, 100,  300,  400, 1000, 1250,  2500,  3000,  5000]
                4 -> [100, 200,  600,  800, 2000, 2500,  5000,  6000, 10000]
                5 -> [200, 400, 1200, 1600, 4000, 5000, 10000, 12000, 20000]
                _ -> [ 20,  40,  120,  160,  400,  500,  1000,  1200,  2000]
