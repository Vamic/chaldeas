-- | Fixed rates for leveling `Servant`s based on their rarity.
module MyServant.Leveling (maxLevel, ascendCost, skillCost) where

import StandardLibrary
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


skillCost :: Servant -> Int -> Int
skillCost (Servant {rarity}) = (_ * 1000) <<< fromMaybe 0 <<< index costs
  where
    costs = case rarity of
                1 -> [ 10,  20,   60,   80,  200,  250,   500,   600,  1000]
                3 -> [ 50, 100,  300,  400, 1000, 1250,  2500,  3000,  5000]
                4 -> [100, 200,  600,  800, 2000, 2500,  5000,  6000, 10000]
                5 -> [200, 400, 1200, 1600, 4000, 5000, 10000, 12000, 20000]
                _ -> [ 20,  40,  120,  160,  400,  500,  1000,  1200,  2000]
