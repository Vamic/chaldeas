module Database.Servant.Ruler where

import Operators
import Database.Model

rulers ∷ Array Servant
rulers = Servant ↤
[ { name:     "Jeanne d'Arc"
  , rarity:   5
  , class:    Ruler
  , attr:     Star
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1482,  hp: 2420 }
              , max:   { atk: 9593, hp: 16500 }
              , grail: { atk: 10501, hp: 18076 }
              }
  , ratings:  { damage:2, np:4, critical:3, utility:3, support:4, durability:5 }
  , actives:  [ { name:   "Revelation A"
                , icon:   IconStarTurn
                , cd:     6
                , effect: [ Grant Self 3 StarsPerTurn 9.0 ]
                }
              , { name:   "True Name Revelation B"
                , icon:   IconBeamDown
                , cd:     5
                , effect: [ Debuff Enemy 1 NPDown 30.0 ]
                }
              , { name:   "Divine Judgment A"
                , icon:   IconStun
                , cd:     6
                , effect: [ Debuff Enemy 1 StunChance 120.0 ]
                }
              ]
  , passives: [magicResistance:EX]
  , phantasm: { name:   "Luminosite Eternelle"
              , desc:   "God is Here With Me"
              , rank:   A
              , card:   Arts
              , kind:   "Barrier"
              , hits:   0
              , effect: [ Grant Party 3 DefenseUp 25.0 
                        , Grant Party 1 Invincibility 100.0
                        , To Party RemoveDebuffs 100.0
                        ]
              , over:   [ Grant Party 2 HealPerTurn 1000.0 ]
              }
  , gen:      { starAbsorb: 99, starGen: 10.1, npPerHit: 0.76, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, EnumaElish, Saberface]
  , death:    21.0
  , align:    Lawful:Good
  }
, { name:     "Amakusa Shirou"
  , rarity:   5
  , class:    Ruler
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1695,  hp: 2069 }
              , max:   { atk: 10972, hp: 14107 }
              , grail: { atk: 12011, hp: 15455 }
              }
  , ratings:  { damage:3, np:5, critical:3, utility:5, support:2, durability:4 }
  , actives:  [ { name:   "Revelation A"
                , icon:   IconStarTurn
                , cd:     6
                , effect: [ Grant Self 3 StarsPerTurn 9.0 ]
                }
              , { name:   "Baptism Rite B+"
                , icon:   IconNobleTurn
                , cd:     10
                , effect: [ Grant Self 5 GaugePerTurn 20.0 
                          , To (EnemyType Undead) GaugeDown 1.0
                          , To (EnemyType Demon) GaugeDown 1.0
                          ]
                }
              , { name:   "Divine Judgment C"
                , icon:   IconStun
                , cd:     6
                , effect: [ Debuff Enemy 1 StunChance 100.0 ]
                }
              ]
  , passives: [magicResistance:A]
  , phantasm: { name:   "Twin Arm—Big Crunch"
              , desc:   "Dual Arm Zero-Order Convergence"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-"
              , hits:   1
              , effect: [ To Enemies RemoveBuffs 100.0 
                        , To Enemies Damage 500.0
                        ]
              , over:   [ Debuff Enemies 1 CritDown 30.0 ]
              }
  , gen:      { starAbsorb: 100, starGen: 10.0, npPerHit: 0.86, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 7 }
  , traits:   [Male, EnumaElish]
  , death:    21.0
  , align:    Lawful:Good
  }
]
