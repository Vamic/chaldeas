module Database.Servant.Rider where

import Operators
import Database.Model

riders ∷ Array Servant
riders = Servant ↤
[ { name:     "Ozymandias"
  , rarity:   5
  , class:    Rider
  , attr:     Sky
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1850,  hp: 1881 }
              , max:   { atk: 11971, hp: 12830 }
              , grail: { atk: 13104, hp: 14056 }
              }
  , ratings:  { damage:4, np:4, critical:2, utility:4, support:4, durability:3 }
  , actives:  [ { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 18.0 ]
                }
              , { name:   "Imperial Privilege A"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Self Heal 3000.0
                          , Chance 60 Self 3 AttackUp 40.0
                          , Chance 60 Self 3 DefenseUp 40.0
                          ]
                }
              , { name:   "Protection of the Sun God A"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Party GaugeUp 20.0 
                          , Grant Party 1 BuffUp 40.0
                          ]
                }
              ]
  , passives: [magicResistance:B, riding:APlus, divinity:B]
  , phantasm: { name:   "Ramesseum Tentyris"
              , desc:   "The Shining Great Temple Complex"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Fortress" 
              , hits:   5
              , effect: [ To Enemy Damage 1000.0 
                        , Debuff Enemy 1 SealNP 100.0
                        ]
              , over:   [ Debuff Enemy 3 DefenseDown 20.0 ]
              }
  , gen:      { starAbsorb: 205, starGen: 9.0, npPerHit: 0.59, npAttacked: 3 }
  , hits:     { a: 3, b: 1, q: 5, ex: 5 }
  , traits:   [Male, Brynhildr, EarthOrSky, Divine, Riding, EnumaElish]
  , death:    30.0
  , align:    Chaotic:Neutral
  }
, { name:     "Francis Drake"
  , rarity:   5
  , class:    Rider
  , attr:     Sky
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1750,  hp: 1881 }
              , max:   { atk: 11326, hp: 12830 }
              , grail: { atk: 12398, hp: 14056 }
              }
  , ratings:  { damage:4, np:5, critical:5, utility:3, support:3, durability:2 }
  , actives:  [ { name:   "Voyager of the Storm A+"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Party 1 NPUp 17.0
                          , Grant Party 1 AttackUp 17.0
                          ]
                }
              , { name:   "Golden Rule B"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 45.0 ]
                }
              , { name:   "Pioneer of the Stars EX"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0 
                          , Grant Self 3 IgnoreInvinc 100.0
                          , To Party GainStars 10.0
                          ]
                }
              ]
  , passives: [magicResistance:D, riding:D]
  , phantasm: { name:   "Golden Wild Hunt"
              , desc:   "Golden Stag and the Eventide Tempest"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage 600.0 ]
              , over:   [ To Party GainStars 20.0 ]
              }
  , gen:      { starAbsorb: 208, starGen: 9.0, npPerHit: 0.42, npAttacked: 3 }
  , hits:     { a: 4, b: 2, q: 6, ex: 4 }
  , traits:   [Female, Riding]
  , death:    50.0
  , align:    Chaotic:Evil
  }
]
