module Database.Servant.Saber where

import Operators
import Database.Model

sabers ∷ Array Servant
sabers = Servant ↤
[ { name:     "Okita Souji"
  , rarity:   5
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1865,  hp: 1939 }
              , max:   { atk: 12068, hp: 13225 }
              , grail: { atk: 13210, hp: 14489 }
              }
  , ratings:  { damage:5, np:5, critical:5, utility:3, support:2, durability:3 }
  , actives:  [ { name:   "Shukuchi B"
                , icon:   IconQuickUp
                , cd:     5
                , effect: [ Grant Self 1 QuickUp 50.0 ]
                }
              , { name:   "Weak Constitution A"
                , icon:   IconStarUp
                , cd:     5
                , effect: [ Grant Self 1 StarAbsorb 1000.0 ]
                }
              , { name:   "Mind's Eye (Fake) A"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 CritUp 40.0
                          ]
                }
              ]
  , passives: [magicResistance:E, riding:E]
  , phantasm: { name:   "Mumyou Sandanzuki"
              , desc:   "Three-Stage Thrust"
              , rank:   Unknown
              , card:   Quick 
              , kind:   "Anti-Personnel"
              , hits:   3
              , effect: [ To Enemy DamageThruDef 2000.0 ]
              , over:   [ Debuff Enemy 3 DefenseDown 30.0 ]
              }
  , gen:      { starAbsorb: 98, starGen: 10.2, npPerHit: 1.09, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 5, ex: 3 }
  , traits:   [Female, Riding, Saberface, EnumaElish]
  , death:    35.0
  , align:    Neutral:Neutral
  }
, { name:     "Altera"
  , rarity:   5
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1907,  hp: 2039 }
              , max:   { atk: 12343, hp: 13907 }
              , grail: { atk: 13511, hp: 15236 }
              }
  , ratings:  { damage:5, np:3, critical:3, utility:2, support:1, durability:3 }
  , actives:  [ { name:   "Tactics B"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Party 1 NPUp 18.0 ]
                }
              , { name:   "Natural Body D"
                , icon:   IconHoodUp
                , cd:     5
                , effect: [ Grant Self 3 DebuffResist 80.0 
                          , To Self Heal 2500.0
                          ]
                }
              , { name:   "Natural Body EX"
                , icon:   IconHoodUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 10.0 
                          , To Party GainStars 15.0
                          ]
                }
              ]
  , passives: [magicResistance:B, riding:A, divinity:B]
  , phantasm: { name:   "Photon Ray"
              , desc:   "Sword of the God of War"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage 600.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown 40.0 ]
              }
  , gen:      { starAbsorb: 102, starGen: 10.1, npPerHit: 0.84, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, Divine, EnumaElish]
  , death:    24.5
  , align:    Chaotic:Good
  }
, { name:     "Nero Claudius (Bride)"
  , rarity:   5
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1793,  hp: 2089 }
              , max:   { atk: 11607, hp: 14248 }
              , grail: { atk: 12706, hp: 15609 }
              }
  , ratings:  { damage:4, np:4, critical:2, utility:4, support:5, durability:3 }
  , actives:  [ { name:   "Stars for the Sky A"
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Ally 3 NPGen 45.0 ]
                }
              , { name:   "Flowers for the Earth A"
                , icon:   IconSwordUp
                , cd:     6
                , effect: [ Grant Ally 3 AttackUp 40.0 
                          , Grant Ally 3 StarGen 50.0
                          ]
                }
              , { name:   "Love for the People A"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Ally Heal 3000.0 
                          , Grant Ally 3 DefenseUp 20.0
                          ]
                }
              ]
  , passives: [magicResistance:C, riding:B]
  , phantasm: { name:   "Fax Caelestis"
              , desc:   "Closing Rose That Fames Stars"
              , rank:   BPlus
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   2
              , effect: [ To Enemy Damage 1500.0 ]
              , over:   [ Debuff Enemy 5 Burn 500.0 
                        , Debuff Enemy 5 DefenseDown 20.0
                        , Debuff Enemy 5 CritDown 20.0
                        ]
              }
  , gen:      { starAbsorb: 102, starGen: 10.1, npPerHit: 0.7, npAttacked: 3 }
  , hits:     { a: 3, b: 1, q: 3, ex: 4 }
  , traits:   [Female, Riding, Saberface, Roman, EnumaElish, HeavenOrEarth ]
  , death:    35.0
  , align:    Chaotic:Bride
  }
]
