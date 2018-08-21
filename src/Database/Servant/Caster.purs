module Database.Servant.Caster where

import Operators
import Database.Model

casters ∷ Array Servant
casters = Servant ↤
[ { name:     "Zhuge Liang (El-Melloi II)"
  , rarity:   5
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1637,  hp: 2091 }
              , max:   { atk: 10598, hp: 14259 }
              , grail: { atk: 11601, hp: 15621 }
              }
  , ratings:  { damage:2, np:4, critical:4, utility:4, support:5, durability:3 }
  , actives:  [ { name:   "Discerning Eye A"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Ally 3 CritUp 20.0
                          , To Ally GaugeUp 50.0
                          ]
                }
              , { name:   "Tactician's Advice A+"
                , icon:   IconShieldUp
                , cd:     6
                , effect: [ Grant Party 3 DefenseUp 30.0
                          , Grant Party 3 ReduceDamage 500.0
                          , To Party GaugeUp 10.0
                          ]
                }
              , { name:   "Tactician's Command A+"
                , icon:   IconSwordUp
                , cd:     6
                , effect: [ Grant Party 3 AttackUp 30.0
                          , Grant Party 3 DamageUp 500.0
                          , To Party GaugeUp 10.0
                          ]
                }
              ]
  , passives: [itemConstruction:B, territoryCreation:A]
  , phantasm: { name:   "Unreturning Formation"
              , desc:   "Stone Sentinel Maze"
              , rank:   CMinus 
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Debuff Enemies 3 DefenseDown 50.0
                        , Debuff Enemies 3 Curse 500.0
                        , To Enemies GaugeDown 1.0
                        ]
              , over:   [ Debuff Enemies 1 StunChance 50.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 10.8, npPerHit: 1.64, npAttacked: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    34.5
  , align:    Neutral:Good
  }
, { name:     "Tamamo no Mae"
  , rarity:   5
  , class:    Caster
  , attr:     Sky
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1629,  hp: 2091 }
              , max:   { atk: 10546, hp: 14259 }
              , grail: { atk: 11544, hp: 15621 }
              }
  , ratings:  { damage:1, np:3, critical:1, utility:5, support:5, durability:4 }
  , actives:  [ { name:   "Curse EX"
                , icon:   IconDarkMagic
                , cd:     5
                , effect: [ To Enemy Drain 100.0 ]
                }
              , { name:   "Morph A"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 30.0 
                          , Grant Self 1 DefenseUp 30.0
                          ]
                }
              , { name:   "Fox's Wedding EX"
                , icon:   IconArtsUp
                , cd:     5
                , effect: [ Grant Ally 3 ArtsUp 50.0
                          , To Ally Heal 2500.0
                          ]
                }
              ]
  , passives: [territoryCreation:C, divinity:A]
  , phantasm: { name:   "Eightfold Blessings of Amaterasu"
              , desc:   "on Heavy Stone under the Sunlit Watery Heavens"
              , rank:   D
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ To Allies Cooldowns 1.0
                        , To Allies Heal 3000.0
                        ]
              , over:   [ To Allies GaugeUp 25.0 ]
              }
  , gen:      { starAbsorb: 49, starGen: 11.0, npPerHit: 0.32, npAttacked: 3 }
  , hits:     { a: 5, b: 1, q: 3, ex: 4 }
  , traits:   [Male, Divine, EnumaElish, HeavenOrEarth]
  , death:    36.0
  , align:    Neutral:Evil
  }
, { name:     "Xuanzang Sanzang"
  , rarity:   5
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1801,  hp: 1901 }
              , max:   { atk: 11658, hp: 12965 }
              , grail: { atk: 12761, hp: 14204 }
              }
  , ratings:  { damage:3, np:5, critical:3, utility:3, support:4, durability:3 }
  , actives:  [ { name:   "Rapid Sutra Chanting A"
                , icon:   IconNoble
                , cd:     7
                , effect: [ To Self GaugeUp 80.0 
                          , Grant Self 1 NPUp 20.0
                          ]
                }
              , { name:   "Captivating Rosy Cheeks A"
                , icon:   IconTargetUp
                , cd:     6
                , effect: [ Grant Self 1 Taunt 100.0 
                          , Grant Self 1 ReduceDamage 1500.0
                          ]
                }
              , { name:   "Sanzang's Teaching A"
                , icon:   IconNobleUp
                , cd:     5
                , effect: [ Grant Party 3 NPGen 30.0 
                          , Grant Party 3 StarGen 30.0
                          , Grant Party 1 DebuffImmunity 100.0
                          ]
                }
              ]
  , passives: [territoryCreation:APlus, divinity:D]
  , phantasm: { name:   "Five Elements Mountain Buddha Palm"
              , desc:   "Wu Xing Shan: Shijia Rulai Zhang"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   12
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ Debuff Enemy 1 CritDown 80.0 ]
              }
  , gen:      { starAbsorb: 52, starGen: 11.0, npPerHit: 0.82, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 6 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    34.5
  , align:    Lawful:Good
  }
]
