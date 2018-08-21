module Database.Servant.Lancer where

import Operators
import Database.Model

lancers ∷ Array Servant
lancers = Servant ↤
[ { name:     "Scathach"
  , rarity:   5
  , class:    Lancer
  , attr:     Star
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1758,  hp: 2174 }
              , max:   { atk: 11375, hp: 14825 }
              , grail: { atk: 12452, hp: 16241 }
              }
  , ratings:  { damage:5, np:3, critical:4, utility:3, support:3, durability:4 }
  , actives:  [ { name:   "Wisdom of Dun Scaith A+"
                , icon:   IconDodge
                , cd:     5
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Chance 80 Self 3 CritUp 50.0
                          , Chance 80 Self 3 StarAbsorb 500.0
                          ]
                }
              , { name:   "Primordial Rune"
                , icon:   IconQuickUp
                , cd:     6
                , effect: [ Grant Ally 1 QuickUp 50.0 ]
                }
              , { name:   "God-Slayer B"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 1 (DamageUpVs Divine) 100.0 
                          , Grant Self 1 (DamageUpVs Undead)  100.0
                          ] 
                }
              ]
  , passives: [magicResistance:A]
  , phantasm: { name:   "Gae Bolg Alternative"
              , desc:   "Soaring Spear of Piercing Death"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage 2400.0  
                        , Debuff Enemy 1 Stun 100.0
                        ]
              , over:   [ To Enemy InstantKill 50.0 ]
              }
  , gen:      { starAbsorb: 88, starGen: 12.2, npPerHit: 0.71, npAttacked: 4 }
  , hits:     { a: 3, b: 6, q: 2, ex: 7 }
  , traits:   [Female, EnumaElish, King]
  , death:    32.0
  , align:    Neutral:Good
  }
, { name:     "Karna"
  , rarity:   5
  , class:    Lancer
  , attr:     Sky
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1850,  hp: 1999 }
              , max:   { atk: 11976, hp: 13632 }
              , grail: { atk: 13110, hp: 14934 }
              }
  , ratings:  { damage:4, np:4, critical:4, utility:4, support:3, durability:2 }
  , actives:  [ { name:   "Knowledge of the Deprived A"
                , icon:   IconCircuits
                , cd:     6
                , effect: [ Debuff Enemy 1 SealNP 100.0 
                          , Debuff Enemy 1 DebuffVuln 50.0
                          ]
                }
              , { name:   "Mana Burst (Flame) A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 30.0 
                          , Grant Self 1 NPUp 20.0
                          ]
                }
              , { name:   "Uncrowned Arms Mastery"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 25.0 
                          , Grant Self 3 StarGen 50.0
                          , Grant Self 3 CritUp 40.0
                          ]
                }
              ]
  , passives: [magicResistance:C, divinity:A, riding:A]
  , phantasm: { name:   "Vasavi Shakti"
              , desc:   "O' Sun, Abide to Death"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Divine"
              , hits:   5
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ To (EnemyType Divine) Damage 150.0 ]
              }
  , gen:      { starAbsorb: 88, starGen: 12.2, npPerHit: 0.72, npAttacked: 4 }
  , hits:     { a: 3, b: 1, q: 3, ex: 4 }
  , traits:   [Male, Riding, Brynhildr, Divine, EnumaElish, HeavenOrEarth]
  , death:    28.0
  , align:    Lawful:Good
  }
, { name:     "Tamamo no Mae (Lancer)"
  , rarity:   5
  , class:    Lancer
  , attr:     Sky
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1657,  hp: 2221 }
              , max:   { atk: 10726, hp: 15147 }
              , grail: { atk: 11741, hp: 16594 }
              }
  , ratings:  { damage:4, np:5, critical:4, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Beach Flower EX"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 10.0 
                          , Grant (AllyType Male) 3 StarGen 42.0
                          ]
                }
              , { name:   "Midsummer Curse A"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Debuff Enemy 1 Charm 100.0 
                          , Debuff Enemy 3 DefenseDown 30.0
                          , Debuff Enemy 5 Curse 1000.0
                          , To Enemy DemeritCharge 1.0
                          ]
                }
              , { name:   "Goddess Morph B"
                , icon:   IconShield
                , cd:     6
                , effect: [ Grant Self 1 Invincibility 100.0 
                          , Grant Self 1 CritUp 50.0
                          , Grant Self 1 StarGen 50.0
                          , Grant Self 1 NPGen 50.0
                          , Grant Self 1 DebuffResist 50.0
                          , Grant Self 1 HealingReceived 50.0
                          , To Self DemeritStun 1.0
                          ]
                }
              ]
  , passives: [riding:A, territoryCreation:A, divinity:APlusPlus]
  , phantasm: { name:   "Tokonatsu Nikkou—Goddess' Love Parasol"
              , desc:   "Everlasting Summer Sunlight—Hiyoke Kasa Chouai I-Shin"
              , rank:   C
              , card:   Buster
              , kind:   "Anti-Unit"
              , hits:   4
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ To (EnemyType Male) Damage 150.0 ]
              }
  , gen:      { starAbsorb: 91, starGen: 12.2, npPerHit: 1.05, npAttacked: 4 }
  , hits:     { a: 2, b: 3, q: 4, ex: 4 }
  , traits:   [Female, Divine, EarthOrSky, Riding, EnumaElish]
  , death:    40.0
  , align:    Neutral:Summer
  }
, { name:     "Brynhild"
  , rarity:   5
  , class:    Lancer
  , attr:     Sky
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1766,  hp: 2174 }
              , max:   { atk: 11432, hp: 14825 }
              , grail: { atk: 12514, hp: 16241 }
              }
  , ratings:  { damage:4, np:4, critical:4, utility:3, support:4, durability:3 }
  , actives:  [ { name:   "Mana Burst (Flame) B"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 25.0 
                          , Grant Self 1 NPUp 15.0
                          ]
                }
              , { name:   "Primordial Rune"
                , icon:   IconExclamationDown
                , cd:     6
                , effect: [ Debuff Enemy 3 CritDown 50.0 
                          , Debuff Enemy 1 NPDown 30.0
                          ]
                }
              , { name:   "Hero's Assistant C"
                , icon:   IconStarUp
                , cd:     5
                , effect: [ Grant Ally 3 StarAbsorb 600.0 
                          , To Ally Heal 1000.0
                          ]
                }
              ]
  , passives: [magicResistance:B, riding:A, divinity:E]
  , phantasm: { name:   "Brynhild Romantia"
              , desc:   "Until Death Divide the Two Apart"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemy Damage 1000.0
                        , Grant Allies 3 StarDrop 30.0
                        ]
              , over:   [ To (EnemyType Brynhildr) Damage 150.0 ]
              }
  , gen:      { starAbsorb: 87, starGen: 12.2, npPerHit: 1.07, npAttacked: 4 }
  , hits:     { a: 2, b: 1, q: 3, ex: 5 }
  , traits:   [Riding, Divine, EnumaElish, HeavenOrEarth]
  , death:    32.0
  , align:    Neutral:Good
  }
]
