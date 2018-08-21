module Database.Servant.Archer where

import Operators
import Database.Model

archers ∷ Array Servant
archers = Servant ↤
[ { name:     "Gilgamesh"
  , rarity:   5
  , class:    Archer
  , attr:     Sky
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1897,  hp: 1920 }
              , max:   { atk: 12280, hp: 13097 }
              , grail: { atk: 13442, hp: 14348 }
              }
  , ratings:  { damage:5, np:4, critical:5, utility:3, support:3, durability:2 }
  , actives:  [ { name:   "Charisma A+"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 21.0 ]
                }
              , { name:   "Golden Rule A"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 50.0 ]
                }
              , { name:   "Collector EX"
                , icon:   IconStarUp
                , cd:     5
                , effect: [ Grant Self 3 StarAbsorb 600.0 ]
                }
              ]
  , passives: [magicResistance:E, independentAction:APlus, divinity:B]
  , phantasm: { name:   "Enuma Elish"
              , desc:   "The Star of Creation that Split Heaven and Earth"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-World"
              , hits:   1
              , effect: [ Grant Self 1 NPUp 30.0
                        , To Enemies Damage 600.0 
                        ]
              , over:   [ To (EnemyType EnumaElish) Damage 150.0 ]
              }
  , gen:      { starAbsorb: 153, starGen: 7.9, npPerHit: 0.34, npAttacked: 3 }
  , hits:     { a: 5, b: 5, q: 5, ex: 8 }
  , traits:   [Male, Divine, EnumaElish, HeavenOrEarth]
  , death:    31.5
  , align:    Chaotic:Good
  }
, { name:     "Altria Pendragon (Archer)"
  , rarity:   5
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1742,  hp: 2134 }
              , max:   { atk: 11276, hp: 14553 }
              , grail: { atk: 12343, hp: 15943 }
              }
  , ratings:  { damage:4, np:5, critical:3, utility:3, support:3, durability:3 }
  , actives:  [ { name:   "Summer Splash! A+"
                , icon:   IconArtsUp
                , cd:     6
                , effect: [ Grant Self 3 ArtsUp 30.0 
                          , Grant Party 3 DefenseUp 20.0 
                          ]
                }
              , { name:   "Beach House Protection EX"
                , icon:   IconHeal
                , cd:     4
                , effect: [ To Self Heal 5000.0 
                          , To Self DemeritGauge 10.0
                          ]
                }
              , { name:   "Beach Flower B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 18.0 
                          , Grant (AllyType Male) 3 StarGen 38.0
                          ]
                }
              ]
  , passives: [magicResistance:A, independentAction:A, territoryCreation:A]
  , phantasm: { name:   "Excalibur Viviane"
              , desc:   "Sword of Sunlight-Glitter Victory"
              , rank:   A
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   10
              , effect: [ To Enemy Damage 1500.0 
                        , To Enemy GaugeDown 70.0
                        ]
              , over:   [ To Self GaugeUp 20.0 ]
              }
  , gen:      { starAbsorb: 153, starGen: 8.0, npPerHit: 0.59, npAttacked: 3 }
  , hits:     { a: 3, b: 3, q: 4, ex: 5 }
  , traits:   [Female, Dragon, EarthOrSky, Saberface, EnumaElish]
  , death:    25.8
  , align:    Lawful:Good
  }
, { name:     "Nikola Tesla"
  , rarity:   5
  , class:    Archer
  , attr:     Star
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1820,  hp: 2027 }
              , max:   { atk: 11781, hp: 13825 }
              , grail: { atk: 12896, hp: 15146 }
              }
  , ratings:  { damage:5, np:4, critical:4, utility:1, support:1, durability:3 }
  , actives:  [ { name:   "Galvanism A"
                , icon:   IconNobleUp
                , cd:     5
                , effect: [ Grant Self 3 NPGen 50.0 ]
                }
              , { name:   "Inherent Wisdom A"
                , icon:   IconGuts
                , cd:     5
                , effect: [ Grant Self 3 Guts 1.0 
                          , Chance 80 Self 3 DefenseUp 30.0
                          , Chance 80 Self 1 NPUp 30.0
                          ]
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
  , passives: [magicResistance:C, independentAction:B]
  , phantasm: { name:   "System Keraunos"
              , desc:   "Legend of Mankind—Advent of Lightning EX"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   3
              , effect: [ To Enemies Damage 600.0 
                        , Debuff Enemies 1 StunChance 40.0
                        , To Self DemeritHealth 500.0
                        ]
              , over:   [ To (EnemyType EarthOrSky) Damage 150.0 ]
              }
  , gen:      { starAbsorb: 147, starGen: 7.9, npPerHit: 0.87, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 3 }
  , traits:   [Male, Brynhildr]
  , death:    31.5
  , align:    Chaotic:Good
  }
, { name:     "Orion"
  , rarity:   5
  , class:    Archer
  , attr:     Sky
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1716,  hp: 2134 }
              , max:   { atk: 11107, hp: 14553 }
              , grail: { atk: 12158, hp: 15943 }
              }
  , ratings:  { damage:4, np:4, critical:4, utility:3, support:2, durability:5 }
  , actives:  [ { name:   "Grace of the Goddess EX"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 1 DefenseUp 50.0 
                          , Grant Self 3 AttackUp 20.0
                          , Grant Self 3 DebuffResist 50.0
                          ]
                }
              , { name:   "Punish the Unfaithful A+"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 1 (DamageUpVs Male) 100.0 ]
                }
              , { name:   "Mind's Eye (Fake) B-"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 CritUp 34.0
                          ]
                }
              ]
  , passives: [magicResistance:D, independentAction:APlus]
  , phantasm: { name:   "Tri-Star Amore Mio"
              , desc:   "Moon Goddess's Arrows of Love and Romance"
              , rank:   APlus
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemy Damage 1800.0 
                        , Debuff Enemy 3 AttackDown 20.0
                        , To Enemy GaugeDown 1.0
                        ]
              , over:   [ Debuff Enemy 3 CritDown 20.0 ]
              }
  , gen:      { starAbsorb: 153, starGen: 8.0, npPerHit: 1.0, npAttacked: 3 }
  , hits:     { a: 1, b: 1, q: 3, ex: 4 }
  , traits:   [Male, EnumaElish, HeavenOrEarth]
  , death:    27.0
  , align:    Chaotic:Neutral
  }
]
