module Database.Servant.Berserker where

import Operators
import Database.Model

berserkers ∷ Array Servant
berserkers = Servant ↤
[ { name:     "Cu Chulainn (Alter)"
  , rarity:   5
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1979,  hp: 1790 }
              , max:   { atk: 12805, hp: 12210 }
              , grail: { atk: 14017, hp: 13377 }
              }
  , ratings:  { damage:5, np:3, critical:1, utility:4, support:2, durability:3 }
  , actives:  [ { name:   "Madness of the Spirits A"
                , icon:   IconExclamationDown
                , cd:     6
                , effect: [ Debuff Enemies 3 AttackDown 20.0 
                          , Debuff Enemies 3 CritDown 50.0
                          ]
                }
              , { name:   "Protection from Arrows C"
                , icon:   IconDodge
                , cd:     5
                , effect: [ Grant Self 0 Evasion 2.0 
                          , Grant Self 3 DefenseUp 14.0
                          ]
                }
              , { name:   "Battle Continuation A"
                , icon:   IconGuts
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              ]
  , passives: [madness:C, divinity:C]
  , phantasm: { name:   "Curruid Coinchenn"
              , desc:   "Beast of Crunching Death Fangs"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   12
              , effect: [ To Enemy DamageThruDef 1000.0 ]
              , over:   [ Grant Self 1 AttackUp 30.0 
                        , Grant Self 1 DefenseUp 30.0
                        ]
              }
  , gen:      { starAbsorb: 9, starGen: 5.1, npPerHit: 0.69, npAttacked: 5 }
  , hits:     { a: 3, b: 3, q: 4, ex: 5 }
  , traits:   [Male, Divine, Brynhildr, EnumaElish, HeavenOrEarth]
  , death:    52.0
  , align:    Chaotic:Evil
  }
, { name:     "Minamoto-no-Raikou"
  , rarity:   5
  , class:    Berserker
  , attr:     Sky
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1786,  hp: 1980 }
              , max:   { atk: 11556, hp: 13500 }
              , grail: { atk: 12650, hp: 14790 }
              }
  , ratings:  { damage:5, np:3, critical:5, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Eternal Arms Mastery A+"
                , icon:   IconStarUp
                , cd:     6
                , effect: [ Grant Self 3 StarAbsorb 6000.0 ]
                }
              , { name:   "Mana Burst (Lightning) A"
                , icon:   IconBusterUp
                , cd:     6
                , effect: [ Grant Self 1 BusterUp 30.0 
                          , Grant Self 1 Evasion 0.0
                          ]
                }
              , { name:   "Mystic Slayer A"
                , icon:   IconDamageUp
                , cd:     6
                , effect: [ Grant Self 3 (DamageUpVs Demonic) 50.0 
                          , Grant Self 3 (DamageUpVs EarthOrSky) 50.0
                          ]
                }
              ]
  , passives: [magicResistance:D, madness:EX, riding:APlus, divinity:C]
  , phantasm: { name:   "Vengeful Lightning of the Ox-King"
              , desc:   "Goou Shourai—Tenmoukaikai"
              , rank:   BPlusPlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   7
              , effect: [ To Enemies Damage 300.0 ]
              , over:   [ Grant Self 1 StarGen 100.0 ]
              }
  , gen:      { starAbsorb: 9, starGen: 4.9, npPerHit: 0.46, npAttacked: 5 }
  , hits:     { a: 4, b: 1, q: 3, ex: 5 }
  , traits:   [Female, Divine, EarthOrSky, Riding, EnumaElish]
  , death:    39.0
  , align:    Chaotic:Good
  }
, { name:     "Sakata Kintoki"
  , rarity:   5
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1964,  hp: 1782 }
              , max:   { atk: 12712, hp: 12150 }
              , grail: { atk: 13915, hp: 13311 }
              }
  , ratings:  { damage:5, np:4, critical:1, utility:3, support:1, durability:2 }
  , actives:  [ { name:   "Monstrous Strength A+"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 1 AttackUp 50.0 ]
                }
              , { name:   "Animal Communication C"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0 ]
                }
              , { name:   "Natural Body A"
                , icon:   IconHoodUp
                , cd:     5
                , effect: [ Grant Self 3 DebuffResist 120.0 
                          , To Self Heal 3000.0
                          ]
                }
              ]
  , passives: [madness:E, divinity:D]
  , phantasm: { name:   "Golden Spark"
              , desc:   "Golden Impact"
              , rank:   CMinus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy DamageThruDef 1000.0 ]
              , over:   [ Debuff Enemy 1 StunChance 50.0 ]
              }
  , gen:      { starAbsorb: 9, starGen: 5.0, npPerHit: 1.03, npAttacked: 5 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Divine, Brynhildr, EnumaElish]
  , death:    52.0
  , align:    Lawful:Good
  }
, { name:     "Vlad III"
  , rarity:   5
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1777,  hp: 2019 }
              , max:   { atk: 11499, hp: 13770 }
              , grail: { atk: 12587, hp: 15086 }
              }
  , ratings:  { damage:4, np:2, critical:3, utility:2, support:2, durability:3 }
  , actives:  [ { name:   "Vampirism A"
                , icon:   IconDarkMagic
                , cd:     6
                , effect: [ To Enemy Drain 100.0 ]
                }
              , { name:   "Morph C"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 24.0 ]
                }
              , { name:   "Battle Continuation A"
                , icon:   IconGuts
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              ]
  , passives: [madness:EX]
  , phantasm: { name:   "Kazikli Bey"
              , desc:   "Bloodstained Demon King"
              , rank:   CPlus
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   10
              , effect: [ To Enemy Damage 1500.0 ]
              , over:   [ To Party GainStars 20.0 ]
              }
  , gen:      { starAbsorb: 9, starGen: 4.9, npPerHit: 0.5, npAttacked: 5 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhildr, HeavenOrEarth]
  , death:    45.5
  , align:    Lawful:Evil
  }
, { name:     "Nightingale"
  , rarity:   5
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1573,  hp: 2232 }
              , max:   { atk: 10184, hp: 15221 }
              , grail: { atk: 11148, hp: 16675 }
              }
  , ratings:  { damage:3, np:3, critical:1, utility:3, support:5, durability:2 }
  , actives:  [ { name:   "Nurse of Steel A"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Ally Heal 4000.0 ]
                }
              , { name:   "Understanding of the Human Body A"
                , icon:   IconDamageUp
                , cd:     6
                , effect: [ Grant Self 3 (DamageUpVs Humanoid) 50.0 
                          , Grant Self 3 (DefenseUpVs Humanoid) 25.0
                          ]
                }
              , { name:   "Angel's Cry EX"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Ally 3 BusterUp 50.0 ]
                }
              ]
  , passives: [madness:EX]
  , phantasm: { name:   "Nightingale Pledge"
              , desc:   "I Will Abstain From Whatever Is Deleterious And Mischievous"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ To Party RemoveBuffs 100.0 
                        , To Party Heal 5000.0
                        ]
              , over:   [ Debuff Enemies 1 NPDown 50.0 ]
              }
  , gen:      { starAbsorb: 10, starGen: 5.0, npPerHit: 0.77, npAttacked: 5 }
  , hits:     { a: 2, b: 1, q: 6, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    56.8
  , align:    Lawful:Good
  }
]
