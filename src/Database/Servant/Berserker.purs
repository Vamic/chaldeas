module Database.Servant.Berserker where

import Prelude
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
                          , Debuff Enemies 3 CritChance 50.0
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
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              ]
  , passives: [madness C, divinity C]
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
  , gen:      { starWeight: 9, starRate: 5.1, npPerHit: 0.69, npPerDefend: 5 }
  , hits:     { a: 3, b: 3, q: 4, ex: 5 }
  , traits:   [Male, Divine, Brynhildr, EnumaElish]
  , death:    52.0
  , align:    Chaotic:Evil
  }
, { name:     "Minamoto-no-Raikou"
  , rarity:   5
  , class:    Berserker
  , attr:     Heaven
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
                , effect: [ Grant Self 3 (AttackUpVs Demonic) 50.0 
                          , Grant Self 3 (AttackUpVs HeavenOrEarth) 50.0
                          ]
                }
              ]
  , passives: [magicResistance D, madness EX, riding APlus, divinity C]
  , phantasm: { name:   "Vengeful Lightning of the Ox-King"
              , desc:   "Goou Shourai—Tenmoukaikai"
              , rank:   BPlusPlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   7
              , effect: [ To Enemies Damage 300.0 ]
              , over:   [ Grant Self 1 StarUp 100.0 ]
              }
  , gen:      { starWeight: 9, starRate: 4.9, npPerHit: 0.46, npPerDefend: 5 }
  , hits:     { a: 4, b: 1, q: 3, ex: 5 }
  , traits:   [Female, Divine, Riding, EnumaElish]
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
  , passives: [madness E, divinity D]
  , phantasm: { name:   "Golden Spark"
              , desc:   "Golden Impact"
              , rank:   CMinus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy DamageThruDef 1000.0 ]
              , over:   [ Chance 50 $ Debuff Enemy 1 Stun 0.0 ]
              }
  , gen:      { starWeight: 9, starRate: 5.0, npPerHit: 1.03, npPerDefend: 5 }
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
                , effect: [ Chance 100 $ To Enemy GaugeDown 1.0 ]
                }
              , { name:   "Morph C"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 24.0 ]
                }
              , { name:   "Battle Continuation A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              ]
  , passives: [madness EX]
  , phantasm: { name:   "Kazikli Bey"
              , desc:   "Bloodstained Demon King"
              , rank:   CPlus
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   10
              , effect: [ To Enemy Damage 1500.0 ]
              , over:   [ To Party GainStars 20.0 ]
              }
  , gen:      { starWeight: 9, starRate: 4.9, npPerHit: 0.5, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhildr]
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
                , effect: [ Grant Self 3 (AttackUpVs Humanoid) 50.0 
                          , Grant Self 3 (DefenseUpVs Humanoid) 25.0
                          ]
                }
              , { name:   "Angel's Cry EX"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Ally 3 BusterUp 50.0 ]
                }
              ]
  , passives: [madness EX]
  , phantasm: { name:   "Nightingale Pledge"
              , desc:   "I Will Abstain From Whatever Is Deleterious And Mischievous"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ To Party RemoveBuffs 0.0 
                        , To Party Heal 5000.0
                        ]
              , over:   [ Debuff Enemies 1 NPDown 50.0 ]
              }
  , gen:      { starWeight: 10, starRate: 5.0, npPerHit: 0.77, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 6, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    56.8
  , align:    Lawful:Good
  }
, { name:     "Heracles"
  , rarity:   4
  , class:    Berserker
  , attr:     Heaven
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1775,  hp: 1652 }
              , max:   { atk: 10655, hp: 10327 }
              , grail: { atk: 12901, hp: 12521 }
              }
  , ratings:  { damage:5, np:3, critical:2, utility:2, support:1, durability:4 }
  , actives:  [ { name:   "Valor A+"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 31.0 
                          , Grant Self 3 DebuffResist 42.0
                          ]
                }
              , { name:   "Mind's Eye (Fake) B"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 CritUp 36.0
                          ]
                }
              , { name:   "Battle Continuation A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              ]
  , passives: [madness B, divinity A]
  , phantasm: { name:   "Nine Lives"
              , desc:   "Shooting Down a Hundred Heads"
              , rank:   APlus
              , card:   Buster
              , kind:   "Unknown"
              , hits:   15
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ Debuff Enemy 3 DefenseDown 10.0 ]
              }
  , gen:      { starWeight: 10, starRate: 5.0, npPerHit: 1.07, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Divine, EnumaElish]
  , death:    39.0
  , align:    Chaotic:Mad
  }
, { name:     "Frankenstein"
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1573,  hp: 1710 }
              , max:   { atk: 9441, hp: 10687 }
              , grail: { atk: 11431, hp: 12958 }
              }
  , ratings:  { damage:4, np:4, critical:2, utility:3, support:2, durability:2 }
  , actives:  [ { name:   "Galvanism B"
                , icon:   IconNobleUp
                , cd:     5
                , effect: [ Grant Self 3 NPGen 45.0 ]
                }
              , { name:   "Wail of the Living Dead C"
                , icon:   IconStun
                , cd:     6
                , effect: [ Chance 60 $ Debuff Enemy 1 Stun 0.0 ]
                }
              , { name:   "Overload C"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Self 1 NPUp 30.0 
                          , Debuff Self 5 Burn 300.0
                          ]
                }
              ]
  , passives: [madness D]
  , phantasm: { name:   "Blasted Tree"
              , desc:   "Lightning Tree of Crucifixion"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage 1300.0 
                        , Debuff Self 2 Stun 0.0
                        ]
              , over:   [ Debuff Enemies 3 CritChance 20.0 ]
              }
  , gen:      { starWeight: 10, starRate: 4.9, npPerHit: 0.83, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    58.5
  , align:    Chaotic:Neutral
  }
, { name:     "Ibaraki-Douji"
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1606,  hp: 1752 }
              , max:   { atk: 9636, hp: 10954 }
              , grail: { atk: 11667, hp: 13282 }
              }
  , ratings:  { damage:5, np:2, critical:2, utility:3, support:1, durability:4 }
  , actives:  [ { name:   "Demonic Nature of Oni A"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 20.0
                          , Grant Self 3 NPUp 30.0
                          ]
                }
              , { name:   "Disengage A"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Self RemoveDebuffs 0.0
                          , To Self Heal 2500.0
                          ]
                }
              , { name:   "Morph A"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 30.0 
                          , Grant Self 1 DefenseUp 30.0
                          ]
                }
              ]
  , passives: [madness B]
  , phantasm: { name:   "Great Grudge of Rashomon"
              , desc:   "Rashomon Daiengi"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemy Damage 1000.0 
                        , To Enemy RemoveBuffs 0.0
                        ]
              , over:   [ Debuff Enemy 3 DefenseDown 10.0 ]
              }
  , gen:      { starWeight: 10, starRate: 4.9, npPerHit: 1.03, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 4, ex: 5 }
  , traits:   [Female, Magical, EnumaElish]
  , death:    52.0
  , align:    Chaotic:Evil
  }
, { name:     "Lancelot"
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1746,  hp: 1652 }
              , max:   { atk: 10477, hp: 10327 }
              , grail: { atk: 12685, hp: 12521 }
              }
  , ratings:  { damage:4, np:1, critical:4, utility:1, support:1, durability:1 }
  , actives:  [ { name:   "Eternal Arms Mastery A+"
                , icon:   IconStarUp
                , cd:     5
                , effect: [ Grant Self 3 StarAbsorb 6000.0 ]
                }
              , { name:   "Protection of the Spirits A"
                , icon:   IconStarHaloUp
                , cd:     5
                , effect: [ Grant Self 3 StarUp 30.0 ]
                }
              , { name:   "Mana Reversal A"
                , icon:   IconNobleUp
                , cd:     5
                , effect: [ Grant Self 1 NPGen 100.0 
                          , Grant Self 3 CritUp 50.0
                          ]
                }
              ]
  , passives: [magicResistance E, madness C]
  , phantasm: { name:   "Knight of Owner"
              , desc:   "A Knight Does Not Die Empty-Handed"
              , rank:   APlusPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   10
              , effect: [ To Enemies Damage 1000.0 ]
              , over:   [ Grant Self 3 AttackUp 30.0 ]
              }
  , gen:      { starWeight: 10, starRate: 5.0, npPerHit: 0.5, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    52.0
  , align:    Lawful:Mad
  }
, { name:     "Beowulf"
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1707,  hp: 1652 }
              , max:   { atk: 10247, hp: 10327 }
              , grail: { atk: 12407, hp: 12521 }
              }
  , ratings:  { damage:5, np:2, critical:2, utility:1, support:1, durability:2 }
  , actives:  [ { name:   "Berserk A"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 1 AttackUp 30.0 
                          , Grant Self 1 NPUp 20.0
                          ]
                }
              , { name:   "Intuition B"
                , icon:   IconStar
                , cd:     5
                , effect: [ To Party GainStars 14.0 ]
                }
              , { name:   "Battle Continuation B"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 4 Guts 1.0 ]
                }
              ]
  , passives: [madness EMinus]
  , phantasm: { name:   "Grendel Buster"
              , desc:   "Primal Conflict"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   12
              , effect: [ Grant Self 1 SureHit 0.0 
                        , To Enemy Damage 1100.0 
                        ]
              , over:   [ Debuff Enemy 3 CritChance 30.0 ]
              }
  , gen:      { starWeight: 10, starRate: 4.9, npPerHit: 0.68, npPerDefend: 5 }
  , hits:     { a: 3, b: 1, q: 3, ex: 4 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    58.5
  , align:    Chaotic:Good
  }
, { name:     "Tamamo Cat"
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1504,  hp: 1833 }
              , max:   { atk: 9026, hp: 11458 }
              , grail: { atk: 10929, hp: 13893 }
              }
  , ratings:  { damage:4, np:2, critical:3, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Monstrous Strength B"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 2 AttackUp 30.0 ]
                }
              , { name:   "Curse E"
                , icon:   IconDarkMagic
                , cd:     5
                , effect: [ Chance 40 $ To Enemy GaugeDown 1.0 ]
                }
              , { name:   "Morph B"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 27.0 ]
                }
              ]
  , passives: [madness C]
  , phantasm: { name:   "Napping in the Dazzling Sunshine and Feasting"
              , desc:   "Sansan Nikkou Hiruyasumi Shuchi Nikurin"  
              , rank:   D
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemies Damage 1000.0 
                        , Debuff Self 2 Stun 0.0
                        ]
              , over:   [ Grant Self 3 HealPerTurn 2000.0 ]
              }
  , gen:      { starWeight: 10, starRate: 5.0, npPerHit: 0.71, npPerDefend: 5 }
  , hits:     { a: 3, b: 2, q: 2, ex: 3 }
  , traits:   [Female, Beast, EnumaElish]
  , death:    39.0
  , align:    Chaotic:Good
  }
, { name:     "Lu Bu Fengxian"
  , rarity:   3
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1507,  hp: 1494 }
              , max:   { atk: 8119, hp: 8302 }
              , grail: { atk: 10988, hp: 11256 }
              }
  , ratings:  { damage:5, np:2, critical:2, utility:2, support:0, durability:2 }
  , actives:  [ { name:   "Valor B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 27.0 
                          , Grant Self 3 MentalResist 36.0
                          ]
                }
              , { name:   "Defiant B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 25.0 
                          , Debuff Self 3 BuffFail 50.0
                          ]
                }
              , { name:   "Chaotic Villain A"
                , icon:   IconBeamUp
                , cd:     6
                , effect: [ Grant Self 1 NPUp 30.0 
                          , Grant Self 1 StarAbsorb 3000.0
                          , Debuff Others 1 DefenseDown 20.0
                          ]
                }
              ]
  , passives: [madness A]
  , phantasm: { name:   "God Force"
              , desc:   "Five Weapons of the War God"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel/Anti-Army/Anti-Fortress"
              , hits:   1
              , effect: [ To Enemy DamageThruDef 1000.0 ]
              , over:   [ Chance 30 $ Debuff Enemy 1 Stun 0.0 ]
              }
  , gen:      { starWeight: 9, starRate: 5.0, npPerHit: 1.04, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    50.3
  , align:    Chaotic:Evil
  }
, { name:     "Spartacus"
  , rarity:   1
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 922,  hp: 1544 }
              , max:   { atk: 5073, hp: 7722 }
              , grail: { atk: 7883, hp: 11904 }
              }
  , ratings:  { damage:3, np:3, critical:1, utility:2, support:1, durability:3 }
  , actives:  [ { name:   "Honor of Suffering B+"
                , icon:   IconHealTurn
                , cd:     7
                , effect: [ Grant Self 5 HealPerTurn 1500.0 ]
                }
              , { name:   "Unyielding Will A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              , { name:   "Triumphant Return of the Sword B"
                , icon:   IconBusterUp
                , cd:     6
                , effect: [ Grant Self 1 BusterUp 40.0 
                          , To Self Heal 2000.0
                          ]
                }
              ]
  , passives: [madness EX]
  , phantasm: { name:   "Crying Warmonger"
              , desc:   "Howl of the Wounded Beast"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemies DamageThruDef 500.0 ]
              , over:   [ To Self Heal 3000.0 ]
              }
  , gen:      { starWeight: 9, starRate: 4.9, npPerHit: 1.01, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Roman, EnumaElish]
  , death:    65.0
  , align:    Neutral:Neutral
  }
, { name:     "Asterios"
  , rarity:   1
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1097,  hp: 1320 }
              , max:   { atk: 6037, hp: 6604 }
              , grail: { atk: 9381, hp: 10181 }
              }
  , ratings:  { damage:3, np:2, critical:1, utility:3, support:4, durability:3 }
  , actives:  [ { name:   "Monstrous Strength A"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 30.0 ]
                }
              , { name:   "Natural Demon A++"
                , icon:   IconHoodUp
                , cd:     5
                , effect: [ Grant Self 3 DebuffResist 100.0 
                          , Grant Self 3 DefenseUp 40.0
                          ]
                }
              , { name:   "Labrys of the Abyss C"
                , icon:   IconStarUp
                , cd:     5
                , effect: [ Grant Self 1 StarAbsorb 6000.0
                          , Grant Self 1 BusterUp 30.0
                          ]
                }
              ]
  , passives: [madness B]
  , phantasm: { name:   "Chaos Labyrinth"
              , desc:   "Eternally Unchanging Labyrinth"
              , rank:   EX
              , card:   Arts
              , kind:   "Maze"
              , hits:   0
              , effect: [ Debuff Enemies 6 AttackDown 20.0 
                        , Debuff Enemies 1 AttackDown 40.0
                        , Debuff Enemies 1 DefenseDown 40.0
                        ]
              , over:   [ Debuff Enemies 6 DefenseDown 10.0 ]
              }
  , gen:      { starWeight: 9, starRate: 4.9, npPerHit: 0.68, npPerDefend: 5 }
  , hits:     { a: 3, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    58.5
  , align:    Chaotic:Evil
  }
, { name:     "Kiyohime"
  , rarity:   3
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1233,  hp: 1649 }
              , max:   { atk: 6644, hp: 9166 }
              , grail: { atk: 8992, hp: 12428 }
              }
  , ratings:  { damage:4, np:4, critical:1, utility:1, support:2, durability:2 }
  , actives:  [ { name:   "Morph C"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 24.0 ]
                }
              , { name:   "Stalking B"
                , icon:   IconShieldDown
                , cd:     5
                , effect: [ Debuff Enemy 4 DefenseDown 24.0 
                          , Grant Enemy 3 AttackUp 20.0
                          ]
                }
              , { name:   "Flame-Colored Kiss A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 30.0 
                          , To Self RemoveDebuffs 0.0
                          ]
                }
              ]
  , passives: [madness EX]
  , phantasm: { name:   "Transforming, Flame-Emitting Meditation"
              , desc:   "Tenshin Kashou Zanmai"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   3
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Chance 50 $ Debuff Enemies 1 Stun 0.0 
                        , Debuff Enemies 10 Burn 500.0
                        ]
              }
  , gen:      { starWeight: 9, starRate: 4.9, npPerHit: 2.03, npPerDefend: 5 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Dragon, EnumaElish]
  , death:    65.0
  , align:    Chaotic:Evil
  }
, { name:     "Eric Bloodaxe"
  , rarity:   2
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1116,  hp: 1447 }
              , max:   { atk: 6290, hp: 7688 }
              , grail: { atk: 9115, hp: 11095 }
              }
  , ratings:  { damage:4, np:3, critical:1, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Supporting Curse C+"
                , icon:   IconSwordDown
                , cd:     5
                , effect: [ Debuff Enemy 2 AttackDown 15.0 
                          , Debuff Enemy 2 DefenseDown 30.0
                          ]
                }
              , { name:   "Battle Continuation B"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 4 Guts 1.0 ]
                }
              , { name:   "Half-Dead Bloodaxe A+"
                , icon:   IconBubbles
                , cd:     6
                , effect: [ To Self RemoveDebuffs 0.0 
                          , Grant Self 3 MaxHP 3000.0
                          ]
                }
              ]
  , passives: [madness B]
  , phantasm: { name:   "Bloodbath Crown"
              , desc:   "Bloodstained Coronation"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemies Damage 500.0 
                        , To Self DemeritDamage 1000.0
                        ]
              , over:   [ Grant Self 1 AttackUp 50.0 ]
              }
  , gen:      { starWeight: 9, starRate: 4.9, npPerHit: 1.02, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    58.5
  , align:    Chaotic:Neutral
  }
, { name:     "Darius III"
  , rarity:   3
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1412,  hp: 1577 }
              , max:   { atk: 7608, hp: 8763 }
              , grail: { atk: 10297, hp: 11881 }
              }
  , ratings:  { damage:4, np:3, critical:2, utility:2, support:2, durability:3 }
  , actives:  [ { name:   "Golden Rule B"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 45.0 ]
                }
              , { name:   "Disengage A"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Self RemoveDebuffs 0.0 
                          , To Self Heal 2500.0
                          ]
                }
              , { name:   "Battle Continuation A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              ]
  , passives: [madness B]
  , phantasm: { name:   "Athanaton Ten Thousand"
              , desc:   "Ten Thousand Immortals"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   10
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Debuff Enemies 3 AttackDown 10.0 
                        , Debuff Enemies 3 DefenseDown 10.0
                        ]
              }
  , gen:      { starWeight: 9, starRate: 5.0, npPerHit: 0.67, npPerDefend: 5 }
  , hits:     { a: 3, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    65.0
  , align:    Lawful:Neutral
  }
, { name:     "Caligula"
  , rarity:   2
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1211,  hp: 1374 }
              , max:   { atk: 6831, hp: 7303 }
              , grail: { atk: 9899, hp: 10540 }
              }
  , ratings:  { damage:3, np:2, critical:1, utility:3, support:3, durability:2 }
  , actives:  [ { name:   "Sadistic Streak A"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 30.0 
                          , Debuff Self 3 DefenseDown 10.0
                          ]
                }
              , { name:   "Imperial Privilege A"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Self Heal 3000.0 
                          , Chance 60 $ Grant Self 3 AttackUp 40.0
                          , Chance 60 $ Grant Self 3 DefenseUp 40.0
                          ]
                }
              , { name:   "Glory of Past Days B"
                , icon:   IconBusterUp
                , cd:     3
                , effect: [ Grant Self 1 BusterUp 50.0 
                          , To Self DemeritHealth 500.0
                          ]
                }
              ]
  , passives: [madness APlus]
  , phantasm: { name:   "Flucticulus Diana"
              , desc:   "Moonlight, Devour my Soul"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Chance 150 $ Debuff Enemies 3 SealSkills 0.0 ]
              , over:   [ Chance 70 $ Debuff Enemies 3 SealNP 0.0 ]
              }
  , gen:      { starWeight: 9, starRate: 5.0, npPerHit: 0.68, npPerDefend: 5 }
  , hits:     { a: 3, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Roman, EnumaElish]
  , death:    56.8
  , align:    Chaotic:Evil
  }

]
