module Database.Servant.Archer where

import Prelude
import Operators
import Database.Model

archers ∷ Array Servant
archers = Servant <$>
[ { name:     "Gilgamesh"
  , rarity:   5
  , class:    Archer
  , attr:     Heaven
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
  , passives: [magicResistance E, independentAction APlus, divinity B]
  , phantasm: { name:   "Enuma Elish"
              , desc:   "The Star of Creation that Split Heaven and Earth"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-World"
              , hits:   1
              , effect: [ Grant Self 1 NPUp 30.0
                        , To Enemies Damage 600.0 
                        ]
              , over:   [ To (EnemiesType EnumaElish) Damage 150.0 ]
              }
  , gen:      { starWeight: 153, starRate: 7.9, npPerHit: 0.34, npPerDefend: 3 }
  , hits:     { a: 5, b: 5, q: 5, ex: 8 }
  , traits:   [Male, Divine, EnumaElish, King]
  , death:    31.5
  , align:    Chaotic:Good
  , limited: true
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
                          , Grant (AlliesType Male) 3 StarUp 38.0
                          ]
                }
              ]
  , passives: [magicResistance A, independentAction A, territoryCreation A]
  , phantasm: { name:   "Excalibur Viviane"
              , desc:   "Sword of Sunlight-Glitter Victory"
              , rank:   A
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   10
              , effect: [ To Enemy Damage 1500.0 
                        , Chance 70 $ To Enemy GaugeDown 1.0
                        ]
              , over:   [ To Self GaugeUp 20.0 ]
              }
  , gen:      { starWeight: 153, starRate: 8.0, npPerHit: 0.59, npPerDefend: 3 }
  , hits:     { a: 3, b: 3, q: 4, ex: 5 }
  , traits:   [Female, Dragon, Saberface, EnumaElish, King]
  , death:    25.8
  , align:    Lawful:Good
  , limited:  true
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
                , icon:   IconKneel
                , cd:     5
                , effect: [ Grant Self 3 Guts 1.0 
                          , Chance 80 $ Grant Self 3 DefenseUp 30.0
                          , Chance 80 $ Grant Self 1 NPUp 30.0
                          ]
                }
              , { name:   "Pioneer of the Stars EX"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0
                          , Grant Self 3 IgnoreInvinc 0.0
                          , To Party GainStars 10.0
                          ]
                }
              ]
  , passives: [magicResistance C, independentAction B]
  , phantasm: { name:   "System Keraunos"
              , desc:   "Legend of Mankind—Advent of Lightning EX"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   3
              , effect: [ To Enemies Damage 600.0 
                        , Chance 40 $ Debuff Enemies 1 Stun 0.0
                        , To Self DemeritDamage 500.0
                        ]
              , over:   [ To (EnemiesType HeavenOrEarth) Damage 150.0 ]
              }
  , gen:      { starWeight: 147, starRate: 7.9, npPerHit: 0.87, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 3 }
  , traits:   [Male, Brynhildr]
  , death:    31.5
  , align:    Chaotic:Good
  , limited:  false
  }
, { name:     "Orion"
  , rarity:   5
  , class:    Archer
  , attr:     Heaven
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
                , effect: [ Grant Self 1 (AttackUpVs Male) 100.0 ]
                }
              , { name:   "Mind's Eye (Fake) B-"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 CritUp 34.0
                          ]
                }
              ]
  , passives: [magicResistance D, independentAction APlus]
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
              , over:   [ Debuff Enemy 3 CritChance 20.0 ]
              }
  , gen:      { starWeight: 153, starRate: 8.0, npPerHit: 1.0, npPerDefend: 3 }
  , hits:     { a: 1, b: 1, q: 3, ex: 4 }
  , traits:   [Male, GreekMyth, EnumaElish]
  , death:    27.0
  , align:    Chaotic:Balanced
  , limited:  false
  }
, { name:     "Arjuna"
  , rarity:   5
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1907,  hp: 1940 }
              , max:   { atk: 12342, hp: 13230 }
              , grail: { atk: 13510, hp: 14494 }
              }
  , ratings:  { damage:4, np:4, critical:5, utility:3, support:2, durability:3 }
  , actives:  [ { name:   "Clairvoyance C+"
                , icon:   IconStarHaloUp
                , cd:     6
                , effect: [ Grant Self 3 StarUp 36.0 ]
                }
              , { name:   "Hero of the Endowed A"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp 25.0 
                          , Grant Self 5 HealPerTurn 2000.0
                          , Grant Self 5 StarsPerTurn 8.0
                          ]
                }
              , { name:   "Mana Burst (Flame) A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 30.0 
                          , Grant Self 1 NPUp 20.0
                          ]
                }
              ]
  , passives: [magicResistance C, independentAction A, divinity B]
  , phantasm: { name:   "Pashupata"
              , desc:   "Raised Hand of the Destruction God"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage 600.0 
                        , To Enemies Kill 50.0
                        , To (EnemiesType Divine) Kill 80.0
                        ]
              , over:   [ Debuff Enemies 3 DefenseDown 20.0 ]
              }
  , gen:      { starWeight: 154, starRate: 8.0, npPerHit: 0.51, npPerDefend: 3 }
  , hits:     { a: 3, b: 3, q: 2, ex: 5 }
  , traits:   [Male, Divine, EnumaElish]
  , death:    31.5
  , align:    Lawful:Balanced   
  , limited:  false
  }
, { name:     "Chloe von Einzbern"
  , rarity:   4
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1640,  hp: 1746 }
              , max:   { atk: 9845, hp: 10914 }
              , grail: { atk: 11920, hp: 13239 }
              }
  , ratings:  { damage:5, np:4, critical:5, utility:2, support:1, durability:3 }
  , actives:  [ { name:   "Mind's Eye (Fake) B"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 CritUp 36.0
                          ]
                }
              , { name:   "Projection B"
                , icon:   IconAllUp
                , cd:     5
                , effect: [ Grant Self 1 ArtsUp 35.0 
                          , Grant Self 1 QuickUp 35.0
                          , Grant Self 1 BusterUp 35.0
                          ]
                }
              , { name:   "Kiss Demon B"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0 
                          , Grant Self 3 StarUp 100.0
                          ]
                }
              ]
  , passives: [magicResistance C, independentAction B]
  , phantasm: { name:   "Kakuyoku San-Ren"
              , desc:   "Triple-Linked Crane Wings"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   6
              , effect: [ Grant Self 1 SureHit 0.0 
                        , To Enemy Damage 1500.0
                        ]
              , over:   [ Debuff Enemy 3 CritChance 20.0 ]
              }
  , gen:      { starWeight: 148, starRate: 8.0, npPerHit: 0.38, npPerDefend: 3 }
  , hits:     { a: 6, b: 2, q: 3, ex: 4 }
  , traits:   [Female, PseudoServant, EnumaElish]
  , death:    36.0
  , align:    Chaotic:Good
  , limited:  true
  }
, { name:     "Oda Nobunaga"
  , rarity:   4
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1582,  hp: 1862 }
              , max:   { atk: 9494, hp: 11637 }
              , grail: { atk: 11495, hp: 14110 }
              }
  , ratings:  { damage:4, np:3, critical:5, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Strategy B"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Party 3 NPGen 30.0 ]
                }
              , { name:   "Unifying the Nation by Force A"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 1 (AttackUpVs Divine) 100.0 ]
                }
              , { name:   "The Demonic King A"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 3 CritUp 50.0 
                          , Grant Self 3 StarAbsorb 600.0
                          ]
                }
              ]
  , passives: [magicResistance B, independentAction B]
  , phantasm: { name:   "Three Line Formation"
              , desc:   "Three Thousand Worlds"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   10
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ To (EnemiesType Riding) Damage 150.0 ]
              }
  , gen:      { starWeight: 150, starRate: 7.9, npPerHit: 0.43, npPerDefend: 3 }
  , hits:     { a: 4, b: 4, q: 2, ex: 5 }
  , traits:   [Female, EnumaElish, King]
  , death:    31.5
  , align:    Lawful:Balanced
  , limited:  false
  }
, { name:     "Tristan"
  , rarity:   4
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1622,  hp: 1862 }
              , max:   { atk: 9735, hp: 11637 }
              , grail: { atk: 11787, hp: 14110 }
              }
  , ratings:  { damage:3, np:4, critical:3, utility:4, support:4, durability:3 }
  , actives:  [ { name:   "Harp of Healing C"
                , icon:   IconBubbles
                , cd:     6
                , effect: [ To Party RemoveMental 0.0 
                          , Grant Party 0 Evasion 1.0
                          , To Party Heal 600.0
                          ]
                }
              , { name:   "Unblessed Birth B"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0 
                          , Debuff Self 1 SealNP 0.0
                          ]
                }
              , { name:   "Critique to the King B"
                , icon:   IconCircuits
                , cd:     5
                , effect: [ To Enemy RemoveBuffs 0.0 
                          , Debuff Enemy 3 CritChance 20.0
                          ]
                }
              ]
  , passives: [magicResistance B, independentAction B]
  , phantasm: { name:   "Failnaught"
              , desc:   "Fantasia of Lamentation"
              , rank:   A
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   7
              , effect: [ Grant Self 1 SureHit 0.0 
                        , To Enemy Damage 2000.0
                        ]
              , over:   [ Debuff Enemy 3 DebuffVuln 30.0 ]
              }
  , gen:      { starWeight: 145, starRate: 8.1, npPerHit: 0.58, npPerDefend: 3 }
  , hits:     { a: 3, b: 5, q: 4, ex: 6 }
  , traits:   [Male, EnumaElish]
  , death:    31.5
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "EMIYA"
  , rarity:   4
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1566,  hp: 1843 }
              , max:   { atk: 9398, hp: 11521 }
              , grail: { atk: 11379, hp: 13969 }
              }
  , ratings:  { damage:3, np:4, critical:3, utility:2, support:1, durability:4 }
  , actives:  [ { name:   "Mind's Eye (True) B"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 DefenseUp 18.0
                          ]
                }
              , { name:   "Clairvoyance C"
                , icon:   IconStarHaloUp
                , cd:     6
                , effect: [ Grant Self 3 StarUp 32.0 ]
                }
              , { name:   "Projection A"
                , icon:   IconAllUp
                , cd:     5
                , effect: [ Grant Self 1 ArtsUp 40.0 
                          , Grant Self 1 QuickUp 40.0
                          , Grant Self 1 BusterUp 40.0
                          ]
                }
              ]
  , passives: [magicResistance D, independentAction B]
  , phantasm: { name:   "Unlimited Blade Works"
              , desc:   "Infinite Creation of Swords"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Unknown"
              , hits:   10
              , effect: [ To Enemies DamageThruDef 600.0 ]
              , over:   [ Debuff Enemies 3 AttackDown 10.0 ]
              }
  , gen:      { starWeight: 145, starRate: 7.9, npPerHit: 0.51, npPerDefend: 3 }
  , hits:     { a: 3, b: 1, q: 2, ex: 5 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    31.5
  , align:    Neutral:Balanced
  , limited:  false
  }
, { name:     "Atalante"
  , rarity:   4
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1438,  hp: 1996 }
              , max:   { atk: 8633, hp: 12476 }
              , grail: { atk: 10453, hp: 15127 }
              }
  , ratings:  { damage:4, np:2, critical:5, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Beyond Arcadia A"
                , icon:   IconQuickUp
                , cd:     5
                , effect: [ Grant Party 1 QuickUp 50.0 ]
                }
              , { name:   "Hunter's Aesthetic C"
                , icon:   IconStarUp
                , cd:     4
                , effect: [ Grant Self 1 StarAbsorb 1000.0 ]
                }
              , { name:   "Calydonian Hunt A"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 NPGen 50.0
                          ]
                }
              ]
  , passives: [magicResistance D, independentAction A]
  , phantasm: { name:   "Phoebus Catastrophe"
              , desc:   "Complaint Message on the Arrow"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   10
              , effect: [ To Enemies Damage 1200.0 ]
              , over:   [ To Party GainStars 35.0 ]
              }
  , gen:      { starWeight: 148, starRate: 8.0, npPerHit: 0.5, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    31.5
  , align:    Neutral:Evil
  , limited:  false
  }
, { name:     "Anne Bonny & Mary Read (Archer)"
  , rarity:   4
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1574,  hp: 1843  }
              , max:   { atk: 9446, hp: 11521 }
              , grail: { atk: 11437, hp: 13969 }
              }
  , ratings:  { damage:4, np:3, critical:4, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Beach Flower A+"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 19.5 
                          , Grant (AlliesType Male) 3 StarUp 41.0
                          ]
                }
              , { name:   "Treasure Hunt (Sea) C"
                , icon:   IconStarHaloUp
                , cd:     6
                , effect: [ Grant Self 1 StarAbsorb 600.0 
                          , To Party GainStars 15.0
                          ]
                }
              , { name:   "Pirate's Honor C+"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 25.5 
                          , Grant Self 0 Guts 1.0
                          , Debuff Self 3 DebuffVuln 50.0
                          ]
                }
              ]
  , passives: [magicResistance D, independentAction APlus]
  , phantasm: { name:   "Carribean Free Bird Act 2"
              , desc:   "Wings Abreast As If Trees With Entwined Branches"
              , rank:   CPlusPlus
              , card:   Buster
              , kind:   "Anti-Unit"
              , hits:   9
              , effect: [ To Enemy Damage 1000.0 
                        , To Enemy DamageRevenge 600.0
                        ] 
              , over:   [ Debuff Enemy 3 DefenseDown 10.0 ]
              }
  , gen:      { starWeight: 153, starRate: 8.1, npPerHit: 0.85, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    40.5
  , align:    Chaotic:Balanced
  , limited:  true
  }
, { name:     "Robin Hood"
  , rarity:   3
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1247,  hp: 1833 }
              , max:   { atk: 6715, hp: 10187 }
              , grail: { atk: 9088, hp: 13812 }
              }
  , ratings:  { damage:5, np:4, critical:3, utility:3, support:2, durability:2 }
  , actives:  [ { name:   "Sabotage A"
                , icon:   IconSwordDown
                , cd:     5
                , effect: [ Debuff Enemies 3 AttackDown 15.0 
                          , Debuff Enemies 5 Poison 500.0
                          ]
                }
              , { name:   "Golden Rule E"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 30.0 ]
                }
              , { name:   "May King B"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 SureHit 0.0 
                          , Grant Self 1 Evasion 0.0
                          , To Party GainStars 20.0
                          ]
                }
              ]
  , passives: [magicResistance D, independentAction A]
  , phantasm: { name:   "Yew Bow"
              , desc:   "Bow of Prayer"
              , rank:   D
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage 1500.0 ]
              , over:   [ To (EnemyType Poisoned) Damage 200.0 ]
              }
  , gen:      { starWeight: 150, starRate: 8.0, npPerHit: 0.87, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    31.5
  , align:    Neutral:Good
  , limited:  false
  }
, { name:     "Euryale"
  , rarity:   3
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1306,  hp: 1711 }
              , max:   { atk: 7032, hp: 9506 }
              , grail: { atk: 9517, hp: 12889 }
              }
  , ratings:  { damage:4, np:4, critical:3, utility:3, support:2, durability:3 }
  , actives:  [ { name:   "Vampirism C"
                , icon:   IconDarkMagic
                , cd:     6
                , effect: [ Chance 80 $ To Enemy GaugeDown 1.0
                          , To Self GaugeUp 27.0
                          ]
                }
              , { name:   "Siren Song A"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Chance 100 $ Debuff (EnemyType Male) 1 Charm 0.0 ]
                }
              , { name:   "Whim of the Goddess A (Euryale)"
                , icon:   IconArtsUp
                , cd:     5
                , effect: [ Grant Self 3 ArtsUp 30.0 ]
                }
              ]
  , passives: [magicResistance A, independentAction APlus, coreOfGoddess EX]
  , phantasm: { name:   "Eye of the Euryale B"
              , desc:   "Gaze of the Goddess"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage 1200.0 
                        , To (EnemyType Male) Damage 250.0
                        , Debuff Enemy 3 AttackDown 20.0
                        ]
              , over:   [ Chance 100 $ Debuff (EnemyType Male) 1 Charm 0.0 ]
              }
  , gen:      { starWeight: 156, starRate: 7.9, npPerHit: 0.9, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 3 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    22.5
  , align:    Chaotic:Good
  , limited:  false
  }
, { name:     "Arash"
  , rarity:   1
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1057,  hp: 1424 }
              , max:   { atk: 5816, hp: 7122 }
              , grail: { atk: 9037, hp: 10979 }
              }
  , ratings:  { damage:5, np:3, critical:3, utility:5, support:1, durability:2 }
  , actives:  [ { name:   "Toughness EX"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 20.0 
                          , Grant Self 3 PoisonResist 160.0
                          ]
                }
              , { name:   "Clairvoyance A"
                , icon:   IconStarHaloUp
                , cd:     6
                , effect: [ Grant Self 3 StarUp 40.0 ]
                }
              , { name:   "Arrow Construction A"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 30.0 
                          , To Self Heal 3000.0
                          ]
                }
              ]
  , passives: [magicResistance C, independentAction C]
  , phantasm: { name:   "Stella"
              , desc:   "Single-Shot Shooting Star"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage 1200.0 
                        , To Self DemeritKill 0.0
                        ]
              , over:   [ To Enemies Damage 0.0 ]
              }
  , gen:      { starWeight: 147, starRate: 8.0, npPerHit: 0.84, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    45.0
  , align:    Chaotic:Balanced
  , limited:  false
  }
, { name:     "David"
  , rarity:   3
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1436,  hp: 1555 }
              , max:   { atk: 7736, hp: 8643 }
              , grail: { atk: 10470, hp: 11719 }
              }
  , ratings:  { damage:4, np:4, critical:3, utility:3, support:4, durability:4 }
  , actives:  [ { name:   "Divine Protection A"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 1 DefenseUp 50.0 
                          , To Self Heal 2000.0
                          ]
                }
              , { name:   "Harp of Healing B"
                , icon:   IconBubbles
                , cd:     6
                , effect: [ To Party RemoveMental 0.0 
                          , Grant Party 0 Evasion 1.0
                          , To Party Heal 800.0
                          ]
                }
              , { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 18.0 ]
                }
              ]
  , passives: [magicResistance A, independentAction A]
  , phantasm: { name:   "Hamesh Avanim"
              , desc:   "The Five Stones"
              , rank:   CMinus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ Grant Self 1 SureHit 0.0 
                        , To Enemy Damage 1000.0
                        ]
              , over:   [ Chance 100 $ Debuff Enemy 0 SealSkills 0.0 ]
              }
  , gen:      { starWeight: 153, starRate: 8.0, npPerHit: 0.76, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish, King]
  , death:    36.0
  , align:    Lawful:Balanced
  , limited:  false
  }
, { name:     "Kid Gilgamesh"
  , rarity:   3
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1429,  hp: 1571 }
              , max:   { atk: 7696, hp: 8731 }
              , grail: { atk: 10415, hp: 11838 }
              }
  , ratings:  { damage:4, np:4, critical:4, utility:3, support:3, durability:2 }
  , actives:  [ { name:   "Charisma A+"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 21.0 ]
                }
              , { name:   "Fair Youth C"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Chance 75 $ Debuff (EnemyType Humanoid) 1 Charm 0.0 ]
                }
              , { name:   "Golden Rule A"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 50.0 ]
                }
              ]
  , passives: [magicResistance E, independentAction A, divinity B]
  , phantasm: { name:   "Gate of Babylon"
              , desc:   "King's Treasure"
              , rank:   BPlus
              , card:   Buster
              , kind:   "Anti-Unit"
              , hits:   10
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Debuff Enemies 1 NPDown 50.0
                        , Debuff Enemies 1 CritDown 50.0
                        , Debuff Enemies 1 DebuffVuln 20.0
                        ]
              }
  , gen:      { starWeight: 153, starRate: 7.9, npPerHit: 0.62, npPerDefend: 3 }
  , hits:     { a: 3, b: 3, q: 3, ex: 3 }
  , traits:   [Male, Divine, EnumaElish, King]
  , death:    36.0
  , align:    Chaotic:Good
  , limited:  false
  }
, { name:     "Billy the Kid"
  , rarity:   3
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1279,  hp: 1711 }
              , max:   { atk: 6890, hp: 9506 }
              , grail: { atk: 9325, hp: 12889 }
              }
  , ratings:  { damage:4, np:3, critical:5, utility:2, support:1, durability:3 }
  , actives:  [ { name:   "Marksmanship A++"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 1 CritUp 120.0 ]
                }
              , { name:   "Quick Draw A+"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0 ]
                }
              , { name:   "Mind's Eye (Fake) C"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 CritUp 32.0
                          ]
                }
              ]
  , passives: [independentAction A, riding CPlus]
  , phantasm: { name:   "Thunderer"
              , desc:   "Thunderbolt of Broken Sound"
              , rank:   CPlus
              , card:   Quick
              , kind:   "Anti-Unit"
              , hits:   3
              , effect: [ Grant Self 1 SureHit 0.0 
                        , To Enemy Damage 2000.0
                        ]
              , over:   [ Debuff Enemy 3 CritChance 10.0 ]
              }
  , gen:      { starWeight: 150, starRate: 8.0, npPerHit: 0.56, npPerDefend: 3 }
  , hits:     { a: 3, b: 4, q: 2, ex: 4 }
  , traits:   [Male, Riding, EnumaElish]
  , death:    45.0
  , align:    Chaotic:Balanced
  , limited:  false
  }
, { name:     "Tawara Touta"
  , rarity:   3
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1306,  hp: 1764 }
              , max:   { atk: 7032, hp: 9800 }
              , grail: { atk: 9517, hp: 13287 }
              }
  , ratings:  { damage:3, np:3, critical:2, utility:3, support:3, durability:4 }
  , actives:  [ { name:   "Protection of the Dragon King C"
                , icon:   IconBusterUp
                , cd:     6
                , effect: [ Grant Self 3 BusterUp 30.0 
                          , To Self Heal 2000.0
                          ]
                }
              , { name:   "Protection from Arrows C"
                , icon:   IconDodge
                , cd:     5
                , effect: [ Grant Self 0 Evasion 2.0 
                          , Grant Self 3 DefenseUp 14.0
                          ]
                }
              , { name:   "Inexhaustible Straw Bag EX"
                , icon:   IconHPUp
                , cd:     5
                , effect: [ Grant Party 3 MaxHP 2000.0 ]
                }
              ]
  , passives: [magicResistance C, independentAction B]
  , phantasm: { name:   "Hachiman Prayer"
              , desc:   "Hachiman Prayer—Shooting Through a Great Demon"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Grant Self 1 (AttackUpVs Demonic) 50.0 ]
              }
  , gen:      { starWeight: 150, starRate: 7.8, npPerHit: 0.57, npPerDefend: 3 }
  , hits:     { a: 3, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Brynhildr, EnumaElish]
  , death:    36.0
  , align:    Neutral:Good
  , limited:  false
  }

]
