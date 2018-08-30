module Database.Servant.Archer where

import Prelude
import Operators
import Database.Model

archers ∷ Array Servant
archers = [
  { name:     "Gilgamesh"
  , id:       12
  , rarity:   5
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1897,  hp: 1920 }
              , max:   { atk: 12280, hp: 13097 }
              , grail: { atk: 13442, hp: 14348 }
              }
  , actives:  [ { name:   "Charisma A+"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.5 ~ 21.0 ]
                }
              , { name:   "Golden Rule A"
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 20.0 ~ 50.0 ]
                }
              , { name:   "Collector EX"
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 3 StarAbsorb $ 300.0 ~ 600.0 ]
                }
              ]
  , passives: [magicResistance E, independentAction APlus, divinity B]
  , phantasm: { name:   "Enuma Elish"
              , desc:   "The Star of Creation that Split Heaven and Earth"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-World"
              , hits:   1
              , effect: [ Grant Self 1 NPUp $ Flat 30.0
                        , To Enemies Damage $ 400.0 ~ 600.0 
                        ]
              , over:   [ To (EnemiesType EnumaElish) Damage $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 153, starRate: 7.9, npAtk: 0.34, npDef: 3 }
  , hits:     { arts: 5, buster: 5, quick: 5, ex: 8 }
  , traits:   [Male, Divine, EnumaElish, King]
  , death:    31.5
  , align:    Chaotic:Good
  , limited: true
  , free:    false
  }
, { name:     "Altria Pendragon (Archer)"
  , id:       129
  , rarity:   5
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1742,  hp: 2134 }
              , max:   { atk: 11276, hp: 14553 }
              , grail: { atk: 12343, hp: 15943 }
              }
  , actives:  [ { name:   "Summer Splash! A+"
                , icon:   IconArtsUp
                , cd:     8
                , effect: [ Grant Self 3 ArtsUp  $ 20.0 ~ 30.0 
                          , Grant Party 3 DefenseUp $ 10.0 ~ 20.0 
                          ]
                }
              , { name:   "Beach House Protection EX"
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Self Heal $ 2000.0 ~ 5000.0 
                          , To Self DemeritGauge $ Flat 10.0
                          ]
                }
              , { name:   "Beach Flower B"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 8.0 ~ 18.0 
                          , Grant (AlliesType Male) 3 StarUp $ 18.0 ~ 38.0
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
              , effect: [ To Enemy Damage $ 900.0 ~ 1500.0 
                        , Chance 70 ∘ To Enemy GaugeDown $ Flat 1.0
                        ]
              , over:   [ To Self GaugeUp $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 153, starRate: 8.0, npAtk: 0.59, npDef: 3 }
  , hits:     { arts: 3, buster: 3, quick: 4, ex: 5 }
  , traits:   [Female, EnumaElish, Arthur, Dragon, King, Saberface ]
  , death:    25.8
  , align:    Lawful:Good
  , limited:  true
  , free:     false
  }
, { name:     "Nikola Tesla"
  , id:       77
  , rarity:   5
  , class:    Archer
  , attr:     Star
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1820,  hp: 2027 }
              , max:   { atk: 11781, hp: 13825 }
              , grail: { atk: 12896, hp: 15146 }
              }
  , actives:  [ { name:   "Galvanism A"
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Self 3 NPGen $ 30.0 ~ 50.0 ]
                }
              , { name:   "Inherent Wisdom A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 3 Guts $ 1000.0 ~ 3000.0
                          , Chance 80 ∘ Grant Self 3 DefenseUp $ 20.0 ~ 30.0
                          , Chance 80 ∘ Grant Self 1 NPUp $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Pioneer of the Stars EX"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0
                          , Grant Self 3 IgnoreInvinc Full
                          , To Party GainStars $ Flat 10.0
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
              , effect: [ To Enemies Damage $ 400.0 ~ 600.0 
                        , Chance 40 $ Debuff Enemies 1 Stun Full
                        , To Self DemeritDamage $ Flat 500.0
                        ]
              , over:   [ To (EnemiesType HeavenOrEarth) Damage$ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 147, starRate: 7.9, npAtk: 0.87, npDef: 3 }
  , hits:     { arts: 2, buster: 1, quick: 3, ex: 3 }
  , traits:   [Male, Brynhild]
  , death:    31.5
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Orion"
  , id:       60
  , rarity:   5
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1716,  hp: 2134 }
              , max:   { atk: 11107, hp: 14553 }
              , grail: { atk: 12158, hp: 15943 }
              }
  , actives:  [ { name:   "Grace of the Goddess EX"
                , icon:   IconShieldUp
                , cd:     9
                , effect: [ Grant Self 1 DefenseUp $ 30.0 ~ 50.0 
                          , Grant Self 3 AttackUp $ Flat 20.0
                          , Grant Self 3 DebuffResist $ Flat 50.0
                          ]
                }
              , { name:   "Punish the Unfaithful A+"
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 1 (AttackUpVs Male) $ 50.0 ~ 100.0 ]
                }
              , { name:   "Mind's Eye (Fake) B-"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 17.0 ~ 34.0
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
              , effect: [ To Enemy Damage $ 1200.0 ~ 1800.0 
                        , Debuff Enemy 3 AttackDown $ Flat 20.0
                        , To Enemy GaugeDown $ Flat 1.0
                        ]
              , over:   [ Debuff Enemy 3 CritChance $ 20.0 ~ 60.0 ]
              , first:  false
              }
  , gen:      { starWeight: 153, starRate: 8.0, npAtk: 1.0, npDef: 3 }
  , hits:     { arts: 1, buster: 1, quick: 3, ex: 4 }
  , traits:   [Male, GreekMyth, EnumaElish]
  , death:    27.0
  , align:    Chaotic:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Arjuna"
  , id:       84
  , rarity:   5
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1907,  hp: 1940 }
              , max:   { atk: 12342, hp: 13230 }
              , grail: { atk: 13510, hp: 14494 }
              }
  , actives:  [ { name:   "Clairvoyance C+"
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 StarUp $ 18.0 ~ 36.0 ]
                }
              , { name:   "Hero of the Endowed A"
                , icon:   IconNoble
                , cd:     10
                , effect: [ To Self GaugeUp $ Flat 25.0 
                          , Grant Self 5 HealPerTurn $ 1000.0 ~ 2000.0
                          , Grant Self 5 StarsPerTurn $ 4.0 ~ 8.0
                          ]
                }
              , { name:   "Mana Burst (Flame) A"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 BusterUp $ 20.0 ~ 30.0 
                          , Grant Self 1 NPUp $ 10.0 ~ 20.0
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
              , effect: [ To Enemies Damage $ 400.0 ~ 600.0 
                        , To Enemies Kill $ Flat 50.0
                        , To (EnemiesType Divine) Kill $ Flat 80.0
                        ]
              , over:   [ Debuff Enemies 3 DefenseDown $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 154, starRate: 8.0, npAtk: 0.51, npDef: 3 }
  , hits:     { arts: 3, buster: 3, quick: 2, ex: 5 }
  , traits:   [Male, Divine, EnumaElish]
  , death:    31.5
  , align:    Lawful:Balanced   
  , limited:  false
  , free:    false
  }
, { name:     "Chloe von Einzbern"
  , id:       137
  , rarity:   4
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1640,  hp: 1746 }
              , max:   { atk: 9845, hp: 10914 }
              , grail: { atk: 11920, hp: 13239 }
              }
  , actives:  [ { name:   "Mind's Eye (Fake) B"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 18.0 ~ 36.0
                          ]
                }
              , { name:   "Projection B"
                , icon:   IconAllUp
                , cd:     7
                , effect: [ Grant Self 1 ArtsUp $ 20.0 ~ 35.0 
                          , Grant Self 1 QuickUp $ 20.0 ~ 35.0
                          , Grant Self 1 BusterUp $ 20.0 ~ 35.0
                          ]
                }
              , { name:   "Kiss Demon B"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0 
                          , Grant Self 3 StarUp $ 50.0 ~ 100.0
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
              , effect: [ Grant Self 1 SureHit Full
                        , To Enemy Damage $ 900.0 ~ 1500.0
                        ]
              , over:   [ Debuff Enemy 3 CritChance $ 20.0 ~ 60.0 ]
              , first:  false
              }
  , gen:      { starWeight: 148, starRate: 8.0, npAtk: 0.38, npDef: 3 }
  , hits:     { arts: 6, buster: 2, quick: 3, ex: 4 }
  , traits:   [Female, PseudoServant, EnumaElish]
  , death:    36.0
  , align:    Chaotic:Good
  , limited:  true
  , free:     true
  }
, { name:     "Oda Nobunaga"
  , id:       69
  , rarity:   4
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1862,  hp: 1582 }
              , max:   { atk: 9494, hp: 11637 }
              , grail: { atk: 11495, hp: 14110 }
              }
  , actives:  [ { name:   "Strategy B"
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Party 3 NPGen $ 20.0 ~ 30.0 ]
                }
              , { name:   "Unifying the Nation by Force A"
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 1 (AttackUpVs Divine) $ 50.0 ~ 100.0 ]
                }
              , { name:   "The Demonic King A"
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 3 CritUp $ 20.0 ~ 50.0 
                          , Grant Self 3 StarAbsorb $ 300.0 ~ 600.0
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
              , effect: [ To Enemies Damage $ 100.0 ~ 500.0 ]
              , over:   [ To (EnemiesType Riding) Damage $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 150, starRate: 7.9, npAtk: 0.43, npDef: 3 }
  , hits:     { arts: 4, buster: 4, quick: 2, ex: 5 }
  , traits:   [Female, EnumaElish, King]
  , death:    31.5
  , align:    Lawful:Balanced
  , limited:  true
  , free:     true
  }
, { name:     "Tristan"
  , id:       122
  , rarity:   4
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1622,  hp: 1862 }
              , max:   { atk: 9735, hp: 11637 }
              , grail: { atk: 11787, hp: 14110 }
              }
  , actives:  [ { name:   "Harp of Healing C"
                , icon:   IconBubbles
                , cd:     8
                , effect: [ To Party RemoveMental Full 
                          , Grant Party 0 Evasion Full
                          , To Party Heal $ 200.0 ~ 600.0
                          ]
                }
              , { name:   "Unblessed Birth B"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0 
                          , Debuff Self 1 SealNP Full
                          ]
                }
              , { name:   "Critique to the King B"
                , icon:   IconCircuits
                , cd:     7
                , effect: [ To Enemy RemoveBuffs Full
                          , Debuff Enemy 3 CritChance $ 10.0 ~ 20.0
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
              , effect: [ Grant Self 1 SureHit Full
                        , To Enemy Damage $ 1200.0 ~ 2000.0
                        ]
              , over:   [ Debuff Enemy 3 DebuffVuln $ 30.0 ~ 70.0 ]
              , first:  false
              }
  , gen:      { starWeight: 145, starRate: 8.1, npAtk: 0.58, npDef: 3 }
  , hits:     { arts: 3, buster: 5, quick: 4, ex: 6 }
  , traits:   [Male, EnumaElish]
  , death:    31.5
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
, { name:     "EMIYA"
  , id:       11
  , rarity:   4
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1566,  hp: 1843 }
              , max:   { atk: 9398, hp: 11521 }
              , grail: { atk: 11379, hp: 13969 }
              }
  , actives:  [ { name:   "Mind's Eye (True) B"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 DefenseUp $ 9.0 ~ 18.0
                          ]
                }
              , { name:   "Clairvoyance C"
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 StarUp $ 16.0 ~ 32.0 ]
                }
              , { name:   "Projection A"
                , icon:   IconAllUp
                , cd:     7
                , effect: [ Grant Self 1 ArtsUp $ 25.0 ~ 40.0 
                          , Grant Self 1 QuickUp $ 25.0 ~ 40.0
                          , Grant Self 1 BusterUp $ 25.0 ~ 40.0
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
              , effect: [ To Enemies DamageThruDef $ 400.0 ~ 600.0 ]
              , over:   [ Debuff Enemies 3 AttackDown $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 145, starRate: 7.9, npAtk: 0.51, npDef: 3 }
  , hits:     { arts: 3, buster: 1, quick: 2, ex: 5 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    31.5
  , align:    Neutral:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Atalante"
  , id:       14
  , rarity:   4
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1438,  hp: 1996 }
              , max:   { atk: 8633, hp: 12476 }
              , grail: { atk: 10453, hp: 15127 }
              }
  , actives:  [ { name:   "Beyond Arcadia A"
                , icon:   IconQuickUp
                , cd:     7
                , effect: [ Grant Party 1 QuickUp $ 30.0 ~ 50.0 ]
                }
              , { name:   "Hunter's Aesthetic C"
                , icon:   IconStarUp
                , cd:     6
                , effect: [ Grant Self 1 StarAbsorb $ 500.0 ~ 1000.0 ]
                }
              , { name:   "Calydonian Hunt A"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 NPGen $ 30.0 ~ 50.0
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
              , effect: [ To Enemies Damage $ 800.0 ~ 1200.0 ]
              , over:   [ To Party GainStars $ 15.0 ~ 35.0 ]
              , first:  false
              }
  , gen:      { starWeight: 148, starRate: 8.0, npAtk: 0.5, npDef: 3 }
  , hits:     { arts: 2, buster: 1, quick: 3, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    31.5
  , align:    Neutral:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Anne Bonny & Mary Read (Archer)"
  , id:       131
  , rarity:   4
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1574,  hp: 1843  }
              , max:   { atk: 9446, hp: 11521 }
              , grail: { atk: 11437, hp: 13969 }
              }
  , actives:  [ { name:   "Beach Flower A+"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.5 ~ 19.5 
                          , Grant (AlliesType Male) 3 StarUp $ 21.0 ~ 41.0
                          ]
                }
              , { name:   "Treasure Hunt (Sea) C"
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 1 StarAbsorb $ 300.0 ~ 600.0 
                          , To Party GainStars $ 5.0 ~ 15.0
                          ]
                }
              , { name:   "Pirate's Honor C+"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 8.5 ~ 25.5 
                          , Grant Self 0 Guts $ Flat 1.0
                          , Debuff Self 3 DebuffVuln $ Flat 50.0
                          ]
                }
              ]
  , passives: [magicResistance D, independentAction A]
  , phantasm: { name:   "Carribean Free Bird Act 2"
              , desc:   "Wings Abreast As If Trees With Entwined Branches"
              , rank:   CPlusPlus
              , card:   Buster
              , kind:   "Anti-Unit"
              , hits:   9
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0 
                        , To Enemy LastStand $ Flat 600.0
                        ] 
              , over:   [ Debuff Enemy 3 DefenseDown $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 153, starRate: 8.1, npAtk: 0.85, npDef: 3 }
  , hits:     { arts: 2, buster: 1, quick: 3, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    40.5
  , align:    Chaotic:Balanced
  , limited:  true
  , free:     false
  }
, { name:     "Robin Hood"
  , id:       13
  , rarity:   3
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1247,  hp: 1833 }
              , max:   { atk: 6715, hp: 10187 }
              , grail: { atk: 9088, hp: 13812 }
              }
  , actives:  [ { name:   "Sabotage A"
                , icon:   IconSwordDown
                , cd:     7
                , effect: [ Debuff Enemies 3 AttackDown $ 5.0 ~ 15.0 
                          , Debuff Enemies 5 Poison $ Flat 500.0
                          ]
                }
              , { name:   "Golden Rule E"
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 12.0 ~ 30.0 ]
                }
              , { name:   "May King B"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 SureHit Full
                          , Grant Self 1 Evasion Full
                          , To Party GainStars $ 10.0 ~ 20.0
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
              , effect: [ To Enemy Damage $ 900.0 ~ 1500.0 ]
              , over:   [ To Enemy (DamageVs Poisoned) $ 200.0 ~ 250.0 ]
              , first:  false
              }
  , gen:      { starWeight: 150, starRate: 8.0, npAtk: 0.87, npDef: 3 }
  , hits:     { arts: 2, buster: 1, quick: 3, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    31.5
  , align:    Neutral:Good
  , limited:  false
  , free:     false
  }
, { name:     "Euryale"
  , id:       15
  , rarity:   3
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1306,  hp: 1711 }
              , max:   { atk: 7032, hp: 9506 }
              , grail: { atk: 9517, hp: 12889 }
              }
  , actives:  [ { name:   "Vampirism C"
                , icon:   IconDarkMagic
                , cd:     8
                , effect: [ Chances 60 80 ∘ To Enemy GaugeDown $ Flat 1.0
                          , To Self GaugeUp $ 18.0 ~ 27.0
                          ]
                }
              , { name:   "Siren Song A"
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 70 100 
                            $ Debuff (EnemyType Male) 1 Charm Full ]
                }
              , { name:   "Whim of the Goddess A (Euryale)"
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Self 3 ArtsUp $ 20.0 ~ 30.0 ]
                }
              ]
  , passives: [magicResistance A, independentAction APlus, coreOfGoddess EX]
  , phantasm: { name:   "Eye of the Euryale B"
              , desc:   "Gaze of the Goddess"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage $ Flat 1200.0 
                        , To Enemy (DamageVs Male) $ 150.0 ~ 250.0
                        , Chance 150 ∘ Debuff Enemy 3 AttackDown $ Flat 20.0
                        ]
              , over:   [ Chances 100 200 
                          $ Debuff (EnemyType Male) 1 Charm Full ]
              , first:  false
              }
  , gen:      { starWeight: 156, starRate: 7.9, npAtk: 0.9, npDef: 3 }
  , hits:     { arts: 2, buster: 1, quick: 3, ex: 3 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    22.5
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Arash"
  , id:       16
  , rarity:   1
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1057,  hp: 1424 }
              , max:   { atk: 5816, hp: 7122 }
              , grail: { atk: 9037, hp: 10979 }
              }
  , actives:  [ { name:   "Toughness EX"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 10.0 ~ 20.0 
                          , Grant Self 3 PoisonResist $ 80.0 ~ 160.0
                          ]
                }
              , { name:   "Clairvoyance A"
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 StarUp $ 20.0 ~ 40.0 ]
                }
              , { name:   "Arrow Construction A"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 20.0 ~ 30.0 
                          , To Self Heal $ 1000.0 ~ 3000.0
                          ]
                }
              ]
  , passives: [magicResistance C, independentAction C]
  , phantasm: { name:   "Stella"
              , desc:   "Single-Shot Shooting Star"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   2
              , effect: [ To Enemies Damage $ 800.0 ~ 1200.0 
                        , To Self DemeritKill Full
                        ]
              , over:   [ To Enemies Damage $ 0.0 ~ 800.0 ]
              , first:  false
              }
  , gen:      { starWeight: 147, starRate: 8.0, npAtk: 0.84, npDef: 3 }
  , hits:     { arts: 2, buster: 1, quick: 3, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    45.0
  , align:    Chaotic:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "David"
  , id:       63
  , rarity:   3
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1436,  hp: 1555 }
              , max:   { atk: 7736, hp: 8643 }
              , grail: { atk: 10470, hp: 11719 }
              }
  , actives:  [ { name:   "Divine Protection A"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 1 DefenseUp $ Flat 50.0 
                          , To Self Heal $ 1000.0 ~ 2000.0
                          ]
                }
              , { name:   "Harp of Healing B"
                , icon:   IconBubbles
                , cd:     8
                , effect: [ To Party RemoveMental Full
                          , Grant Party 0 Evasion $ Flat 1.0
                          , To Party Heal $ 300.0 ~ 800.0
                          ]
                }
              , { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.0 ~ 18.0 ]
                }
              ]
  , passives: [magicResistance A, independentAction A]
  , phantasm: { name:   "Hamesh Avanim"
              , desc:   "The Five Stones"
              , rank:   CMinus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ Grant Self 1 SureHit Full
                        , To Enemy Damage $ 600.0 ~ 1000.0
                        ]
              , over:   [ Chances 100 500 $ Debuff Enemy 1 SealSkills Full ]
              , first:  false
              }
  , gen:      { starWeight: 153, starRate: 8.0, npAtk: 0.76, npDef: 3 }
  , hits:     { arts: 2, buster: 1, quick: 2, ex: 3 }
  , traits:   [Male, EnumaElish, King]
  , death:    36.0
  , align:    Lawful:Balanced
  , limited:  false
  , free:     true
  }
, { name:     "Kid Gilgamesh"
  , id:       95
  , rarity:   3
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1429,  hp: 1571 }
              , max:   { atk: 7696, hp: 8731 }
              , grail: { atk: 10415, hp: 11838 }
              }
  , actives:  [ { name:   "Charisma A+"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.5 ~ 21.0 ]
                }
              , { name:   "Fair Youth C"
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 45 75 
                            $ Debuff (EnemyType Humanoid) 1 Charm Full ]
                }
              , { name:   "Golden Rule A"
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 20.0 ~ 50.0 ]
                }
              ]
  , passives: [magicResistance E, independentAction A, divinity B]
  , phantasm: { name:   "Gate of Babylon"
              , desc:   "King's Treasure"
              , rank:   BPlus
              , card:   Buster
              , kind:   "Anti-Unit"
              , hits:   10
              , effect: [ To Enemies Damage $ 100.0 ~ 500.0 ]
              , over:   [ Debuff Enemies 1 NPDown $ 50.0 ~ 90.0
                        , Debuff Enemies 1 CritDown $ 50.0 ~ 90.0
                        , Debuff Enemies 1 DebuffVuln $ 20.0 ~ 40.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 153, starRate: 7.9, npAtk: 0.62, npDef: 3 }
  , hits:     { arts: 3, buster: 3, quick: 3, ex: 3 }
  , traits:   [Male, Divine, EnumaElish, King]
  , death:    36.0
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Billy the Kid"
  , id:       105
  , rarity:   3
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1279,  hp: 1711 }
              , max:   { atk: 6890, hp: 9506 }
              , grail: { atk: 9325, hp: 12889 }
              }
  , actives:  [ { name:   "Marksmanship A++"
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 1 CritUp $ 60.0 ~ 120.0 ]
                }
              , { name:   "Quick Draw A+"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0 ]
                }
              , { name:   "Mind's Eye (Fake) C"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 16.0 ~ 32.0
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
              , effect: [ Grant Self 1 SureHit Full
                        , To Enemy Damage $ 1200.0 ~ 2000.0
                        ]
              , over:   [ Debuff Enemy 3 CritChance $ 10.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 150, starRate: 8.0, npAtk: 0.56, npDef: 3 }
  , hits:     { arts: 3, buster: 4, quick: 2, ex: 4 }
  , traits:   [Male, Riding, EnumaElish]
  , death:    45.0
  , align:    Chaotic:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Tawara Touta"
  , id:       125
  , rarity:   3
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1306,  hp: 1764 }
              , max:   { atk: 7032, hp: 9800 }
              , grail: { atk: 9517, hp: 13287 }
              }
  , actives:  [ { name:   "Protection of the Dragon King C"
                , icon:   IconBusterUp
                , cd:     8
                , effect: [ Grant Self 3 BusterUp $ 20.0 ~ 30.0 
                          , To Self Heal $ 1000.0 ~ 2000.0
                          ]
                }
              , { name:   "Protection from Arrows C"
                , icon:   IconDodge
                , cd:     7
                , effect: [ Grant Self 0 Evasion $ Flat 2.0 
                          , Grant Self 3 DefenseUp $ 7.0 ~ 14.0
                          ]
                }
              , { name:   "Inexhaustible Straw Bag EX"
                , icon:   IconHPUp
                , cd:     7
                , effect: [ Grant Party 3 MaxHP $ 1000.0 ~ 2000.0 ]
                }
              ]
  , passives: [magicResistance C, independentAction B]
  , phantasm: { name:   "Hachiman Prayer"
              , desc:   "Hachiman Prayer—Shooting Through a Great Demon"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ Grant Self 1 (AttackUpVs Demonic) $ 50.0 ~ 100.0 ]
              , first:  true
              }
  , gen:      { starWeight: 150, starRate: 7.8, npAtk: 0.57, npDef: 3 }
  , hits:     { arts: 3, buster: 1, quick: 2, ex: 3 }
  , traits:   [Male, Brynhild, EnumaElish]
  , death:    36.0
  , align:    Neutral:Good
  , limited:  false
  , free:     false
  }
]
