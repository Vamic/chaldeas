module Database.Servant.Rider (riders) where

import StandardLibrary
import Database.Model

riders :: Array Servant
riders = Servant <$>
[ { name:     "Ozymandias"
  , id:       118
  , rarity:   5
  , class:    Rider
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    5
  , stats:    { base:  { atk: 1850,  hp: 1881 }
              , max:   { atk: 11971, hp: 12830 }
              , grail: { atk: 13104, hp: 14056 }
              }
  , skills:   [ { name:   "Charisma"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Imperial Privilege"
                , rank:   A
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Self Heal $ 1000.0 ~ 3000.0
                          , Chance 60 <<< Grant Self 3 AttackUp $ 20.0 ~ 40.0
                          , Chance 60 <<< Grant Self 3 DefenseUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Protection of the Sun God"
                , rank:   A
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Party GaugeUp $ Flat 20.0
                          , Grant Party 1 BuffUp $ 20.0 ~ 40.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding APlus, divinity B]
  , phantasm: { name:   "Ramesseum Tentyris"
              , desc:   "The Shining Great Temple Complex"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   5
              , effect: [ To Enemy Damage $ 600.0 ~  1000.0
                        , Debuff Enemy 1 SealNP Full
                        ]
              , over:   [ Debuff Enemy 3 DefenseDown $ 20.0 ~ 60.0 ]
              , first:  false
              }
  , gen:      { starWeight: 205, starRate: 9.0, npAtk: 0.59, npDef: 3 }
  , hits:     { quick: 5, arts: 3, buster: 1, ex: 5 }
  , traits:   [Male, Brynhild, Divine, Riding, EnumaElish, King]
  , death:    30.0
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  }
, { name:     "Francis Drake"
  , id:       65
  , rarity:   5
  , class:    Rider
  , attr:     Star
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    10
  , stats:    { base:  { atk: 1750,  hp: 1881 }
              , max:   { atk: 11326, hp: 12830 }
              , grail: { atk: 12398, hp: 14056 }
              }
  , skills:   [ { name:   "Voyager of the Storm"
                , rank:   APlus
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 8.5 ~ 17.0
                          , Grant Party 1 AttackUp $ 8.5 ~ 17.0
                          ]
                }
              , { name:   "Golden Rule"
                , rank:   B
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 18.0 ~ 45.0 ]
                }
              , { name:   "Pioneer of the Stars"
                , rank:   EX
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0
                          , Grant Self 3 IgnoreInvinc Full
                          , To Party GainStars $ Flat 10.0
                          ]
                }
              ]
  , passives: [magicResistance D, riding D]
  , phantasm: { name:   "Golden Wild Hunt"
              , desc:   "Golden Stag and the Eventide Tempest"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage $ 400.0 ~ 600.0 ]
              , over:   [ To Party GainStars $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 208, starRate: 9.0, npAtk: 0.42, npDef: 3 }
  , hits:     { quick: 6, arts: 4, buster: 2, ex: 4 }
  , traits:   [Female, Riding]
  , death:    50.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  }
, { name:     "Iskandar"
  , id:       108
  , rarity:   5
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    10
  , stats:    { base:  { atk: 1786,  hp: 1938 }
              , max:   { atk: 11560, hp: 13219 }
              , grail: { atk: 12654, hp: 14482 }
              }
  , skills:   [ { name:   "Charisma"
                , rank:   A
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.0 ~ 20.0 ]
                }
              , { name:   "Tactics"
                , rank:   B
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Lightning Conqueror"
                , rank:   EX
                , icon:   IconBusterUp
                , cd:     8
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0
                          , Grant Self 1 StarUp $ Flat 50.0
                          ]
                }
              ]
  , passives: [magicResistance D, riding APlus, divinity C]
  , phantasm: { name:   "Ionioi Hetairoi"
              , desc:   "Army of the King"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   8
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 205, starRate: 8.8, npAtk: 0.66, npDef: 3 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 6 }
  , traits:   [Male, Brynhild, Divine, GreekMythMale, King, Riding, EnumaElish]
  , death:    40.0
  , align:    [Neutral, Good]
  , limited:  true
  , free:     false
  }
, { name:     "Queen Medb"
  , id:       99
  , rarity:   5
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    5
  , stats:    { base:  { atk: 1591,  hp: 2048 }
              , max:   { atk: 10296, hp: 13968 }
              , grail: { atk: 11270, hp: 15303 }
              }
  , skills:   [ { name:   "Golden Rule (Body)"
                , rank:   A
                , icon:   IconHealTurn
                , cd:     8
                , effect: [ Grant Self 3 DebuffResist Full
                          , Grant Self 3 HealPerTurn $ 500.0 ~ 1000.0
                          , Grant Self 3 GaugePerTurn $ Flat 10.0
                          ]
                }
              , { name:   "Charisma"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Siren Song"
                , rank:   C
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 50 80 $
                            Debuff (EnemyType Male) 1 Charm Full ]
                }
              ]
  , passives: [magicResistance B, riding A]
  , phantasm: { name:   "Chariot My Love"
              , desc:   "My Dear Iron Chariot"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0 ]
              , over:   [ To Enemy (DamageVs Male) $ 150.0 ~ 200.0
                        , Debuff Enemy 3 MentalVuln $ 10.0 ~ 50.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 208, starRate: 9.0, npAtk: 0.86, npDef: 3 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 7 }
  , traits:   [Female, Riding, EnumaElish, King]
  , death:    40.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  }
, { name:     "Sakata Kintoki (Rider)"
  , id:       115
  , rarity:   4
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    9
  , stats:    { base:  { atk: 1636,  hp: 1728 }
              , max:   { atk: 9819, hp: 10800 }
              , grail: { atk: 11889, hp: 13095 }
              }
  , skills:   [ { name:   "Long-Distance Dash"
                , rank:   A
                , icon:   IconQuickUp
                , cd:     7
                , effect: [ Grant Self 3 (Performance Quick) $ 20.0 ~ 30.0
                          , Grant Self 3 StarUp $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Animal Communication"
                , rank:   C
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0 ]
                }
              , { name:   "Natural Body"
                , rank:   A
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 OffensiveResist $ 60.0 ~ 120.0
                          , To Self Heal $ 1000.0 ~ 3000.0
                          ]
                }
              ]
  , passives: [divinity C]
  , phantasm: { name:   "Golden Drive—Good Night"
              , desc:   "Night Wolf Deadly Nine—Golden Dash"
              , rank:   B
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   4
              , effect: [ To Enemy Damage $ 1200.0 ~ 2000.0 ]
              , over:   [ Grant Self 1 (Performance Quick) $ 10.0 ~ 90.0 ]
              , first:  false
              }
  , gen:      { starWeight: 198, starRate: 9.0, npAtk: 1.15, npDef: 3 }
  , hits:     { quick: 4, arts: 2, buster: 1, ex: 5 }
  , traits:   [Male, Brynhild, Divine, EnumaElish]
  , death:    40.0
  , align:    [Lawful, Good]
  , limited:  true
  , free:     true
  }
, { name:     "Altria Pendragon (Santa Alter)"
  , id:       73
  , rarity:   4
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    29
  , stats:    { base:  { atk: 1543,  hp: 1805 }
              , max:   { atk: 9258, hp: 11286 }
              , grail: { atk: 11209, hp: 13684 }
              }
  , skills:   [ { name:   "Saint's Gift"
                , rank:   EX
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Ally Heal $ 1500.0 ~ 3500.0
                          , Grant Ally 3 StarUp $ Flat 30.0
                          ]
                }
              , { name:   "Intuition"
                , rank:   A
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 5.0 ~ 15.0 ]
                }
              , { name:   "Mana Burst"
                , rank:   AMinus
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 25.0 ~ 45.0 ]
                }
              ]
  , passives: [magicResistance A, riding A]
  , phantasm: { name:   "Excalibur Morgan"
              , desc:   "Sword of Promised Victory"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage $ 450.0 ~ 650.0 ]
              , over:   [ To Self GaugeUp $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 204, starRate: 8.9, npAtk: 0.87, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, Arthur, Dragon, Riding, Saberface, EnumaElish, King]
  , death:    35.0
  , align:    [Lawful, Good]
  , limited:  true
  , free:     true
  }
, { name:     "Martha"
  , id:       30
  , rarity:   4
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , curve:    4
  , stats:    { base:  { atk: 1335,  hp: 2090 }
              , max:   { atk: 8014, hp: 13068 }
              , grail: { atk: 9703, hp: 15845 }
              }
  , skills:   [ { name:   "Protection of the Faith"
                , rank:   A
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 DebuffResist $ 50.0 ~ 100.0
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              , { name:   "Miracle"
                , rank:   DPlus
                , icon:   IconHeal
                , cd:     8
                , effect: [ To Party Heal $ 1000.0 ~ 2000.0
                          , To Party RemoveDebuffs Full
                          ]
                }
              , { name:   "Oath of the Holy Maiden"
                , rank:   C
                , icon:   IconShieldDown
                , cd:     7
                , effect: [ Debuff Enemy 3 DefenseDown $ 10.0 ~ 30.0
                          , To Enemy RemoveBuffs Full
                          ]
                }
              ]
  , passives: [magicResistance A, riding APlusPlus, divinity C]
  , phantasm: { name:   "Tarasque"
              , desc:   "O Tragic Drake Who Knew Naught of Love"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 205, starRate: 9.0, npAtk: 1.58, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Female, Riding, Divine, EnumaElish]
  , death:    30.0
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  }
, { name:     "Mordred (Rider)"
  , id:       132
  , rarity:   4
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    24
  , stats:    { base:  { atk: 1535,  hp: 1824 }
              , max:   { atk: 9212, hp: 11400 }
              , grail: { atk: 11154, hp: 13822 }
              }
  , skills:   [ { name:   "Cerulean Ride"
                , rank:   A
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Self 3 (Performance Arts) $ 20.0 ~ 30.0 ]
                }
              , { name:   "Rodeo Flip"
                , rank:   APlus
                , icon:   IconDodge
                , cd:     9
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 1 StarUp $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Endless Summer"
                , rank:   B
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 3 Guts $ Flat 1000.0
                          , To Self GaugeUp $ 20.0 ~ 30.0
                          ]
                }
              ]
  , passives: [magicResistance B, surfing A]
  , phantasm: { name:   "Prydwen Tube Riding"
              , desc:   "Kingly Mood of Reining in Surging Waves!"
              , rank:   A
              , card:   Arts
              , kind:   "Anti-Wave"
              , hits:   5
              , effect: [ To Enemies Damage $ 450.0 ~ 750.0 ]
              , over:   [ Chances 50 90 <<< To Enemies GaugeDown $ Flat 1.0 ]
              , first:  false
              }
  , gen:      { starWeight: 204, starRate: 9.2, npAtk: 0.71, npDef: 3 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 4 }
  , traits:   [Female, Dragon, Saberface, EnumaElish]
  , death:    35.0
  , align:    [Chaotic, Good]
  , limited:  true
  , free:     false
  }
, { name:     "Marie Antoinette"
  , id:       29
  , rarity:   4
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    9
  , stats:    { base:  { atk: 1382,  hp: 1975 }
              , max:   { atk: 8293, hp: 12348 }
              , grail: { atk: 10041, hp: 14972 }
              }
  , skills:   [ { name:   "Siren Song"
                , rank:   C
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 50 80 $
                            Debuff (EnemyType Male) 1 Charm Full ]
                }
              , { name:   "Beautiful Princess"
                , rank:   A
                , icon:   IconShield
                , cd:     8
                , effect: [ Times 3 $ Grant Self 0 Invincibility Full
                          , Grant Self 5 HealPerTurn $ 200.0 ~ 600.0
                          ]
                }
              , { name:   "God's Grace"
                , rank:   B
                , icon:   IconStaffUp
                , cd:     7
                , effect: [ Grant Self 3 MentalSuccess $ 10.0 ~ 30.0
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              ]
  , passives: [magicResistance C, riding APlus]
  , phantasm: { name:   "Guillotine Breaker"
              , desc:   "Glory to the Crown of Lillies"
              , rank:   A
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage $ 600.0 ~ 1000.0
                        , To Party RemoveDebuffs Full
                        ]
              , over:   [ To Party Heal $ 500.0 ~ 2500.0 ]
              , first:  false
              }
  , gen:      { starWeight: 201, starRate: 9.0, npAtk: 1.0, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Female, Riding, EnumaElish]
  , death:    35.0
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  }
, { name:     "Anne Bonny & Mary Read"
  , id:       66
  , rarity:   4
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    29
  , stats:    { base:  { atk: 1504,  hp: 1805 }
              , max:   { atk: 9029, hp: 11286 }
              , grail: { atk: 10932, hp: 13684 }
              }
  , skills:   [ { name:   "Voyage"
                , rank:   A
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 StarUp $ 30.0 ~ 50.0 ]
                }
              , { name:   "Marksmanship"
                , rank:   B
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 1 CritUp $ 50.0 ~ 100.0 ]
                }
              , { name:   "Combination"
                , rank:   C
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 1 StarAbsorb $ 300.0 ~ 600.0
                          , Grant Self 1 AttackUp $ 20.0 ~ 30.0
                          ]
                }
              ]
  , passives: [magicResistance D]
  , phantasm: { name:   "Carribbean Free Bird"
              , desc:   "Bond of Lovebirds"
              , rank:   CPlusPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   6
              , effect: [ To Enemy Damage $ 1600.0 ~ 2400.0 ]
              , over:   [ To Enemy LastStand $ 1200.0 ~ 2000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 200, starRate: 9.1, npAtk: 0.84, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 1 }
  , traits:   [Female, EnumaElish]
  , death:    50.0
  , align:    [Chaotic, Evil, Chaotic, Balanced]
  , limited:  false
  , free:     false
  }
, { name:     "Astolfo"
  , id:       94
  , rarity:   4
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    24
  , stats:    { base:  { atk: 1489,  hp: 1787 }
              , max:   { atk: 8937, hp: 11172 }
              , grail: { atk: 10821, hp: 13546 }
              }
  , skills:   [ { name:   "Monstrous Strength"
                , rank:   CMinus
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 1 AttackUp $ 8.0 ~ 28.0 ]
                }
              , { name:   "Trap of Argalia"
                , rank:   D
                , icon:   IconStun
                , cd:     10
                , effect: [ Chances 60 90 $ Debuff Enemy 1 Stun Full ]
                }
              , { name:   "Evaporation of Reason"
                , rank:   D
                , icon:   IconStarTurn
                , cd:     10
                , effect: [ Chance 65 <<< Grant Party 3 StarsPerTurn $ 5.0 ~ 10.0
                          , Chance 65 <<< Grant Self 3 StarUp $ 30.0 ~ 50.0
                          , Chance 65 <<< Grant Self 3 CritUp $ 20.0 ~ 40.0
                          ]
                }
              ]
  , passives: [magicResistance A, riding APlus, independentAction B]
  , phantasm: { name:   "Hippogriff"
              , desc:   "Otherworldly Phantom Horse"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies DamageThruDef $ 600.0 ~ 1000.0
                        , Times 3 $ Grant Self 0 Evasion Full
                        ]
              , over:   [ To Party GainStars $ 5.0 ~ 25.0 ]
              , first:  false
              }
  , gen:      { starWeight: 205, starRate: 9.0, npAtk: 0.66, npDef: 1 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 4 }
  , traits:   [Nonbinary, Riding, EnumaElish]
  , death:    40.0
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  }
, { name:     "Georgios"
  , id:       24
  , rarity:   2
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    2
  , stats:    { base:  { atk: 929,  hp: 1731 }
              , max:   { atk: 5236, hp: 9200 }
              , grail: { atk: 7587, hp: 13278 }
              }
  , skills:   [ { name:   "Guardian Knight"
                , rank:   APlus
                , icon:   IconCrosshairUp
                , cd:     7
                , effect: [ Grant Self 3 Taunt Full
                          , Grant Self 3 DefenseUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Martyr's Soul"
                , rank:   BPlus
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 MentalResist $ 50.0 ~ 100.0
                          , To Self Heal $ 750.0 ~ 2000.0
                          ]
                }
              , { name:   "Battle Continuation"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1000.0 ~ 2500.0 ]
                }
              ]
  , passives: [magicResistance A, riding B]
  , phantasm: { name:   "Ascalon"
              , desc:   "Blessed Sword of Force Subversion"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   4
              , effect: [ Debuff Enemy 3 (ApplyTrait Dragon) Full
                        , To Enemy Damage $ 900.0 ~ 1500.0
                        ]
              , over:   [ Grant Self 1 DefenseUp $ 20.0 ~ 40.0 ]
              , first:  false
             }
  , gen:      { starWeight: 205, starRate: 8.9, npAtk: 0.85, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Riding, Brynhild, EnumaElish]
  , death:    45.0
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  }
, { name:     "Ushiwakamaru"
  , id:       27
  , rarity:   3
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    8
  , stats:    { base:  { atk: 1314, hp: 1625 }
              , max:   { atk: 7076, hp: 9028 }
              , grail: { atk: 9576, hp: 12240 }
              }
  , skills:   [ { name:   "Tengu's Strategy"
                , rank:   A
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Party 3 NPGen $ 10.0 ~ 20.0 ]
                }
              , { name:   "Charisma"
                , rank:   CPlus
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 8.5 ~ 17.0 ]
                }
              , { name:   "Art of the Swallow"
                , rank:   B
                , icon:   IconDodge
                , cd:     8
                , effect: [ Times 1 $ Grant Self 1 Evasion Full
                          , Grant Self 1 StarUp $ 50.0 ~ 100.0
                          ]
                }
              ]
  , passives: [magicResistance C, riding APlus]
  , phantasm: { name:   "Dan-No-Ura Eight-Boat Leap"
              , desc:   "Dan-no-ura • Hassou Tobi"
              , rank:   C
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage $ 1600.0 ~ 2400.0 ]
              , over:   [ Grant Self 3 StarUp $ 50.0 ~ 90.0 ]
              , first:  false
              }
  , gen:      { starWeight: 204, starRate: 9.1, npAtk: 0.87, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, Riding, Brynhild, EnumaElish]
  , death:    35.0
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  }
, { name:     "Medusa"
  , id:       23
  , rarity:   3
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    24
  , stats:    { base:  { atk: 1337,  hp: 1608 }
              , max:   { atk: 7200, hp: 8937 }
              , grail: { atk: 9744, hp: 12117 }
              }
  , skills:   [ { name:   "Mystic Eyes"
                , rank:   APlus
                , icon:   IconStun
                , cd:     8
                , effect: [ Chances 50 100 $ Debuff Enemy 1 Stun Full ]
                }
              , { name:   "Monstrous Strength"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 2 AttackUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Blood Fort Andromeda"
                , rank:   B
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ Flat 20.0
                          , Grant Self 3 NPGen $ 20.0 ~ 30.0
                          ]
                }
              ]
  , passives: [ magicResistance B, independentAction C, divinity EMinus
              , riding APlus
              ]
  , phantasm: { name:   "Bellerophon"
              , desc:   "Bridle of Chivalry"
              , rank:   APlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage $ 600.0 ~ 1000.0 ]
              , over:   [ Grant Party 3 StarUp $ 20.0 ~ 60.0 ]
              , first:  false
              }
  , gen:      { starWeight: 194, starRate: 9.0, npAtk: 0.58, npDef: 3 }
  , hits:     { quick: 2, arts: 3, buster: 1, ex: 3 }
  , traits:   [Female, Riding, Divine]
  , death:    35.0
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  }
, { name:     "Edward Teach"
  , id:       25
  , rarity:   2
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    12
  , stats:    { base:  { atk: 1097,  hp: 1488 }
              , max:   { atk: 6188, hp: 7907 }
              , grail: { atk: 8967, hp: 11411 }
              }
  , skills:   [ { name:   "Voyager of the Storm"
                , rank:   A
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 8.0 ~ 16.0
                          , Grant Party 1 AttackUp $ 8.0 ~ 16.0
                          ]
                }
              , { name:   "Pirate's Glory"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 9.0 ~ 27.0
                          , Times 1 <<< Grant Self 0 Guts $ Flat 1.0
                          , Debuff Self 3 DebuffVuln $ Flat 50.0
                          ]
                }
              , { name:   "Gentlemanly Love"
                , rank:   C
                , icon:   IconHeal
                , cd:     8
                , effect: [ To Party Heal $ 1000.0 ~ 2000.0
                          , To (AlliesType Female) Heal $ 1000.0 ~ 2000.0
                          , To (AlliesType Nonbinary) Heal $ 1000.0 ~ 2000.0
                          ]
                }
              ]
  , passives: [magicResistance E]
  , phantasm: { name:   "Queen Anne's Revenge"
              , desc:   "Revenge of the Queen Anne"
              , rank:   CPlusPlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0
                        , To Party GainStars $ Flat 5.0
                        ]
              , over:   [ Chances 30 70 <<< To Enemies GaugeDown $ Flat 1.0 ]
              , first:  false
              }
  , gen:      { starWeight: 198, starRate: 8.8, npAtk: 0.56, npDef: 3 }
  , hits:     { quick: 2, arts: 3, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    45.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  }
, { name:     "Alexander"
  , id:       28
  , rarity:   3
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    3
  , stats:    { base:  { atk: 1366,  hp: 1979 }
              , max:   { atk: 7356, hp: 8640 }
              , grail: { atk: 9955, hp: 11714 }
              }
  , skills:   [ { name:   "Charisma"
                , rank:   C
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 8.0 ~ 16.0 ]
                }
              , { name:   "Fair Youth"
                , rank:   B
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 50 80 $
                            Debuff (EnemyType Humanoid) 1 Charm Full ]
                }
              , { name:   "Omen of the Conqueror"
                , rank:   A
                , icon:   IconQuickUp
                , cd:     7
                , effect: [ Grant Party 3 (Performance Quick) $ 10.0 ~ 20.0 ]
                }
              ]
  , passives: [magicResistance D, divinity E, riding APlus]
  , phantasm: { name:   "Bucephalus"
              , desc:   "The Beginning of Trampling Conquest"
              , rank:   BPlusPlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage $ 800.0 ~ 1200.0 ]
              , over:   [ To Party GainStars $ 15.0 ~ 35.0 ]
              , first:  false
              }
  , gen:      { starWeight: 205, starRate: 9.0, npAtk: 0.86, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Riding, Divine, EnumaElish, GreekMythMale, King]
  , death:    40.0
  , align:    [Neutral, Good]
  , limited:  false
  , free:     false
  }
, { name:     "Boudica"
  , id:       26
  , rarity:   3
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    3
  , stats:    { base:  { atk: 1168,  hp: 1823 }
              , max:   { atk: 6289, hp: 10130 }
              , grail: { atk: 8511, hp: 13735 }
              }
  , skills:   [ { name:   "Vow to the Goddess"
                , rank:   B
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 3 (AttackVs Roman) $ 40.0 ~ 60.0 ]
                }
              , { name:   "Battle Continuation"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1000.0 ~ 2500.0 ]
                }
              , { name:   "Andraste's Protection"
                , rank:   A
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Party 3 (Performance Arts) $ 10.0 ~ 20.0 ]
                }
              ]
  , passives: [magicResistance D, riding A]
  , phantasm: { name:   "Chariot of Boudica"
              , desc:   "Chariot Without Promised Protection"
              , rank:   BPlus
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Grant Party 3 DefenseUp $ 10.0 ~ 20.0 ]
              , over:   [ Grant Party 1 DefenseUp $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 196, starRate: 8.9, npAtk: 0.85, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, Riding, EnumaElish, King]
  , death:    45.0
  , align:    [Neutral, Good]
  , limited:  false
  , free:     true
  }
]
