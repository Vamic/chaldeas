module Database.Servant.Saber (sabers) where

import Prelude
import Operators
import Database.Model

sabers :: Array Servant
sabers = Servant <$>
[ { name:     "Okita Souji"
  , id:       68
  , rarity:   5
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    10
  , stats:    { base:  { atk: 1865,  hp: 1939 }
              , max:   { atk: 12068, hp: 13225 }
              , grail: { atk: 13210, hp: 14489 }
              }
  , actives:  [ { name:   "Shukuchi B"
                , icon:   IconQuickUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Quick) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Weak Constitution A"
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 1 StarAbsorb $ 500.0 ~ 1000.0 ]
                }
              , { name:   "Mind's Eye (Fake) A"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 20.0 ~ 40.0
                          ]
                }
              ]
  , passives: [magicResistance E, riding E]
  , phantasm: { name:   "Mumyou Sandanzuki"
              , desc:   "Three-Stage Thrust"
              , rank:   Unknown
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   3
              , effect: [ To Enemy DamageThruDef $ 1200.0 ~ 2000.0 ]
              , over:   [ Debuff Enemy 3 DefenseDown $ 30.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 10.2, npAtk: 1.09, npDef: 3 }
  , hits:     { quick: 5, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, Riding, Saberface, EnumaElish]
  , death:    35.0
  , align:    Neutral:Balanced
  , limited:  true
  , free:     false
  }
, { name:     "Altera"
  , id:       8
  , rarity:   5
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    5
  , stats:    { base:  { atk: 1907,  hp: 2039 }
              , max:   { atk: 12343, hp: 13907 }
              , grail: { atk: 13511, hp: 15236 }
              }
  , actives:  [ { name:   "Tactics B"
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Natural Body D"
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 OffensiveResist $ 40.0 ~ 80.0
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              , { name:   "Crest of the Star EX"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 10.0 ~ 30.0
                          , To Party GainStars $ 5.0 ~ 15.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding A, divinity B]
  , phantasm: { name:   "Photon Ray"
              , desc:   "Sword of the God of War"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage $ 400.0 ~ 600.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 10.1, npAtk: 0.84, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, Riding, Divine, EnumaElish, King]
  , death:    24.5
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Nero Claudius (Bride)"
  , id:       90
  , rarity:   5
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    30
  , stats:    { base:  { atk: 1793,  hp: 2089 }
              , max:   { atk: 11607, hp: 14284 }
              , grail: { atk: 12706, hp: 15609 }
              }
  , actives:  [ { name:   "Stars for the Sky A"
                , icon:   IconNobleUp
                , cd:     9
                , effect: [ Grant Ally 3 NPGen $ 35.0 ~ 45.0 ]
                }
              , { name:   "Flowers for the Earth A"
                , icon:   IconSwordUp
                , cd:     8
                , effect: [ Grant Ally 3 AttackUp $ 30.0 ~ 40.0
                          , Grant Ally 3 StarUp $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Love for the People A"
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Ally Heal $ 1000.0 ~ 3000.0
                          , Grant Ally 3 DefenseUp $ 10.0 ~ 20.0
                          ]
                }
              ]
  , passives: [magicResistance C, riding B]
  , phantasm: { name:   "Fax Caelestis"
              , desc:   "Closing Rose That Fames Stars"
              , rank:   BPlus
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   2
              , effect: [ To Enemy Damage $ 900.0 ~ 1500.0 ]
              , over:   [ Debuff Enemy 5 Burn $ 500.0 ~ 900.0
                        , Debuff Enemy 5 DefenseDown $ 20.0 ~ 40.0
                        , Debuff Enemy 5 CritChance $ 20.0 ~ 40.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 10.1, npAtk: 0.7, npDef: 3 }
  , hits:     { quick: 3, arts: 3, buster: 1, ex: 5 }
  , traits:   [Female, Riding, Saberface, Roman, EnumaElish, King]
  , death:    35.0
  , align:    Chaotic:Bride
  , limited:  true
  , free:     false
  }
, { name:     "Mordred"
  , id:       76
  , rarity:   5
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    25
  , stats:    { base:  { atk: 1811,  hp: 2153 }
              , max:   { atk: 11723, hp: 14680 }
              , grail: { atk: 12833, hp: 16083 }
              }
  , actives:  [ { name:   "Mana Burst A"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Intuition B"
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Secret of Pedigree EX"
                , icon:   IconShieldUp
                , cd:     8
                , effect: [ Grant Self 1 DefenseUp $ 30.0 ~ 50.0
                          , To Self RemoveDebuffs Full
                          , To Self GaugeUp $ 10.0 ~ 30.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding B]
  , phantasm: { name:   "Clarent Blood Arthur A+"
              , desc:   "Rebellion Against My Beautiful Father"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage $ 400.0 ~ 600.0 ]
              , over:   [ To Enemies (DamageVs Arthur) $ 180.0 ~ 220.0
                        , To Self GaugeUp $ 20.0 ~ 40.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 10.0, npAtk: 0.56, npDef: 3 }
  , hits:     { quick: 2, arts: 3, buster: 1, ex: 4 }
  , traits:   [Female, Riding, Dragon, Saberface, EnumaElish]
  , death:    24.5
  , align:    Chaotic:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Ryougi Shiki (Saber)"
  , id:       91
  , rarity:   5
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    15
  , stats:    { base:  { atk: 1656,  hp: 2266 }
              , max:   { atk: 10721, hp: 15453 }
              , grail: { atk: 11736, hp: 16929 }
              }
  , actives:  [ { name:   "Mystic Eyes of Death Perception C"
                , icon:   IconMystic
                , cd:     7
                , effect: [ Grant Self 1 IgnoreInvinc Full
                          , Grant Self 1 (Performance Arts) $ 25.0 ~ 40.0
                          , Debuff Enemies 1 DeathDown $ 60.0 ~ 80.0
                          ]
                }
              , { name:   "Unyou B"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 15.0 ~ 25.0
                          , Grant Self 3 DebuffResist $ 18.0 ~ 36.0
                          ]
                }
              , { name:   "Yin-Yang A"
                , icon:   IconYinYang
                , cd:     6
                , effect: [ To Self Heal $ 2000.0 ~ 5000.0
                          , To Self DemeritGauge $ Flat 10.0
                          ]
                }
              ]
  , passives: [magicResistance A, connectionRoot A, independentManifestation C]
  , phantasm: { name:   "Amalavijñāna—Boundary of Emptiness"
              , desc:   "Mukushiki—Kara no Kyoukai"
              , rank:   EX
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   1
              , effect: [ To Enemies DamageThruDef $ 450.0 ~ 750.0
                        , To Party RemoveDebuffs Full
                        ]
              , over:   [ To Enemies Kill $ 60.0 ~ 100.0 ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 9.9, npAtk: 0.84, npDef: 3 }
  , hits:     { quick: 4, arts: 2, buster: 1, ex: 4 }
  , traits:   [Female, EnumaElish, PseudoServant]
  , death:    24.5
  , align:    Neutral:Balanced
  , limited:  true
  , free:     false
  }
, { name:     "Altria Pendragon"
  , id:       2
  , rarity:   5
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    5
  , stats:    { base:  { atk: 1734,  hp: 2222 }
              , max:   { atk: 11221, hp: 15150 }
              , grail: { atk: 12283, hp: 16597 }
              }
  , actives:  [ { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Mana Burst A"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Intuition A"
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 5.0 ~ 15.0 ]
                }
              ]
  , passives: [magicResistance A, riding B]
  , phantasm: { name:   "Excalibur"
              , desc:   "Sword of Promised Victory"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   1
              , effect: [ To Enemies Damage $ 400.0 ~ 600.0 ]
              , over:   [ To Self GaugeUp $ 20.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 10.0, npAtk: 0.86, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, Riding, Dragon, Saberface, Arthur, EnumaElish, King]
  , death:    21.0
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
, { name:     "Lancelot (Saber)"
  , id:       121
  , rarity:   4
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    14
  , stats:    { base:  { atk: 1658,  hp: 1854 }
              , max:   { atk: 9949, hp: 11589 }
              , grail: { atk: 12046, hp: 14051 }
              }
  , actives:  [ { name:   "Knight of the Lake A"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ Flat 30.0
                          , To Party GainStars $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Eternal Arms Mastery A+"
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 3 StarAbsorb $ 3000.0 ~ 6000.0 ]
                }
              , { name:   "Knight of Owner A++"
                , icon:   IconStarTurn
                , cd:     8
                , effect: [ Grant Party 3 StarsPerTurn $ 5.0 ~ 15.0
                          , Grant Self 3 CritUp $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding B]
  , phantasm: { name:   "Arondight Overload"
              , desc:   "Breaking All Restraints - Overloaded Light of the Lake"
              , rank:   A
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ Grant Self 1 (Performance Arts) $ Flat 30.0
                        , To Enemy Damage $ 900.0 ~ 1500.0
                        ]
              , over:   [ Debuff Enemy 5 DamageVuln $ 1000.0 ~ 3000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 100, starRate: 10.0, npAtk: 0.83, npDef: 3 }
  , hits:     { quick: 3, arts: 2, buster: 4, ex: 5 }
  , traits:   [Male, Brynhild, Riding, EnumaElish]
  , death:    28.0
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
, { name:     "Altria Pendragon (Alter)"
  , id:       3
  , rarity:   4
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    14
  , stats:    { base:  { atk: 1708,  hp: 1854 }
              , max:   { atk: 10248, hp: 11589 }
              , grail: { atk: 12408, hp: 14051 }
              }
  , actives:  [ { name:   "Mana Burst A"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Intuition B"
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Charisma E"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 6.0 ~ 12.0 ]
                }
              ]
  , passives: [magicResistance B]
  , phantasm: { name:   "Excalibur Morgan"
              , desc:   "Sword of Promised Victory"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   3
              , effect: [ To Enemies Damage $ 450.0 ~ 650.0 ]
              , over:   [ To Self GaugeUp $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 99, starRate: 9.9, npAtk: 0.86, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, King, Dragon, Saberface, Arthur, EnumaElish]
  , death:    19.2
  , align:    Lawful:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Rama"
  , id:       101
  , rarity:   4
  , class:    Saber
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    24
  , stats:    { base:  { atk: 1642,  hp: 1901 }
              , max:   { atk: 9854, hp: 11882 }
              , grail: { atk: 11931, hp: 14541 }
              }
  , actives:  [ { name:   "Blessing of Martial Arts A"
                , icon:   IconStarUp
                , cd:     6
                , effect: [ Grant Self 1 StarAbsorb $ 300.0 ~ 500.0
                          , Grant Self 1 CritUp $ 50.0 ~ 100.0
                          ]
                }
              , { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Curse of Separation A"
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 3 Guts $ Flat 1000.0
                          , To Self Heal $ 1000.0 ~ 3000.0
                          ]
                }
              ]
  , passives: [magicResistance A, riding APlus, divinity A]
  , phantasm: { name:   "Brahmastra"
              , desc:   "Rakshasa-Piercing Immortal"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Demon"
              , hits:   5
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0 ]
              , over:   [ To Enemy (DamageVs Demonic) $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 100, starRate: 10.2, npAtk: 0.84, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 4 }
  , traits:   [Male, Riding, Brynhild, Divine, EnumaElish, King]
  , death:    24.5
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
, { name:     "Nero Claudius"
  , id:       5
  , rarity:   4
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    15
  , stats:    { base:  { atk: 1574,  hp: 1880 }
              , max:   { atk: 9449, hp: 11753 }
              , grail: { atk: 11441, hp: 14250 }
              }
  , actives:  [ { name:   "Migraine B"
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 MentalResist $ 50.0 ~ 100.0
                          , To Self Heal $ 500.0 ~ 2000.0
                          ]
                }
              , { name:   "Imperial Privilege EX"
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Self Heal $ 1200.0 ~ 3400.0
                          , Chance 60 <<< Grant Self 3 AttackUp $ 22.0 ~ 44.0
                          , Chance 60 <<< Grant Self 3 DefenseUp $ 22.0 ~ 44.0
                          ]
                }
              , { name:   "Invictus Spiritus A"
                , icon:   IconKneel
                , cd:     12
                , effect: [ Times 3 <<< Grant Self 5 Guts $ 300.0 ~ 600.0 ]
                }
              ]
  , passives: [magicResistance C, riding B]
  , phantasm: { name:   "Laus Saint Claudius"
              , desc:   "Imperium of the Maiden's Flowery Words"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Encampment"
              , hits:   1
              , effect: [ To Enemies DamageThruDef $ 600.0 ~ 900.0 ]
              , over:   [ Debuff Enemies 1 DefenseDown $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 10.1, npAtk: 0.84, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 5 }
  , traits:   [Female, Riding, Roman, Saberface, EnumaElish, King]
  , death:    24.5
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Chevalier d'Eon"
  , id:       10
  , rarity:   4
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    9
  , stats:    { base:  { atk: 1734,  hp: 2121 }
              , max:   { atk: 8765, hp: 13256 }
              , grail: { atk: 10613, hp: 16073 }
              }
  , actives:  [ { name:   "Mind's Eye (True) C"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 DefenseUp $ 8.0 ~ 16.0
                          ]
                }
              , { name:   "Self-Suggestion A"
                , icon:   IconBubbles
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , Grant Self 3 DebuffResist $ 50.0 ~ 100.0
                          ]
                }
              , { name:   "Beautiful Appearance C"
                , icon:   IconFace
                , cd:     7
                , effect: [ Grant Self 3 Taunt Full
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              ]
  , passives: [magicResistance C, riding B]
  , phantasm: { name:   "Fleur de Lis"
              , desc:   "Gorgeous Blooming Lillies"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Debuff Enemies 2 AttackDown $ 10.0 ~ 30.0
                        , Debuff Enemies 2 DefenseDown $ 10.0 ~ 30.0
                        ]
              , over:   [ Chances 10 50 $ Debuff Enemies 1 Charm Full ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 10.0, npAtk: 0.83, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Riding, EnumaElish]
  , death:    28.0
  , align:    Neutral:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Siegfried"
  , id:       6
  , rarity:   4
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    4
  , stats:    { base:  { atk: 1363,  hp: 2266 }
              , max:   { atk: 8181, hp: 14165 }
              , grail: { atk: 9905, hp: 17175 }
              }
  , actives:  [ { name:   "Golden Rule C-"
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 15.0 ~ 37.5 ]
                }
              , { name:   "Disengage A"
                , icon:   IconBubbles
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              , { name:   "Dragon-Slayer A"
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 3 (AttackVs Dragon) $ 50.0 ~ 80.0
                          , Grant Self 3 (DefenseVs Dragon) $ Flat 30.0
                          ]
                }
              ]
  , passives: [riding B]
  , phantasm: { name:  "Balmung"
              , desc:   "Phantasmal Greatsword, Felling of the Sky Demon"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ To Enemies (DamageVs Dragon) $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 97, starRate: 10.0, npAtk: 0.83, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Riding, Brynhild, Dragon, EnumaElish, King]
  , death:    28.0
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Gawain"
  , id:       123
  , rarity:   4
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    4
  , stats:    { base:  { atk: 1695,  hp: 1827 }
              , max:   { atk: 10173, hp: 11419 }
              , grail: { atk: 12317, hp: 13845 }
              }
  , actives:  [ { name:   "Numeral of The Saint EX"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ Flat 20.0
                          , When "on Open Field with Sunshine" <<<
                            Grant Self 3 (Performance Buster) $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Charisma E"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 6.0 ~ 12.0 ]
                }
              , { name:   "Belt of Bertilak EX"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ Flat 20.0
                          , To Party GainStars $ 5.0 ~ 10.0
                          , Times 1 <<< Grant Self 1 Guts $ 1000.0 ~ 2000.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding B]
  , phantasm: { name:   "Excalibur Galatine"
              , desc:   "The Reborn Sword of Victory"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   4
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0
                        , Debuff Enemies 1 SealSkills Full
                        ]
              , over:   [ Debuff Enemies 5 Burn $ 1000.0 ~ 5000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 10.0, npAtk: 1.14, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 5 }
  , traits:   [Male, Brynhild, Riding, EnumaElish]
  , death:    21.0
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
, { name:     "Altria Pendragon (Lily)"
  , id:       4
  , rarity:   4
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    14
  , stats:    { base:  { atk: 1287,  hp: 1699 }
              , max:   { atk: 7726, hp: 10623 }
              , grail: { atk: 9355, hp: 12880 }
              }
  , actives:  [ { name:   "Intuition B"
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Mana Burst A"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Journey of the Flowers EX"
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Party 3 NPGen $ 10.0 ~ 20.0 ]
                }
              ]
  , passives: [magicResistance B, riding C]
  , phantasm: { name:   "Caliburn"
              , desc:   "Golden Sword of the Victorious"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   8
              , effect: [ To Enemies Damage $ 300.0 ~ 600.0 ]
              , over:   [ To Self Heal $ 1000.0 ~ 5000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 10.0, npAtk: 0.86, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, Riding, Dragon, Saberface, Arthur, EnumaElish, King]
  , death:    21.0
  , align:    Lawful:Good
  , limited:  true
  , free:     true
  }
, { name:     "Gaius Julius Caesar"
  , id:       7
  , rarity:   3
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    13
  , stats:    { base:  { atk: 1392,  hp: 1727 }
              , max:   { atk: 7497, hp: 9595 }
              , grail: { atk: 10146, hp: 13009 }
              }
  , actives:  [ { name:   "Tactics B"
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Charisma C"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 8.0 ~ 16.0 ]
                }
              , { name:   "Incite EX"
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Ally 3 CritUp $ 20.0 ~ 40.0
                          , Debuff Ally 3 DefenseDown $ Flat 20.0
                          ]
                }
              ]
  , passives: [magicResistance C, riding B, divinity D]
  , phantasm: { name:   "Crocea Mors"
              , desc:   "Yellow Death"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   10
              , effect: [ To Enemy Damage $ 1200.0 ~ 2000.0 ]
              , over:   [ To Party GainStars $ 5.0 ~ 25.0 ]
              , first:  false
              }
  , gen:      { starWeight: 99, starRate: 10.0, npAtk: 1.1, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, King, Riding, Brynhild, Roman, Divine, EnumaElish]
  , death:    31.5
  , align:    Neutral:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Fergus mac Roich"
  , id:       72
  , rarity:   3
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    23
  , stats:    { base:  { atk: 1385,  hp: 1761 }
              , max:   { atk: 7460, hp: 9786 }
              , grail: { atk: 10096, hp: 13268 }
              }
  , actives:  [ { name:   "Valor A"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 10.0 ~ 30.0
                          , Grant Self 3 MentalResist $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Defiant B"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 15.0 ~ 25.0
                          , Debuff Self 3 BuffFail $ Flat 50.0
                          ]
                }
              , { name:   "Mind's Eye (True) A"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 DefenseUp $ 10.0 ~ 20.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding B]
  , phantasm: { name:   "Caladbolg"
              , desc:   "Rainbow Sword"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage $ 400.0 ~ 600.0
                        , Debuff Enemies 3 DefenseDown $ Flat 20.0
                        ]
              , over:   [ Debuff Enemies 3 DebuffVuln $ 20.0 ~ 60.0 ]
              , first:  false
              }
  , gen:      { starWeight: 99, starRate: 10.0, npAtk: 1.09, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Riding, Brynhild, EnumaElish, King]
  , death:    35.0
  , align:    Lawful:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Bedivere"
  , id:       126
  , rarity:   3
  , class:    Saber
  , attr:     Star
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    13
  , stats:    { base:  { atk: 1416,  hp: 1727 }
              , max:   { atk: 7627, hp: 9595 }
              , grail: { atk: 10322, hp: 13009 }
              }
  , actives:  [ { name:   "Tactics C"
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 8.0 ~ 16.0 ]
                }
              , { name:   "Calm and Collected B"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ Flat 30.0
                          , Grant Self 3 DebuffResist $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Oath of Protection B"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Party 1 DefenseUp $ Flat 30.0
                          , Grant Self 1 DebuffResist $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding A]
  , phantasm: { name:   "Switch On—Airgetlám"
              , desc:   "Take Up a Sword, Silver-Colored Arm"
              , rank:   C
              , card:   Buster
              , kind:   "Anti-Unit"
              , hits:   2
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0 ]
              , over:   [ Grant Self 1 (Performance Buster) $ 30.0 ~ 70.0 ]
              , first:  true
              }
  , gen:      { starWeight: 100, starRate: 10.2, npAtk: 1.11, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 5 }
  , traits:   [Male, Brynhild, Riding, EnumaElish]
  , death:    28.0
  , align:    Lawful:Good
  , limited:  false
  , free:     true
  }
, { name:     "Gilles de Rais"
  , id:       9
  , rarity:   3
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    13
  , stats:    { base:  { atk: 1228,  hp: 1889 }
              , max:   { atk: 6615, hp: 10498 }
              , grail: { atk: 8952, hp: 14234 }
              }
  , actives:  [ { name:   "Tactics C"
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 8.0 ~ 16.0 ]
                }
              , { name:   "Golden Rule B"
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 18.0 ~ 45.0 ]
                }
              , { name:   "Prelati's Encouragement B"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 5 (Performance Buster) $ 20.0 ~ 40.0 ]
                }
              ]
  , passives: [magicResistance B, riding B, madness EX]
  , phantasm: { name:   "Saint War Order"
              , desc:   "Rally Thy War Cries Under the Holy Flag"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   0
              , effect: [ Grant Self 2 AttackUp $ 50.0 ~ 100.0
                        , Debuff Self 3 DefenseDown $ Flat 50.0
                        ]
              , over:   [ To Party GainStars $ 5.0 ~ 25.0 ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 9.9, npAtk: 0.82, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Riding, Brynhild, EnumaElish]
  , death:    31.5
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
]
