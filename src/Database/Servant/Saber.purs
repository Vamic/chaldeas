module Database.Servant.Saber where

import Prelude
import Operators
import Database.Model

sabers ∷ Array Servant
sabers = Servant <$>
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
  , passives: [magicResistance E, riding E]
  , phantasm: { name:   "Mumyou Sandanzuki"
              , desc:   "Three-Stage Thrust"
              , rank:   Unknown
              , card:   Quick 
              , kind:   "Anti-Personnel"
              , hits:   3
              , effect: [ To Enemy DamageThruDef 2000.0 ]
              , over:   [ Debuff Enemy 3 DefenseDown 30.0 ]
              }
  , gen:      { starWeight: 98, starRate: 10.2, npPerHit: 1.09, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 5, ex: 3 }
  , traits:   [Female, Riding, Saberface, EnumaElish]
  , death:    35.0
  , align:    Neutral:Balanced
  , limited:  true
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
              , { name:   "Crest of the Star EX"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 30.0 
                          , To Party GainStars 15.0
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
              , effect: [ To Enemies Damage 600.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown 40.0 ]
              }
  , gen:      { starWeight: 102, starRate: 10.1, npPerHit: 0.84, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, Divine, EnumaElish, King]
  , death:    24.5
  , align:    Chaotic:Good
  , limited:  false
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
                          , Grant Ally 3 StarUp 50.0
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
  , passives: [magicResistance C, riding B]
  , phantasm: { name:   "Fax Caelestis"
              , desc:   "Closing Rose That Fames Stars"
              , rank:   BPlus
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   2
              , effect: [ To Enemy Damage 1500.0 ]
              , over:   [ Debuff Enemy 5 Burn 500.0 
                        , Debuff Enemy 5 DefenseDown 20.0
                        , Debuff Enemy 5 CritChance 20.0
                        ]
              }
  , gen:      { starWeight: 102, starRate: 10.1, npPerHit: 0.7, npPerDefend: 3 }
  , hits:     { a: 3, b: 1, q: 3, ex: 4 }
  , traits:   [Female, Riding, Saberface, Roman, EnumaElish, King]
  , death:    35.0
  , align:    Chaotic:Bride
  , limited:  true
  }
, { name:     "Mordred"
  , rarity:   5
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1811,  hp: 2153 }
              , max:   { atk: 11723, hp: 14680 }
              , grail: { atk: 12833, hp: 16083 }
              }
  , ratings:  { damage:4, np:3, critical:2, utility:1, support:1, durability:4 }
  , actives:  [ { name:   "Mana Burst A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 50.0 ]
                }
              , { name:   "Intuition B"
                , icon:   IconStar
                , cd:     5
                , effect: [ To Party GainStars 14.0 ]
                }
              , { name:   "Secret of Pedigree EX"
                , icon:   IconShieldUp
                , cd:     6
                , effect: [ Grant Self 1 DefenseUp 50.0 
                          , To Self RemoveDebuffs 0.0
                          , To Self GaugeUp 30.0
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
              , effect: [ To Enemies Damage 600.0 ]
              , over:   [ To (EnemiesType Arthur) Damage 180.0 
                        , To Self GaugeUp 20.0
                        ]
              }
  , gen:      { starWeight: 98, starRate: 10.0, npPerHit: 0.56, npPerDefend: 3 }
  , hits:     { a: 3, b: 1, q: 2, ex: 4 }
  , traits:   [Female, Riding, Dragon, Saberface, EnumaElish]
  , death:    24.5
  , align:    Chaotic:Balanced
  , limited:  false
  }
, { name:     "Ryougi Shiki (Saber)"
  , rarity:   5
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1656,  hp: 2266 }
              , max:   { atk: 10721, hp: 15453 }
              , grail: { atk: 11736, hp: 16929 }
              }
  , ratings:  { damage:3, np:5, critical:2, utility:2, support:1, durability:4 }
  , actives:  [ { name:   "Mystic Eyes of Death Perception C"
                , icon:   IconMystic
                , cd:     5
                , effect: [ Grant Self 1 IgnoreInvinc 0.0 
                          , Grant Self 1 ArtsUp 40.0
                          , Debuff Enemies 1 DeathDown 80.0
                          ]
                }
              , { name:   "Unyou B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 25.0 
                          , Grant Self 3 DebuffResist 36.0
                          ]
                }
              , { name:   "Yin-Yang A"
                , icon:   IconYinYang
                , cd:     4
                , effect: [ To Self Heal 5000.0 
                          , To Self DemeritGauge 10.0
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
              , effect: [ To Enemies DamageThruDef 750.0 
                        , To Party RemoveDebuffs 0.0
                        ]
              , over:   [ To Enemies Kill 60.0 ]
              }
  , gen:      { starWeight: 102, starRate: 9.9, npPerHit: 0.84, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 4, ex: 4 }
  , traits:   [Female, EnumaElish, PseudoServant]
  , death:    24.5
  , align:    Neutral:Balanced
  , limited:  true
  }
, { name:     "Altria Pendragon"
  , rarity:   5
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1734,  hp: 2222 }
              , max:   { atk: 11221, hp: 15150 }
              , grail: { atk: 12283, hp: 16597 }
              }
  , ratings:  { damage:4, np:3, critical:2, utility:1, support:2, durability:3 }
  , actives:  [ { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 18.0 ]
                }
              , { name:   "Mana Burst A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 50.0 ]
                }
              , { name:   "Intuition A"
                , icon:   IconStar
                , cd:     5
                , effect: [ To Party GainStars 15.0 ]
                }
              ]
  , passives: [magicResistance A, riding B]
  , phantasm: { name:   "Excalibur"
              , desc:   "Sword of Promised Victory"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   1
              , effect: [ To Enemies Damage 600.0 ]
              , over:   [ To Self GaugeUp 20.0 ]
              }
  , gen:      { starWeight: 102, starRate: 10.0, npPerHit: 0.86, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, Dragon, Saberface, Arthur, EnumaElish, King]
  , death:    21.0
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "Lancelot (Saber)"
  , rarity:   4
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1658,  hp: 1854 }
              , max:   { atk: 9949, hp: 11589 }
              , grail: { atk: 12046, hp: 14051 }
              }
  , ratings:  { damage:5, np:4, critical:5, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Knight of the Lake A"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 30.0 
                          , To Party GainStars 6.0
                          ]
                }
              , { name:   "Eternal Arms Mastery A+"
                , icon:   IconStarUp
                , cd:     5
                , effect: [ Grant Self 3 StarAbsorb 6000.0 ]
                }
              , { name:   "Knight of Owner A++"
                , icon:   IconStarTurn
                , cd:     6
                , effect: [ Grant Self 3 StarsPerTurn 15.0 
                          , Grant Self 3 CritUp 50.0
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
              , effect: [ Grant Self 1 ArtsUp 30.0 
                        , To Enemy Damage 1500.0
                        ]
              , over:   [ Debuff Enemy 5 DamageVuln 1000.0 ]
              }
  , gen:      { starWeight: 100, starRate: 10.0, npPerHit: 0.83, npPerDefend: 3 }
  , hits:     { a: 2, b: 4, q: 3, ex: 5 }
  , traits:   [Male, Brynhildr, Riding, EnumaElish]
  , death:    28.0
  , align:    Lawful:Balanced
  , limited:  false
  }
, { name:     "Altria Pendragon (Alter)"
  , rarity:   4
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1708,  hp: 1854 }
              , max:   { atk: 10248, hp: 11589 }
              , grail: { atk: 12408, hp: 14051 }
              }
  , ratings:  { damage:5, np:3, critical:2, utility:1, support:2, durability:2 }
  , actives:  [ { name:   "Mana Burst A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 50.0 ]
                }
              , { name:   "Intuition B"
                , icon:   IconStar
                , cd:     5
                , effect: [ To Party GainStars 14.0 ]
                }
              , { name:   "Charisma E"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 12.0 ]
                }
              ]
  , passives: [magicResistance B]
  , phantasm: { name:   "Excalibur Morgan"
              , desc:   "Sword of Promised Victory"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   3
              , effect: [ To Enemies Damage 650.0 ]
              , over:   [ To Self GaugeUp 10.0 ]
              }
  , gen:      { starWeight: 99, starRate: 9.9, npPerHit: 0.86, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, King, Dragon, Saberface, Arthur, EnumaElish]
  , death:    19.2
  , align:    Lawful:Evil
  , limited:  false
  }
, { name:     "Rama"
  , rarity:   4
  , class:    Saber
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1642,  hp: 1919 }
              , max:   { atk: 9854, hp: 11993 }
              , grail: { atk: 11931, hp: 14541 }
              }
  , ratings:  { damage:5, np:2, critical:5, utility:2, support:2, durability:3 }
  , actives:  [ { name:   "Blessing of Martial Arts A"
                , icon:   IconStarUp
                , cd:     4
                , effect: [ Grant Self 1 StarAbsorb 500.0 
                          , Grant Self 1 CritUp 100.0
                          ]
                }
              , { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 18.0 ]
                }
              , { name:   "Curse of Separation A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 3 Guts 1.0 
                          , To Self Heal 3000.0
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
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ To (EnemyType Demonic) Damage 150.0]
              }
  , gen:      { starWeight: 100, starRate: 10.2, npPerHit: 0.84, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 4 }
  , traits:   [Male, Riding, Brynhildr, Divine, EnumaElish, King]
  , death:    24.5
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "Nero Claudius"
  , rarity:   4
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1574,  hp: 1880 }
              , max:   { atk: 9449, hp: 11753 }
              , grail: { atk: 11441, hp: 14250 }
              }
  , ratings:  { damage:4, np:3, critical:1, utility:1, support:1, durability:5 }
  , actives:  [ { name:   "Migraine B"
                , icon:   IconHoodUp
                , cd:     5
                , effect: [ Grant Self 3 DebuffResist 100.0 
                          , To Self Heal 2000.0
                          ]
                }
              , { name:   "Imperial Privilege EX"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Self Heal 3400.0 
                          , Chance 60 $ Grant Self 3 AttackUp 44.0
                          , Chance 60 $ Grant Self 3 DefenseUp 44.0
                          ]
                }
              , { name:   "Invictus Spiritus A"
                , icon:   IconKneel
                , cd:     10
                , effect: [ Grant Self 5 Guts 3.0 ]
                }
              ]
  , passives: [magicResistance C, riding B]
  , phantasm: { name:   "Laus Saint Claudius"
              , desc:   "Imperium of the Maiden's Flowery Words"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Encampment"
              , hits:   1
              , effect: [ To Enemies DamageThruDef 900.0 ]
              , over:   [ Debuff Enemies 1 DefenseDown 20.0 ]
              }
  , gen:      { starWeight: 102, starRate: 10.1, npPerHit: 0.84, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 5 }
  , traits:   [Female, Riding, Roman, Saberface, EnumaElish, King]
  , death:    24.5
  , align:    Chaotic:Good
  , limited:  false
  }
, { name:     "Chevalier d'Eon"
  , rarity:   4
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1460,  hp: 2121 }
              , max:   { atk: 8765, hp: 13256 }
              , grail: { atk: 10613, hp: 16073 }
              }
  , ratings:  { damage:2, np:3, critical:1, utility:3, support:4, durability:5 }
  , actives:  [ { name:   "Mind's Eye (True) C"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 DefenseUp 16.0
                          ]
                }
              , { name:   "Self-Suggestion A"
                , icon:   IconBubbles
                , cd:     5
                , effect: [ To Self RemoveDebuffs 0.0 
                          , Grant Self 3 DebuffResist 100.0
                          ]
                }
              , { name:   "Beautiful Appearance C"
                , icon:   IconFace
                , cd:     5
                , effect: [ Grant Self 3 Taunt 0.0 
                          , To Self Heal 2500.0
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
              , effect: [ Debuff Enemies 2 AttackDown 30.0 
                        , Debuff Enemies 2 DefenseDown 30.0
                        ]
              , over:   [ Chance 10 $ Debuff Enemies 1 Charm 0.0 ]
              }
  , gen:      { starWeight: 102, starRate: 10.0, npPerHit: 0.83, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Riding, EnumaElish]
  , death:    28.0
  , align:    Neutral:Balanced
  , limited:  false
  }
  , { name:   "Siegfried"
  , rarity:   4
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1363,  hp: 2266 }
              , max:   { atk: 8181, hp: 14165 }
              , grail: { atk: 9905, hp: 17175 }
              }
  , ratings:  { damage:3, np:4, critical:1, utility:3, support:1, durability:3 }
  , actives:  [ { name:   "Golden Rule C-"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 37.5 ]
                }
              , { name:   "Disengage A"
                , icon:   IconBubbles
                , cd:     5
                , effect: [ To Self RemoveDebuffs 0.0 
                          , To Self Heal 2500.0
                          ]
                }
              , { name:   "Dragon-Slayer A"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 3 (AttackUpVs Dragon) 80.0 
                          , Grant Self 3 (DefenseUpVs Dragon) 30.0
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
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ To (EnemiesType Dragon) Damage 150.0 ]
              }
  , gen:      { starWeight: 97, starRate: 10.0, npPerHit: 0.83, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Riding, Brynhildr, Dragon, EnumaElish, King]
  , death:    28.0
  , align:    Chaotic:Good
  , limited:  false
  }
, { name:     "Gawain"
  , rarity:   4
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1695,  hp: 1827 }
              , max:   { atk: 10173, hp: 11419 }
              , grail: { atk: 12317, hp: 13845 }
              }
  , ratings:  { damage:4, np:4, critical:2, utility:3, support:2, durability:3 }
  , actives:  [ { name:   "Numeral of the Saint EX"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 20.0 
                          , When "on Open Field with Sunshine" 
                            $ Grant Self 3 BusterUp 30.0
                          ]
                }
              , { name:   "Charisma E"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 12.0 ]
                }
              , { name:   "Belt of Bertilak EX"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 20.0 
                          , To Party GainStars 10.0
                          , Grant Self 1 Guts 1.0
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
              , effect: [ To Enemies Damage 500.0 
                        , Debuff Enemies 1 SealSkills 0.0
                        ]
              , over:   [ Debuff Enemies 5 Burn 1000.0 ]
              }
  , gen:      { starWeight: 102, starRate: 10.0, npPerHit: 1.14, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 5 }
  , traits:   [Male, Brynhildr, Riding, EnumaElish]
  , death:    21.0
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "Altria Pendragon (Lily)"
  , rarity:   4
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1287,  hp: 1699 }
              , max:   { atk: 7726, hp: 10623 }
              , grail: { atk: 9355, hp: 12880 }
              }
  , ratings:  { damage:3, np:4, critical:2, utility:2, support:2, durability:3 }
  , actives:  [ { name:   "Intuition B"
                , icon:   IconStar
                , cd:     5
                , effect: [ To Party GainStars 14.0 ]
                }
              , { name:   "Mana Burst A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 50.0 ]
                }
              , { name:   "Journey of the Flowers EX"
                , icon:   IconNobleUp
                , cd:     5
                , effect: [ Grant Party 3 NPGen 20.0 ]
                }
              ]
  , passives: [magicResistance B, riding C]
  , phantasm: { name:   "Caliburn"
              , desc:   "Golden Sword of the Victorious"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   8
              , effect: [ To Enemies Damage 600.0 ]
              , over:   [ To Self Heal 1000.0 ]
              }
  , gen:      { starWeight: 102, starRate: 10.0, npPerHit: 0.86, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, Dragon, Saberface, Arthur, EnumaElish, King]
  , death:    21.0
  , align:    Lawful:Good
  , limited:  true
  }
, { name:     "Gaius Julius Caesar"
  , rarity:   3
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1392,  hp: 1727 }
              , max:   { atk: 7497, hp: 9595 }
              , grail: { atk: 10146, hp: 13009 }
              }
  , ratings:  { damage:4, np:4, critical:4, utility:2, support:3, durability:2 }
  , actives:  [ { name:   "Tactics B"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Party 1 NPUp 18.0 ]
                }
              , { name:   "Charisma C"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 16.0 ]
                }
              , { name:   "Incite EX"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Ally 3 CritUp 40.0
                          , Debuff Ally 3 DefenseDown 20.0
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
              , effect: [ To Enemy Damage 2000.0 ]
              , over:   [ To Party GainStars 5.0 ]
              }
  , gen:      { starWeight: 99, starRate: 10.0, npPerHit: 1.1, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, King, Riding, Brynhildr, Roman, Divine, EnumaElish]
  , death:    31.5
  , align:    Neutral:Balanced
  , limited:  false
  }
, { name:     "Fergus mac Roich"
  , rarity:   3
  , class:    Saber
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1385,  hp: 1761 }
              , max:   { atk: 7460, hp: 9786 }
              , grail: { atk: 10096, hp: 13268 }
              }
  , ratings:  { damage:4, np:3, critical:1, utility:2, support:2, durability:4 }
  , actives:  [ { name:   "Valor A"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 30.0  
                          , Grant Self 3 DebuffResist 40.0
                          ]
                }
              , { name:   "Defiant B"
                , icon:   IconShieldUp
                , cd:     4
                , effect: [ Grant Self 3 DefenseUp 25.0 
                          , Debuff Self 3 BuffFail 50.0
                          ]
                }
              , { name:   "Mind's Eye (True) A"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 DefenseUp 20.0
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
              , effect: [ To Enemies Damage 600.0 
                        , Debuff Enemies 3 DefenseDown 20.0
                        ]
              , over:   [ Debuff Enemies 3 DebuffVuln 20.0 ]
              }
  , gen:      { starWeight: 99, starRate: 10.0, npPerHit: 1.09, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Riding, Brynhildr, EnumaElish, King]
  , death:    35.0
  , align:    Lawful:Balanced
  , limited:  false
  }
, { name:     "Bedivere"
  , rarity:   3
  , class:    Saber
  , attr:     Star
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1416,  hp: 1727 }
              , max:   { atk: 7627, hp: 9595 }
              , grail: { atk: 10322, hp: 13009 }
              }
  , ratings:  { damage:4, np:4, critical:1, utility:1, support:3, durability:3 }
  , actives:  [ { name:   "Tactics C"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Party 1 NPUp 16.0 ]
                }
              , { name:   "Calm and Collected B"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 30.0 
                          , Grant Self 3 DebuffResist 50.0
                          ]
                }
              , { name:   "Oath of Protection B"
                , icon:   IconShieldUp
                , cd:     6
                , effect: [ Grant Party 1 DefenseUp 30.0 
                          , Grant Self 1 DebuffResist 50.0
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
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ Grant Self 1 BusterUp 30.0 ]
              }
  , gen:      { starWeight: 100, starRate: 10.2, npPerHit: 1.11, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 5 }
  , traits:   [Male, Brynhildr, Riding, EnumaElish]
  , death:    28.0
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "Gilles de Rais"
  , rarity:   3
  , class:    Saber
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1228,  hp: 1889 }
              , max:   { atk: 6615, hp: 10498 }
              , grail: { atk: 8952, hp: 14234 }
              }
  , ratings:  { damage:4, np:3, critical:2, utility:1, support:2, durability:2 }
  , actives:  [ { name:   "Tactics C"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Party 1 NPUp 16.0 ]
                }
              , { name:   "Golden Rule B"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 45.0 ]
                }
              , { name:   "Prelati's Encouragement B"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 5 BusterUp 40.0 ]
                }
              ]
  , passives: [magicResistance B, riding B, madness EX]
  , phantasm: { name:   "Saint War Order"
              , desc:   "Rally Thy War Cries Under the Holy Flag"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   0
              , effect: [ Grant Self 2 AttackUp 100.0 
                        , Debuff Self 3 DefenseDown 50.0
                        ]
              , over:   [ To Party GainStars 5.0 ]
              }
  , gen:      { starWeight: 98, starRate: 9.9, npPerHit: 0.82, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Riding, Brynhildr, EnumaElish]
  , death:    31.5
  , align:    Lawful:Good
  , limited:  false
  }

]
