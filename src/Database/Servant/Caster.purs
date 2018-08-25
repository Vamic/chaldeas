module Database.Servant.Caster where

import Prelude
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
  , passives: [itemConstruction B, territoryCreation A]
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
              , over:   [ Chance 50 $ Debuff Enemies 1 Stun 0.0 ]
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
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1629,  hp: 2091 }
              , max:   { atk: 10546, hp: 14259 }
              , grail: { atk: 11544, hp: 15621 }
              }
  , ratings:  { damage:1, np:3, critical:1, utility:5, support:5, durability:4 }
  , actives:  [ { name:   "Curse EX"
                , icon:   IconDarkMagic
                , cd:     5
                , effect: [ Chance 100 $ To Enemy GaugeDown 1.0 ]
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
  , passives: [territoryCreation C, divinity A]
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
  , traits:   [Female, Divine, EnumaElish]
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
                , icon:   IconCrosshairUp
                , cd:     6
                , effect: [ Grant Self 1 Taunt 0.0 
                          , Grant Self 1 ReduceDamage 1500.0
                          ]
                }
              , { name:   "Sanzang's Teaching A"
                , icon:   IconNobleUp
                , cd:     5
                , effect: [ Grant Party 3 NPGen 30.0 
                          , Grant Party 3 StarUp 30.0
                          , Grant Party 1 DebuffImmunity 0.0
                          ]
                }
              ]
  , passives: [territoryCreation APlus, divinity D]
  , phantasm: { name:   "Five Elements Mountain Buddha Palm"
              , desc:   "Wu Xing Shan: Shijia Rulai Zhang"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   12
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ Debuff Enemy 1 CritChance 80.0 ]
              }
  , gen:      { starAbsorb: 52, starGen: 11.0, npPerHit: 0.82, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 6 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    34.5
  , align:    Lawful:Good
  }
, { name:     "Leonardo da Vinci"
  , rarity:   5
  , class:    Caster
  , attr:     Star
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1637,  hp: 2091 }
              , max:   { atk: 10598, hp: 14259 }
              , grail: { atk: 11601, hp: 15621 }
              }
  , ratings:  { damage:4, np:5, critical:2, utility:4, support:2, durability:4 }
  , actives:  [ { name:   "Inherent Wisdom EX"
                , icon:   IconKneel
                , cd:     5
                , effect: [ Grant Self 3 Guts 1.0 
                          , Chance 85 $ Grant Self 3 DefenseUp 30.0
                          , Chance 85 $ Grant Self 3 NPUp 30.0
                          ]
                }
              , { name:   "Golden Rule (Body) B"
                , icon:   IconHealTurn
                , cd:     6
                , effect: [ Grant Self 2 DebuffImmunity 0.0 
                          , Grant Self 3 HealPerTurn 1000.0
                          , Grant Self 3 GaugePerTurn  10.0
                          ]
                }
              , { name:   "Pioneer of the Stars EX"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0 
                          , Grant Self 3 Invincibility 0.0
                          , To Party GainStars 10.0
                          ]
                }
              ]
  , passives: [territoryCreation A, itemConstruction A]
  , phantasm: { name:   "Uomo Universale"
              , desc:   "The Universal Man"
              , rank:   EX
              , card:   Arts
              , kind:   "Anti-Unit/Anti-Army"
              , hits:   1
              , effect: [ To Enemies DamageThruDef 750.0 
                        , Debuff Enemies 3 CritChance 10.0
                        ]
              , over:   [ Grant Self 1 NPUp 30.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 10.8, npPerHit: 0.54, npAttacked: 3 }
  , hits:     { a: 3, b: 1, q: 4, ex: 4 }
  , traits:   [Female, EnumaElish]
  , death:    34.5
  , align:    Chaotic:Good
  }
, { name:     "Illyasviel von Einzbern"
  , rarity:   5
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1677,  hp: 2027 }
              , max:   { atk: 10857, hp: 13825 }
              , grail: { atk: 11885, hp: 15146 }
              }
  , ratings:  { damage:4, np:3, critical:1, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Happiness Mystic Code A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 50.0 ]
                }
              , { name:   "Child of Nature B"
                , icon:   IconShield
                , cd:     6
                , effect: [ Grant Self 1 Invincibility 0.0 
                          , Grant Self 3 NPGen 28.0
                          ]
                }
              , { name:   "Suspicious Medicine A"
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Ally Heal 3000.0 
                          , Chance 70 $ Grant Self 1 DebuffImmunity 1.0
                          , Chance 70 $ Grant Self 3 Guts 1.0
                          ]
                }
              ]
  , passives: [magicResistance B, unlimitedManaSupply C]
  , phantasm: { name:   "Quintett Feuer"
              , desc:   "Multi-instrumental Saturation Bombardment"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Unit"
              , hits:   4
              , effect: [ To Enemy Damage 1000.0
                        , Debuff Self 3 AttackDown 10.0
                        , Debuff Self 3 DefenseDown 10.0
                        ]
              , over:   [ Grant Self 1 BusterUp 20.0 ]
              }
  , gen:      { starAbsorb: 51, starGen: 10.7, npPerHit: 0.32, npAttacked: 3 }
  , hits:     { a: 5, b: 1, q: 3, ex: 5 }
  , traits:   [Female, PseudoServant, EnumaElish]
  , death:    42.0
  , align:    Neutral:Good
  }
, { name:     "Medea (Lily)"
  , rarity:   4
  , class:    Caster
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1294,  hp: 2091 }
              , max:   { atk: 7766, hp: 13070 }
              , grail: { atk: 9403, hp: 15847 }
              }
  , ratings:  { damage:1, np:5, critical:3, utility:5, support:5, durability:3 }
  , actives:  [ { name:   "Rapid Words of Divine A"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 150.0 ]
                }
              , { name:   "Poison Resistance A++"
                , icon:   IconBubbles
                , cd:     5
                , effect: [ To Party Cure 0.0 
                          , To Party Heal 3000.0
                          ]
                }
              , { name:   "Ephemeral Love B"
                , icon:   IconHeal
                , cd:     8
                , effect: [ Grant Ally 1 HealingReceived 100.0 ]
                }
              ]
  , passives: [territoryCreation B, itemConstruction B]
  , phantasm: { name:   "Pain Breaker"
              , desc:   "All Flaws Must Be Repaired"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Magic"
              , hits:   0
              , effect: [ To Allies RemoveDebuffs 0.0
                        , To Party Heal 6000.0
                        ]
              , over:   [ Grant Party 3 DebuffResist 40.0 ]
              }
  , gen:      { starAbsorb: 51, starGen: 10.7, npPerHit: 0.4, npAttacked: 3 }
  , hits:     { a: 4, b: 3, q: 4, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    36.0
  , align:    Lawful:Good
  }
, { name:     "Elisabeth Bathory (Halloween)"
  , rarity:   4
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1436,  hp: 1824 }
              , max:   { atk: 8616, hp: 11404 }
              , grail: { atk: 10432, hp: 13827 }
              }
  , ratings:  { damage:4, np:3, critical:2, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Innocent Monster EX"
                , icon:   IconStarTurn
                , cd:     5
                , effect: [ Grant Self 3 StarsPerTurn 12.0 
                          , To Self Heal 2000.0
                          ]
                }
              , { name:   "Mana Burst (Pumpkin) A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 45.0 
                          , Debuff Enemies 10 Burn 300.0
                          ]
                }
              , { name:   "Performance Continuation A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 
                          , To Party GainStars 8.0
                          ]
                }
              ]
  , passives: [territoryCreation B, itemConstruction A]
  , phantasm: { name:   "Bathory Halloween Erzsebet"
              , desc:   "First Class Demon Daughter of Fresh Blood"
              , rank:   EMinus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies DamageThruDef 500.0 ]
              , over:   [ Debuff Enemies 3 Curse 500.0 ]
              }
  , gen:      { starAbsorb: 49, starGen: 10.8, npPerHit: 1.6, npAttacked: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 4 }
  , traits:   [Female, Dragon, EnumaElish]
  , death:    42.0
  , align:    Chaotic:Evil
  }
, { name:     "Nursery Rhyme"
  , rarity:   4
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1438,  hp: 1901 }
              , max:   { atk: 8629, hp: 11882 }
              , grail: { atk: 10448, hp: 14407 }
              }
  , ratings:  { damage:3, np:4, critical:3, utility:2, support:1, durability:4 }
  , actives:  [ { name:   "Self-Modification A"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 3 CritUp 50.0 
                          , Grant Self 3 StarAbsorb 600.0
                          ]
                }
              , { name:   "Morph A+"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 30.0 
                          , Grant Self 1 DefenseUp 30.0
                          , Grant Self 3 DebuffResist 40.0
                          ]
                }
              , { name:   "Meanwhile A"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 40.0 
                          , To Self Heal 2000.0
                          , To Self RemoveDebuffs 0.0
                          ]
                }
              ]
  , passives: [territoryCreation A]
  , phantasm: { name:   "Nursery Rhyme"
              , desc:   "A Tale for Somebody's Sake"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   3
              , effect: [ To Enemies Damage 900.0 
                        , Debuff Enemies 3 DefenseDown 20.0
                        ]
              , over:   [ Chance 60 $ To Enemies GaugeDown 1.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 10.8, npPerHit: 0.54, npAttacked: 3 }
  , hits:     { a: 3, b: 1, q: 3, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    36.0
  , align:    Neutral:Neutral
  }
, { name:     "Helena Blavatsky"
  , rarity:   4
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1438,  hp: 1901 }
              , max:   { atk: 8629, hp: 11882 }
              , grail: { atk: 10448, hp: 14407 }
              }
  , ratings:  { damage:4, np:3, critical:3, utility:4, support:4, durability:2 }
  , actives:  [ { name:   "Mana Tuning C"
                , icon:   IconNoble
                , cd:     7
                , effect: [ To Party GaugeUp 20.0 ]
                }
              , { name:   "Mahatma A"
                , icon:   IconStarTurn
                , cd:     8
                , effect: [ Grant Self 5 StarsPerTurn 5.0 
                          , Chance 80 $ Grant Self 1 NPUp 50.0
                          ]
                }
              , { name:   "Search for the Unknown B"
                , icon:   IconAllUp
                , cd:     7
                , effect: [ Grant Party 3 QuickUp 20.0 
                          , Grant Party 3 ArtsUp 20.0
                          , Grant Party 3 BusterUp 20.0
                          ]
                }
              ]
  , passives: [territoryCreation A, itemConstruction B]
  , phantasm: { name:   "Sanat Kumara"
              , desc:   "Venusian God, Heavenly Lord of the Flame"
              , rank:   A
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage 750.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown 10.0 
                        , Debuff Enemies 3 CritChance 10.0
                        , Debuff Enemies 3 DebuffVuln 10.0  
                        ]
              }
  , gen:      { starAbsorb: 51, starGen: 10.7, npPerHit: 0.45, npAttacked: 3 }
  , hits:     { a: 3, b: 1, q: 6, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    36.0
  , align:    Chaotic:Good
  }
, { name:     "Nitocris"
  , rarity:   4
  , class:    Caster
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1510,  hp: 1806 }
              , max:   { atk: 9060, hp: 11288 }
              , grail: { atk: 10970, hp: 13686 }
              }
  , ratings:  { damage:3, np:5, critical:3, utility:3, support:2, durability:4 }
  , actives:  [ { name:   "Egyptian Magecraft A"
                , icon:   IconReaperUp
                , cd:     5
                , effect: [ Grant Self 3 KillUp 50.0 
                          , To Self Heal 3000.0
                          ]
                }
              , { name:   "Rapid Words of Divine B"
                , icon:   IconNoble
                , cd:     7
                , effect: [ To Self GaugeUp 120.0 ]
                }
              , { name:   "Affection of the Sky God B"
                , icon:   IconKneel
                , cd:     5
                , effect: [ Grant Self 3 Guts 1.0 
                          , To Self Heal 2000.0
                          ]
                }
              ]
  , passives: [territoryCreation A, itemConstruction BPlus, divinity B]
  , phantasm: { name:   "Anpu Neb Ta Djeser"
              , desc:   "Nether Mirror Thesaurus"
              , rank:   BPlus
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage 750.0 ]
              , over:   [ To Enemies Kill 50.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 10.8, npPerHit: 0.54, npAttacked: 4 }
  , hits:     { a: 3, b: 3, q: 4, ex: 5 }
  , traits:   [Female, Divine, King, EnumaElish]
  , death:    36.0
  , align:    Lawful:Good
  }
, { name:     "Irisviel (Dress of Heaven)"
  , rarity:   4
  , class:    Caster
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1372,  hp: 1996 }
              , max:   { atk: 8237, hp: 12476 }
              , grail: { atk: 9973, hp: 15127 }
              }
  , ratings:  { damage:1, np:3, critical:1, utility:2, support:5, durability:4 }
  , actives:  [ { name:   "Sacrificial Resolve A"
                , icon:   IconHealUp
                , cd:     7
                , effect: [ Grant Self 1 HealUp 50.0 ]
                }
              , { name:   "Child of Nature A"
                , icon:   IconShield
                , cd:     6
                , effect: [ Grant Self 1 Invincibility 0.0 
                          , Grant Self 3 NPGen 30.0
                          ]
                }
              , { name:   "Magical Healing A"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Ally Heal 3000.0 ]
                }
              ]
  , passives: [territoryCreation B, coreOfGoddess C]
  , phantasm: { name:   "Song of Grail"
              , desc:   "Sing Out, O' White Grail"
              , rank:   B
              , card:   Arts
              , kind:   "Magecraft"
              , hits:   0
              , effect: [ To Party Heal 3000.0 ]
              , over:   [ Grant Party 3 Guts 0.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 10.8, npPerHit: 0.42, npAttacked: 3 }
  , hits:     { a: 3, b: 3, q: 3, ex: 4 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    34.5
  , align:    Lawful:Good
  }
, { name:     "Marie Antoinette (Caster)"
  , rarity:   4
  , class:    Caster
  , attr:     Mankind 
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1510,  hp: 1824 }
              , max:   { atk: 9060, hp: 11404 }
              , grail: { atk: 10970, hp: 13827 }
              }
  , ratings:  { damage:2, np:3, critical:3, utility:2, support:4, durability:5 }
  , actives:  [ { name:   "Beach Flower A+"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 19.5 
                          , Grant (AlliesType Male) 3 StarUp 41.0
                          ]
                }
              , { name:   "Sparkling Sunflower A"
                , icon:   IconStarTurn
                , cd:     7
                , effect: [ Grant Self 3 StarsPerTurn 10.0 
                          , Grant Self 3 HealPerTurn 500.0
                          ]
                }
              , { name:   "Beautiful Princess (Sea) A"
                , icon:   IconShield
                , cd:     6
                , effect: [ Grant Self 0 Invincibility 3.0 ]
                }
              ]
  , passives: [territoryCreation A, itemConstruction D]
  , phantasm: { name:   "Crystal Dress"
              , desc:   "Precious Brilliance Everlasting"
              , rank:   A
              , card:   Arts
              , kind:   "Anti-Army/Anti-Populace"
              , hits:   3
              , effect: [ To Enemies Damage 750.0 
                        , Debuff Enemies 3 CritChance 20.0
                        ]
              , over:   [ Grant Party 3 CritUp 20.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 10.9, npPerHit: 0.32, npAttacked: 3 }
  , hits:     { a: 5, b: 1, q: 3, ex: 4 }
  , traits:   [Female, EnumaElish]
  , death:    36.0
  , align:    Lawful:Good
  }
, { name:     "Thomas Edison"
  , rarity:   4
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1325,  hp: 1901 }
              , max:   { atk: 7952, hp: 11882 }
              , grail: { atk: 9628, hp: 14407 }
              }
  , ratings:  { damage:2, np:3, critical:4, utility:4, support:3, durability:3 }
  , actives:  [ { name:   "Morph C"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Self 3 DefenseUp 24.0 ]
                }
              , { name:   "Mass Production A"
                , icon:   IconStarTurn
                , cd:     10
                , effect: [ Grant Self 5 StarsPerTurn 10.0 
                          , Grant Self 5 GaugePerTurn 10.0
                          ]
                }
              , { name:   "Concept Improvement A+"
                , icon:   IconSunUp
                , cd:     6
                , effect: [ Grant Ally 1 Overcharge 2.0 
                          , Grant Ally 1 StarUp 30.0
                          ]
                }
              ]
  , passives: [territoryCreation D, itemConstruction D]
  , phantasm: { name:   "W • F • D"
              , desc:   "World Faith Domination"
              , rank:   EX
              , card:   Arts
              , kind:   "Anti-Populace"
              , hits:   1
              , effect: [ To Enemies Damage 750.0 
                        , Debuff Enemies 1 SealSkills 0.0
                        , Debuff Enemies 1 SealNP 0.0
                        ]
              , over:   [ Debuff Enemies 3 CritChance 10.0 ]
              }
  , gen:      { starAbsorb: 51, starGen: 10.6, npPerHit: 0.51, npAttacked: 3 }
  , hits:     { a: 3, b: 3, q: 3, ex: 5 }
  , traits:   [Male, EnumaElish]
  , death:    60.0
  , align:    Lawful:Neutral
  }
, { name:     "Hans Christian Andersen"
  , rarity:   2
  , class:    Caster
  , attr:     Mankind 
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1021,  hp: 1597 }
              , max:   { atk: 5758, hp: 8484 }
              , grail: { atk: 8344, hp: 12244 }
              }
  , ratings:  { damage:1, np:4, critical:4, utility:3, support:5, durability:3 }
  , actives:  [ { name:   "Human Observation A"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Party 3 CritUp 40.0 ]
                }
              , { name:   "Rapid Casting E"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 75.0 ]
                }
              , { name:   "Innocent Monster D"
                , icon:   IconStarTurn
                , cd:     5
                , effect: [ Grant Self 3 StarsPerTurn 9.0 
                          , Debuff Self 3 DefenseDown 14.0
                          ]
                }
              ]
  , passives: [territoryCreation D, itemConstruction C]
  , phantasm: { name:   "Marchen Meines Lebens"
              , desc:   "A Story Just For You"
              , rank:   CPlus
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   0
              , effect: []
              , over:   [ Chance 80 $ Grant Party 3 AttackUp 20.0 
                        , Chance 80 $ Grant Party 3 DefenseUp 20.0
                        , Chance 80 $ Grant Party 3 StarUp 20.0
                        , To Party Heal 1000.0
                        ]
              }
  , gen:      { starAbsorb: 48, starGen: 10.8, npPerHit: 1.66, npAttacked: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    30.0
  , align:    Lawful:Neutral
  }
, { name:     "Medea"
  , rarity:   3
  , class:    Caster
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1377,  hp: 1555 }
              , max:   { atk: 7418, hp: 8643 }
              , grail: { atk: 10039, hp: 11719 }
              }
  , ratings:  { damage:3, np:5, critical:1, utility:4, support:2, durability:3 }
  , actives:  [ { name:   "Rapid Words of Divine A"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 150.0 ]
                }
              , { name:   "Argon Coin"
                , icon:   IconHeal
                , cd:     4
                , effect: [ To Self Heal 2500.0 ]
                }
              , { name:   "Circe's Teaching A"
                , icon:   IconBubbles
                , cd:     6
                , effect: [ To Ally RemoveDebuffs 0.0 
                          , Grant Ally 1 NPGen 50.0
                          ]
                }
              ]
  , passives: [territoryCreation A, itemConstruction A]
  , phantasm: { name:   "Rule Breaker"
              , desc:   "All Spells Must Be Broken"
              , rank:   CPlus
              , card:   Arts
              , kind:   "Anti-Thaumaturgy"
              , hits:   1
              , effect: [ To Enemy Damage 900.0 
                        , To Enemy RemoveBuffs 0.0
                        ]
              , over:   [ To Self GaugeUp 20.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 10.9, npPerHit: 1.64, npAttacked: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    34.5
  , align:    Neutral:Evil
  }
, { name:     "William Shakespeare"
  , rarity:   2
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1028,  hp: 1520 }
              , max:   { atk: 5798, hp: 8080 }
              , grail: { atk: 8402, hp: 11661 }
              }
  , ratings:  { damage:3, np:4, critical:1, utility:3, support:3, durability:4 }
  , actives:  [ { name:   "Enchant A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Party 1 BusterUp 40.0 ]
                }
              , { name:   "Self-Preservation B"
                , icon:   IconShield
                , cd:     6
                , effect: [ Grant Self 1 Invincibility 0.0 
                          , To Self Heal 1500.0
                          ]
                }
              , { name:   "King's Men C"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Ally GaugeUp 20.0 
                          , Grant Ally 1 StarUp 100.0
                          ]
                }
              ]
  , passives: [territoryCreation C]
  , phantasm: { name:   "First Folio"
              , desc:   "When the Curtain Rises, There Will be Thunderous Applause"
              , rank: B  
              , card:  Buster 
              , kind:   "Anti-Personnel"
              , hits:   4
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Debuff Enemies 1 Stun 30.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 10.8, npPerHit: 1.59, npAttacked: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    45.0
  , align:    Neutral:Neutral
  }
, { name:     "Wolfgang Amadeus Mozart"
  , rarity:   1
  , class:    Caster
  , attr:     Star
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 944,  hp: 1425 }
              , max:   { atk: 5195, hp: 7129 }
              , grail: { atk: 8072, hp: 10990 }
              }
  , ratings:  { damage:1, np:3, critical:2, utility:3, support:5, durability:1 }
  , actives:  [ { name:   "Protection of Muse (Fake) EX"
                , icon:   IconArtsUp
                , cd:     5
                , effect: [ Grant Party 1 ArtsUp 44.0 ]
                }
              , { name:   "Aesthetic Appreciation B"
                , icon:   IconBeamDown
                , cd:     5
                , effect: [ Debuff Enemy 1 NPDown 18.0 ]
                }
              , { name:   "Eine kleine Nachtmusik EX"
                , icon:   IconStar
                , cd:     6
                , effect: [ To Party GainStars 50.0 ]
                }
              ]
  , passives: [territoryCreation B]
  , phantasm: { name:   "Requiem for Death"
              , desc:   "Funeral Music for the Death God"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Chance 80 $ Debuff Enemies 3 AttackDown 20.0 
                        , Chance 80 $ Debuff Enemies 3 DefenseDown 20.0
                        ]
              , over:   [ Debuff Enemies 3 Curse 500.0 ]
              }
  , gen:      { starAbsorb: 49, starGen: 11.0, npPerHit: 1.6, npAttacked: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Brynhildr]
  , death:    40.5
  , align:    Neutral:Good
  }
, { name:     "Charles Babbage"
  , rarity:   3
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1113,  hp: 1959 }
              , max:   { atk: 5996, hp: 10887 }
              , grail: { atk: 8115, hp: 14761 }
              }
  , ratings:  { damage:0, np:0, critical:0, utility:0, support:0, durability:0 }
  , actives:  [ { name:   "Concentration C"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 30.0 
                          , Grant Self 1 StarAbsorb 1000.0
                          ]
                }
              , { name:   "Mechanized Armor EX"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 1 AttackUp 25.0 
                          , Grant Self 1 Invincibility 0.0
                          ]
                }
              , { name:   "Overload D"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Self 1 NPUp 25.0 
                          , Debuff Self 5 Burn 300.0
                          ]
                }
              ]
  , passives: [itemConstructionFalse A]
  , phantasm: { name:   "Dimension of Steam"
              , desc:   "Gorgeous World of Ashes"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   4
              , effect: [ To Enemies Damage 600.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown 10.0 ]
              }
  , gen:      { starAbsorb: 48, starGen: 10.8, npPerHit: 0.91, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    36.0
  , align:    Chaotic:Neutral
  }
, { name:     "Cu Chulainn (Caster)"
  , rarity:   3
  , class:    Caster
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1222,  hp: 1728 }
              , max:   { atk: 6580, hp: 9604 }
              , grail: { atk: 8905, hp: 13022 }
              }
  , ratings:  { damage:2, np:4, critical:3, utility:2, support:1, durability:4 }
  , actives:  [ { name:   "Rune Spell A"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 3 CritUp 50.0 
                          , Grant Self 3 DebuffResist 50.0
                          ]
                }
              , { name:   "Divine Protection from Arrows A"
                , icon:   IconDodge
                , cd:     5
                , effect: [ Grant Self 0 Evasion 3.0 
                          , Grant Self 3 DefenseUp 18.0
                          ]
                }
              , { name:   "Disengage C"
                , icon:   IconBubbles
                , cd:     5
                , effect: [ To Self RemoveDebuffs 0.0 
                          , To Self Heal 1500.0
                          ]
                }
              ]
  , passives: [territoryCreation B, divinity B]
  , phantasm: { name:   "Wicker Man"
              , desc:   "Flame Cage that Burns All"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown 10.0 
                        , Debuff Enemies 10 Burn 300.0
                        ]
              }
  , gen:      { starAbsorb: 49, starGen: 10.9, npPerHit: 1.6, npAttacked: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Divine, Brynhildr, EnumaElish]
  , death:    42.0
  , align:    Lawful:Neutral
  }
, { name:     "Mephistopheles"
  , rarity:   3
  , class:    Caster
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1270,  hp: 1659 }
              , max:   { atk: 6839, hp: 9216 }
              , grail: { atk: 9255, hp: 12495 }
              }
  , ratings:  { damage:2, np:4, critical:2, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Curse A"
                , icon:   IconDarkMagic
                , cd:     5
                , effect: [ Chance 80 $ To Enemy GaugeDown 1.0 ]
                }
              , { name:   "Innocent Monster B"
                , icon:   IconStarTurn
                , cd:     5
                , effect: [ Grant Self 3 StarsPerTurn 9.0 
                          , Debuff Self 3 DefenseDown 18.0
                          ]
                }
              , { name:   "Clown's Laughter A+"
                , icon:   IconHoodX
                , cd:     6
                , effect: [ Debuff Enemy 0 BuffBlock 3.0 
                          , Debuff Enemy 5 Curse 1000.0
                          ]
                }
              ]
  , passives: [territoryCreation CPlus, itemConstruction B]
  , phantasm: { name:   "Ticktock Bomb"
              , desc:   "Slumbering Explosive"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies DamageThruDef 500.0 ]
              , over:   [ Debuff Enemies 3 Curse 500.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 11.0, npPerHit: 0.81, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    36.0
  , align:    Chaotic:Evil
  }
, { name:     "Paracelsus von Hohenheim"
  , rarity:   3
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1246,  hp: 1711 }
              , max:   { atk: 6711, hp: 9506 }
              , grail: { atk: 9082, hp: 12889 }
              }
  , ratings:  { damage:2, np:4, critical:1, utility:3, support:3, durability:2 }
  , actives:  [ { name:   "Rapid Casting A"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp 80.0 ]
                }
              , { name:   "Elemental A+"
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Party 3 ArtsUp 20.0 ]
                }
              , { name:   "Philosopher's Stone A"
                , icon:   IconKneel
                , cd:     8
                , effect: [ Grant Ally 3 Guts 1.0 ]
                }
              ]
  , passives: [territoryCreation A, itemConstruction EX]
  , phantasm: { name:   "Sword of Paracelsus"
              , desc:   "Magic Sword of the Elementalist"
              , rank:   APlus
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage 600.0 ]
              , over:   [ Debuff Enemies 3 AttackDown 10.0 ]
              }
  , gen:      { starAbsorb: 50, starGen: 10.8, npPerHit: 0.55, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    36.0
  , align:    Chaotic:Good
  }
, { name:     "Geronimo"
  , rarity:   3
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1273,  hp: 1642 }
              , max:   { atk: 6857, hp: 9123 }
              , grail: { atk: 9280, hp: 12369 }
              }
  , ratings:  { damage:3, np:2, critical:2, utility:1, support:3, durability:2 }
  , actives:  [ { name:   "Bloody Devil B"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 50.0 ]
                }
              , { name:   "Shamanism B"
                , icon:   IconArtsUp
                , cd:     5
                , effect: [ Grant Self 1 ArtsUp 50.0 ]
                }
              , { name:   "Guardian Beast B"
                , icon:   IconQuickUp
                , cd:     5
                , effect: [ Grant Self 1 QuickUp 50.0 ]
                }
              ]
  , passives: [territoryCreation B, itemConstruction C]
  , phantasm: { name:   "Tsago Degi Naleya"
              , desc:   "The One Who Makes the Earth"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage 700.0 
                        , Debuff Enemies 3 CritChance 20.0
                        ]
              , over:   [ To Party Heal 1000.0 
                        , Grant Party 3 DebuffResist 20.0
                        ]
              }
  , gen:      { starAbsorb: 49, starGen: 11.0, npPerHit: 0.9, npAttacked: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 4 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    40.5
  , align:    Neutral:Good
  }
, { name:     "Gilles de Rais (Caster)"
  , rarity:   3
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1209,  hp: 1711 }
              , max:   { atk: 6514, hp: 9506 }
              , grail: { atk: 10816, hp: 14889 }
              }
  , ratings:  { damage:2, np:4, critical:1, utility:3, support:2, durability:2 }
  , actives:  [ { name:   "Mental Corruption A"
                , icon:   IconStaffUp
                , cd:     5
                , effect: [ Grant Self 3 MentalSuccess 25.0 
                          , Grant Self 3 MentalResist 100.0 
                          ]
                }
              , { name:   "Aesthetic Appreciation E-"
                , icon:   IconBeamDown
                , cd:     5
                , effect: [ Debuff Enemy 1 NPDown 11.0 ]
                }
              , { name:   "Evil Eye of the Abyss C"
                , icon:   IconStun
                , cd:     8
                , effect: [ Debuff Enemies 5 Terror 40.0 ]
                }
              ]
  , passives: [territoryCreation B]
  , phantasm: { name:   "Prelati's Spellbook"
              , desc:   "Text of the Sunken Spiraled City "
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Debuff Enemies 3 AttackDown 20.0 ]
              }
  , gen:      { starAbsorb: 48, starGen: 10.8, npPerHit: 1.58, npAttacked: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    48.0
  , align:    Chaotic:Evil
  }

]
