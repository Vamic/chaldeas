module Database.Servant.Caster (casters) where

import Prelude
import Operators
import Database.Model

casters ∷ Array Servant
casters = [
  { name:     "Zhuge Liang (El-Melloi II)"
  , id:       37
  , rarity:   5
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1637,  hp: 2091 }
              , max:   { atk: 10598, hp: 14259 }
              , grail: { atk: 11601, hp: 15621 }
              }
  , actives:  [ { name:   "Discerning Eye A"
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Ally 3 CritUp $ 20.0 ~ 50.0
                          , To Ally GaugeUp $ Flat 30.0
                          ]
                }
              , { name:   "Tactician's Advice A+"
                , icon:   IconShieldUp
                , cd:     8
                , effect: [ Grant Party 3 DefenseUp $ 20.0 ~ 30.0
                          , Grant Party 3 ReduceDamage $ 200.0 ~ 500.0
                          , To Party GaugeUp $ Flat 10.0
                          ]
                }
              , { name:   "Tactician's Command A+"
                , icon:   IconSwordUp
                , cd:     8
                , effect: [ Grant Party 3 AttackUp $ 20.0 ~ 30.0
                          , Grant Party 3 DamageUp $ 200.0 ~ 500.0
                          , To Party GaugeUp $ Flat 10.0
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
              , effect: [ Chance 150
                          ∘ Debuff Enemies 3 DefenseDown $ 10.0 ~ 30.0
                        , Chance 150 ∘ Debuff Enemies 3 Curse $ Flat 500.0
                        , To Enemies GaugeDown $ Flat 1.0
                        ]
              , over:   [ Chances 50 80 $ Debuff Enemies 1 Stun Full ]
              , first:  false
              }
  , gen:      { starWeight: 50, starRate: 10.8, npAtk: 1.64, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhild, PseudoServant]
  , death:    34.5
  , align:    Neutral:Good
  , limited:  false
  , free:     false
  }
, { name:     "Tamamo no Mae"
  , id:       62
  , rarity:   5
  , class:    Caster
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1629,  hp: 2091 }
              , max:   { atk: 10546, hp: 14259 }
              , grail: { atk: 11544, hp: 15621 }
              }
  , actives:  [ { name:   "Curse EX"
                , icon:   IconDarkMagic
                , cd:     7
                , effect: [ Chances 80 100 ∘ To Enemy GaugeDown $ Flat 1.0 ]
                }
              , { name:   "Morph A"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 10.0 ~ 30.0
                          , Grant Self 1 DefenseUp $ Flat 30.0
                          ]
                }
              , { name:   "Fox's Wedding EX"
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Ally 3 (Boost Arts) $ 30.0 ~ 50.0
                          , To Ally Heal $ 1000.0 ~ 2500.0
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
              , effect: [ To Allies Cooldowns $ Flat 1.0
                        , To Allies Heal $ 2000.0 ~ 3000.0
                        ]
              , over:   [ To Allies GaugeUp $ 25.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 49, starRate: 11.0, npAtk: 0.32, npDef: 3 }
  , hits:     { quick: 3, arts: 5, buster: 1, ex: 4 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    36.0
  , align:    Neutral:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Xuanzang Sanzang"
  , id:       113
  , rarity:   5
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1801,  hp: 1901 }
              , max:   { atk: 11658, hp: 12965 }
              , grail: { atk: 12761, hp: 14204 }
              }
  , actives:  [ { name:   "Rapid Sutra Chanting A"
                , icon:   IconNoble
                , cd:     9
                , effect: [ To Self GaugeUp $ 50.0 ~ 80.0
                          , Grant Self 1 NPUp $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Captivating Rosy Cheeks A"
                , icon:   IconCrosshairUp
                , cd:     8
                , effect: [ Grant Self 1 Taunt Full
                          , Grant Self 1 ReduceDamage $ 500.0 ~ 1500.0
                          ]
                }
              , { name:   "Sanzang's Teaching A"
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Party 3 NPGen $ 10.0 ~ 30.0
                          , Grant Party 3 StarUp $ 10.0 ~ 30.0
                          , Grant Party 1 DebuffImmunity Full
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
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0 ]
              , over:   [ Debuff Enemy 1 CritChance $ 80.0 ~ 120.0 ]
              , first:  false
              }
  , gen:      { starWeight: 52, starRate: 11.0, npAtk: 0.82, npDef: 3 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 6 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    34.5
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
, { name:     "Leonardo da Vinci"
  , id:       127
  , rarity:   5
  , class:    Caster
  , attr:     Star
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1637,  hp: 2091 }
              , max:   { atk: 10598, hp: 14259 }
              , grail: { atk: 11601, hp: 15621 }
              }
  , actives:  [ { name:   "Inherent Wisdom EX"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 3 Guts $ 1000.0 ~ 3000.0
                          , Chance 85 ∘ Grant Self 3 DefenseUp $ 20.0 ~ 30.0
                          , Chance 85 ∘ Grant Self 3 NPUp $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Golden Rule (Body) B"
                , icon:   IconHealTurn
                , cd:     8
                , effect: [ Grant Self 2 DebuffImmunity Full
                          , Grant Self 3 HealPerTurn $ 500.0 ~ 1000.0
                          , Grant Self 3 GaugePerTurn $ Flat 10.0
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
  , passives: [territoryCreation A, itemConstruction A]
  , phantasm: { name:   "Uomo Universale"
              , desc:   "The Universal Man"
              , rank:   EX
              , card:   Arts
              , kind:   "Anti-Unit/Anti-Army"
              , hits:   1
              , effect: [ To Enemies DamageThruDef $ 450.0 ~ 750.0
                        , Debuff Enemies 3 CritChance $ Flat 10.0
                        ]
              , over:   [ Grant Self 1 NPUp $ 30.0 ~ 70.0 ]
              , first:  true
              }
  , gen:      { starWeight: 50, starRate: 10.8, npAtk: 0.54, npDef: 3 }
  , hits:     { quick: 4, arts: 3, buster: 1, ex: 4 }
  , traits:   [Female, EnumaElish]
  , death:    34.5
  , align:    Chaotic:Good
  , limited:  true
  , free:     false
  }
, { name:     "Illyasviel von Einzbern"
  , id:       136
  , rarity:   5
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1677,  hp: 2027 }
              , max:   { atk: 10857, hp: 13825 }
              , grail: { atk: 11885, hp: 15146 }
              }
  , actives:  [ { name:   "Happiness Mystic Code A"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Boost Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Child of Nature B"
                , icon:   IconShield
                , cd:     8
                , effect: [ Grant Self 1 Invincibility Full
                          , Grant Self 3 NPGen $ 18.0 ~ 28.0
                          ]
                }
              , { name:   "Suspicious Medicine A"
                , icon:   IconHeal
                , cd:     8
                , effect: [ To Ally Heal $ 1000.0 ~ 3000.0
                          , Chance 70 $ Grant Self 1 DebuffImmunity Full
                          , Chance 70 ∘ Grant Self 3 Guts $ Flat 1000.0
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
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0
                        , Debuff Self 3 AttackDown $ Flat 10.0
                        , Debuff Self 3 DefenseDown $ Flat 10.0
                        ]
              , over:   [ Grant Self 1 (Boost Buster) $ 20.0 ~ 80.0 ]
              , first:  true
              }
  , gen:      { starWeight: 51, starRate: 10.7, npAtk: 0.32, npDef: 3 }
  , hits:     { quick: 3, arts: 5, buster: 1, ex: 5 }
  , traits:   [Female, PseudoServant, EnumaElish]
  , death:    42.0
  , align:    Neutral:Good
  , limited:  true
  , free:     false
  }
, { name:     "Medea (Lily)"
  , id:       67
  , rarity:   4
  , class:    Caster
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1294,  hp: 2091 }
              , max:   { atk: 7766, hp: 13070 }
              , grail: { atk: 9403, hp: 15847 }
              }
  , actives:  [ { name:   "Rapid Words of Divine A"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 80.0 ~ 150.0 ]
                }
              , { name:   "Poison Resistance A++"
                , icon:   IconBubbles
                , cd:     7
                , effect: [ To Party Cure Full
                          , To Party Heal $ 2000.0 ~ 3000.0
                          ]
                }
              , { name:   "Ephemeral Love B"
                , icon:   IconHeal
                , cd:     10
                , effect: [ Grant Ally 1 HealingReceived $ 50.0 ~ 100.0 ]
                }
              ]
  , passives: [territoryCreation B, itemConstruction B]
  , phantasm: { name:   "Pain Breaker"
              , desc:   "All Flaws Must Be Repaired"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Magic"
              , hits:   0
              , effect: [ To Allies RemoveDebuffs Full
                        , To Party Heal $ 4000.0 ~ 6000.0
                        ]
              , over:   [ Grant Party 3 DebuffResist $ 40.0 ~ 120.0 ]
              , first:  false
              }
  , gen:      { starWeight: 51, starRate: 10.7, npAtk: 0.4, npDef: 3 }
  , hits:     { quick: 4, arts: 4, buster: 3, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    36.0
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
, { name:     "Elisabeth Bathory (Halloween)"
  , id:       61
  , rarity:   4
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1436,  hp: 1824 }
              , max:   { atk: 8616, hp: 11404 }
              , grail: { atk: 10432, hp: 13827 }
              }
  , actives:  [ { name:   "Innocent Monster EX"
                , icon:   IconStarTurn
                , cd:     7
                , effect: [ Grant Self 3 StarsPerTurn $ 6.0 ~ 12.0
                          , To Self Heal $ 1000.0 ~ 2000.0
                          ]
                }
              , { name:   "Mana Burst (Pumpkin) A"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Boost Buster) $ 25.0 ~ 45.0
                          , Debuff Enemies 10 Burn $ Flat 300.0
                          ]
                }
              , { name:   "Performance Continuation A"
                , icon:   IconKneel
                , cd:     9
                , effect: [ Grant Self 5 Guts $ 1000.0 ~ 2500.0
                          , To Party GainStars $ Flat 8.0
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
              , effect: [ To Enemies DamageThruDef $ 300.0 ~ 500.0 ]
              , over:   [ Debuff Enemies 3 Curse $ 500.0 ~ 2500.0 ]
              , first:  false
              }
  , gen:      { starWeight: 49, starRate: 10.8, npAtk: 1.6, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 4 }
  , traits:   [Female, Dragon, EnumaElish]
  , death:    42.0
  , align:    Chaotic:Evil
  , limited:  true
  , free:     true
  }
, { name:     "Nursery Rhyme"
  , id:       74
  , rarity:   4
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1438,  hp: 1901 }
              , max:   { atk: 8629, hp: 11882 }
              , grail: { atk: 10448, hp: 14407 }
              }
  , actives:  [ { name:   "Self-Modification A"
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 3 CritUp $ 20.0 ~ 50.0
                          , Grant Self 3 StarWeight $ 300.0 ~ 600.0
                          ]
                }
              , { name:   "Morph A+"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 10.0 ~ 30.0
                          , Grant Self 1 DefenseUp $ Flat 30.0
                          , Grant Self 3 DebuffResist $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Meanwhile A"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 20.0 ~ 40.0
                          , To Self Heal $ 1000.0 ~ 2000.0
                          , To Self RemoveDebuffs Full
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
              , effect: [ To Enemies Damage $ 600.0 ~ 900.0
                        , Debuff Enemies 3 DefenseDown $ Flat 20.0
                        ]
              , over:   [ Chances 60 100 $ To Enemies GaugeDown Full ]
              , first:  false
              }
  , gen:      { starWeight: 50, starRate: 10.8, npAtk: 0.54, npDef: 3 }
  , hits:     { quick: 3, arts: 3, buster: 1, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    36.0
  , align:    Neutral:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Helena Blavatsky"
  , id:       100
  , rarity:   4
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1438,  hp: 1901 }
              , max:   { atk: 8629, hp: 11882 }
              , grail: { atk: 10448, hp: 14407 }
              }
  , actives:  [ { name:   "Mana Tuning C"
                , icon:   IconNoble
                , cd:     9
                , effect: [ To Party GaugeUp $ 10.0 ~ 20.0 ]
                }
              , { name:   "Mahatma A"
                , icon:   IconStarTurn
                , cd:     10
                , effect: [ Grant Self 5 StarsPerTurn $ Flat 5.0
                          , Chances 60 80 ∘ Grant Self 1 NPUp $ Flat 50.0
                          ]
                }
              , { name:   "Search for the Unknown B"
                , icon:   IconAllUp
                , cd:     9
                , effect: [ Grant Party 3 (Boost Quick) $ 15.0 ~ 20.0
                          , Grant Party 3 (Boost Arts) $ 15.0 ~ 20.0
                          , Grant Party 3 (Boost Buster) $ 15.0 ~ 20.0
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
              , effect: [ To Enemies Damage $ 450.0 ~ 750.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown $ 10.0 ~ 50.0
                        , Debuff Enemies 3 CritChance $ 10.0 ~ 50.0
                        , Debuff Enemies 3 DebuffVuln $ 10.0 ~ 50.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 51, starRate: 10.7, npAtk: 0.45, npDef: 3 }
  , hits:     { quick: 6, arts: 3, buster: 1, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    36.0
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Nitocris"
  , id:       120
  , rarity:   4
  , class:    Caster
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1510,  hp: 1806 }
              , max:   { atk: 9060, hp: 11288 }
              , grail: { atk: 10970, hp: 13686 }
              }
  , actives:  [ { name:   "Egyptian Magecraft A"
                , icon:   IconReaperUp
                , cd:     7
                , effect: [ Grant Self 3 KillUp $ 50.0 ~ 100.0
                          , To Self Heal $ 1000.0 ~ 3000.0
                          ]
                }
              , { name:   "Rapid Words of Divine B"
                , icon:   IconNoble
                , cd:     9
                , effect: [ To Self GaugeUp $ 60.0 ~ 120.0 ]
                }
              , { name:   "Affection of the Sky God B"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 3 Guts $ 1000.0 ~ 2000.0
                          , To Self RemoveDebuffs Full
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
              , effect: [ To Enemies Damage $ 450.0 ~ 750.0 ]
              , over:   [ To Enemies Kill $ 50.0 ~ 100.0 ]
              , first:  true
              }
  , gen:      { starWeight: 50, starRate: 10.8, npAtk: 0.54, npDef: 3 }
  , hits:     { quick: 4, arts: 3, buster: 3, ex: 5 }
  , traits:   [Female, Divine, King, EnumaElish]
  , death:    36.0
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
, { name:     "Irisviel (Dress of Heaven)"
  , id:       111
  , rarity:   4
  , class:    Caster
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1372,  hp: 1996 }
              , max:   { atk: 8237, hp: 12476 }
              , grail: { atk: 9973, hp: 15127 }
              }
  , actives:  [ { name:   "Sacrificial Resolve A"
                , icon:   IconHealUp
                , cd:     9
                , effect: [ Grant Self 1 HealUp$ 30.0 ~ 50.0 ]
                }
              , { name:   "Child of Nature A"
                , icon:   IconShield
                , cd:     8
                , effect: [ Grant Self 1 Invincibility Full
                          , Grant Self 3 NPGen $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Magical Healing A"
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Ally Heal $ 2000.0 ~ 3000.0 ]
                }
              ]
  , passives: [territoryCreation B, coreOfGoddess C]
  , phantasm: { name:   "Song of Grail"
              , desc:   "Sing Out, O' White Grail"
              , rank:   B
              , card:   Arts
              , kind:   "Magecraft"
              , hits:   0
              , effect: [ To Party Heal $ 2000.0 ~ 3000.0 ]
              , over:   [ Grant Party 3 GutsUnlimited $ 1000.0 ~ 3000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 50, starRate: 10.8, npAtk: 0.42, npDef: 3 }
  , hits:     { quick: 3, arts: 3, buster: 3, ex: 4 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    34.5
  , align:    Lawful:Good
  , limited:  true
  , free:     true
  }
, { name:     "Marie Antoinette (Caster)"
  , id:       130
  , rarity:   4
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1510,  hp: 1824 }
              , max:   { atk: 9060, hp: 11404 }
              , grail: { atk: 10970, hp: 13827 }
              }
  , actives:  [ { name:   "Beach Flower A+"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.5 ~ 19.5
                          , Grant (AlliesType Male) 3 StarUp $ 21.0 ~ 41.0
                          ]
                }
              , { name:   "Sparkling Sunflower A"
                , icon:   IconStarTurn
                , cd:     9
                , effect: [ Grant Self 3 StarsPerTurn $ 5.0 ~ 10.0
                          , Grant Self 3 HealPerTurn $ 500.0 ~ 1000.0
                          ]
                }
              , { name:   "Beautiful Princess (Sea) A"
                , icon:   IconShield
                , cd:     8
                , effect: [ Grant Self 0 Invincibility $ Flat 3.0
                          , Grant Self 3 DebuffResist $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [territoryCreation A, itemConstruction D]
  , phantasm: { name:   "Crystal Dress"
              , desc:   "Precious Brilliance Everlasting"
              , rank:   A
              , card:   Arts
              , kind:   "Anti-Army/Anti-Populace"
              , hits:   3
              , effect: [ To Enemies Damage $ 450.0 ~ 750.0
                        , Debuff Enemies 3 CritChance $ Flat 20.0
                        ]
              , over:   [ Grant Party 3 CritUp $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 50, starRate: 10.9, npAtk: 0.32, npDef: 3 }
  , hits:     { quick: 3, arts: 5, buster: 1, ex: 4 }
  , traits:   [Female, EnumaElish]
  , death:    36.0
  , align:    Lawful:Good
  , limited:  true
  , free:     false
  }
, { name:     "Thomas Edison"
  , id:       103
  , rarity:   4
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1325,  hp: 1901 }
              , max:   { atk: 7952, hp: 11882 }
              , grail: { atk: 9628, hp: 14407 }
              }
  , actives:  [ { name:   "Morph C"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 16.0 ~ 24.0 ]
                }
              , { name:   "Mass Production A"
                , icon:   IconStarTurn
                , cd:     12
                , effect: [ Grant Self 5 StarsPerTurn $ 5.0 ~ 10.0
                          , Grant Self 5 GaugePerTurn $ 5.0 ~ 10.0
                          ]
                }
              , { name:   "Concept Improvement A+"
                , icon:   IconSunUp
                , cd:     8
                , effect: [ Grant Ally 1 Overcharge $ Flat 2.0
                          , Grant Ally 1 StarUp $ 10.0 ~ 30.0
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
              , effect: [ To Enemies Damage $ 450.0 ~ 750.0
                        , Debuff Enemies 1 SealSkills Full
                        , Debuff Enemies 1 SealNP Full
                        ]
              , over:   [ Debuff Enemies 3 CritChance $ 10.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 51, starRate: 10.6, npAtk: 0.51, npDef: 3 }
  , hits:     { quick: 3, arts: 3, buster: 3, ex: 5 }
  , traits:   [Male, EnumaElish]
  , death:    60.0
  , align:    Lawful:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Hans Christian Andersen"
  , id:       33
  , rarity:   2
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1021,  hp: 1597 }
              , max:   { atk: 5758, hp: 8484 }
              , grail: { atk: 8344, hp: 12244 }
              }
  , actives:  [ { name:   "Human Observation A"
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Party 3 CritUp $ 10.0 ~ 40.0 ]
                }
              , { name:   "Rapid Casting E"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 50.0 ~ 75.0 ]
                }
              , { name:   "Innocent Monster D"
                , icon:   IconStarTurn
                , cd:     7
                , effect: [ Grant Self 3 StarsPerTurn $ 3.0 ~ 9.0
                          , Debuff Self 3 DefenseDown $ Flat 14.0
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
              , effect: [ To Self OverChance $ 60.0 ~ 80.0 ]
              , over:   [ Chance 0 ∘ Grant Party 3 AttackUp $ 20.0 ~ 40.0
                        , Chance 0 ∘ Grant Party 3 DefenseUp $ 20.0 ~ 40.0
                        , Chance 0 ∘ Grant Party 3 StarUp $ 20.0 ~ 40.0
                        , To Party Heal $ 1000.0 ~ 3000.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 48, starRate: 10.8, npAtk: 1.66, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    30.0
  , align:    Lawful:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Medea"
  , id:       31
  , rarity:   3
  , class:    Caster
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1377,  hp: 1555 }
              , max:   { atk: 7418, hp: 8643 }
              , grail: { atk: 10039, hp: 11719 }
              }
  , actives:  [ { name:   "Rapid Words of Divine A"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 80.0 ~ 150.0 ]
                }
              , { name:   "Argon Coin"
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Self Heal $ 500.0 ~ 2500.0 ]
                }
              , { name:   "Circe's Teaching A"
                , icon:   IconBubbles
                , cd:     8
                , effect: [ To Ally RemoveDebuffs Full
                          , Grant Ally 1 NPGen $ 30.0 ~ 50.0
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
              , effect: [ To Enemy Damage $ 600.0 ~ 900.0
                        , To Enemy RemoveBuffs Full
                        ]
              , over:   [ To Self GaugeUp $ 20.0 ~ 100.0 ]
              , first:  false
              }
  , gen:      { starWeight: 50, starRate: 10.9, npAtk: 1.64, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    34.5
  , align:    Neutral:Evil
  , limited:  false
  , free:     false
  }
, { name:     "William Shakespeare"
  , id:       34
  , rarity:   2
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1028,  hp: 1520 }
              , max:   { atk: 5798, hp: 8080 }
              , grail: { atk: 8402, hp: 11661 }
              }
  , actives:  [ { name:   "Enchant A"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Party 1 (Boost Buster) $ 20.0 ~ 40.0 ]
                }
              , { name:   "Self-Preservation B"
                , icon:   IconShield
                , cd:     8
                , effect: [ Grant Self 1 Invincibility Full
                          , To Self Heal $ 500.0 ~ 1500.0
                          ]
                }
              , { name:   "King's Men C"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Ally GaugeUp $ Flat 20.0
                          , Grant Ally 1 StarUp $ 50.0 ~ 100.0
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
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ Debuff Enemies 1 Stun $ 30.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 50, starRate: 10.8, npAtk: 1.59, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    45.0
  , align:    Neutral:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Wolfgang Amadeus Mozart"
  , id:       36
  , rarity:   1
  , class:    Caster
  , attr:     Star
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 944,  hp: 1425 }
              , max:   { atk: 5195, hp: 7129 }
              , grail: { atk: 8072, hp: 10990 }
              }
  , actives:  [ { name:   "Protection of Muse (Fake) EX"
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Party 1 (Boost Arts) $ 22.0 ~ 44.0 ]
                }
              , { name:   "Aesthetic Appreciation B"
                , icon:   IconBeamDown
                , cd:     7
                , effect: [ Debuff Enemy 1 NPDown $ 9.0 ~ 18.0 ]
                }
              , { name:   "Eine kleine Nachtmusik EX"
                , icon:   IconStar
                , cd:     8
                , effect: [ To Party GainStars $ 20.0 ~ 50.0 ]
                }
              ]
  , passives: [territoryCreation B]
  , phantasm: { name:   "Requiem for Death"
              , desc:   "Funeral Music for the Death God"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ To Self OverChance $ 60.0 ~ 80.0 ]
              , over:   [ Chance 0 ∘ Debuff Enemies 3 AttackDown $ 20.0 ~ 40.0
                        , Chance 0 ∘ Debuff Enemies 3 DefenseDown $ 20.0 ~ 40.0
                        , Debuff Enemies 3 Curse $ 500.0 ~ 2500.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 49, starRate: 11.0, npAtk: 1.6, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Male, Brynhild]
  , death:    40.5
  , align:    Neutral:Good
  , limited:  false
  , free:     false
  }
, { name:     "Charles Babbage"
  , id:       80
  , rarity:   3
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1113,  hp: 1959 }
              , max:   { atk: 5996, hp: 10887 }
              , grail: { atk: 8115, hp: 14761 }
              }
  , actives:  [ { name:   "Concentration C"
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 20.0 ~ 30.0
                          , Grant Self 1 StarWeight $ 500.0 ~ 1000.0
                          ]
                }
              , { name:   "Mechanized Armor EX"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 1 AttackUp $ 15.0 ~ 25.0
                          , Grant Self 1 Invincibility Full
                          ]
                }
              , { name:   "Overload D"
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Self 1 NPUp $ 15.0 ~ 25.0
                          , Debuff Self 5 Burn $ Flat 300.0
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
              , effect: [ To Enemies Damage $ 400.0 ~ 600.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown $ 10.0 ~ 20.0 ]
              , first:  false
              }
  , gen:      { starWeight: 48, starRate: 10.8, npAtk: 0.91, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    36.0
  , align:    Chaotic:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Cu Chulainn (Caster)"
  , id:       38
  , rarity:   3
  , class:    Caster
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1222,  hp: 1728 }
              , max:   { atk: 6580, hp: 9604 }
              , grail: { atk: 8905, hp: 13022 }
              }
  , actives:  [ { name:   "Rune Spell A"
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 3 CritUp $ 20.0 ~ 50.0
                          , Grant Self 3 DebuffResist $ 20.0 ~ 50.0
                          ]
                }
              , { name:   "Divine Protection from Arrows A"
                , icon:   IconDodge
                , cd:     7
                , effect: [ Grant Self 0 Evasion $ Flat 3.0
                          , Grant Self 3 DefenseUp $ 9.0 ~ 18.0
                          ]
                }
              , { name:   "Disengage C"
                , icon:   IconBubbles
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , To Self Heal $ 500.0 ~ 1500.0
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
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown $ 10.0 ~ 30.0
                        , Debuff Enemies 10 Burn $ 300.0 ~ 1500.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 49, starRate: 10.9, npAtk: 1.6, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Male, Divine, Brynhild, EnumaElish]
  , death:    42.0
  , align:    Lawful:Balanced
  , limited:  false
  , free:     true
  }
, { name:     "Mephistopheles"
  , id:       35
  , rarity:   3
  , class:    Caster
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1270,  hp: 1659 }
              , max:   { atk: 6839, hp: 9216 }
              , grail: { atk: 9255, hp: 12495 }
              }
  , actives:  [ { name:   "Curse A"
                , icon:   IconDarkMagic
                , cd:     7
                , effect: [ Chances 60 80 ∘ To Enemy GaugeDown $ Flat 1.0 ]
                }
              , { name:   "Innocent Monster B"
                , icon:   IconStarTurn
                , cd:     7
                , effect: [ Grant Self 3 StarsPerTurn $ 3.0 ~ 9.0
                          , Debuff Self 3 DefenseDown $ Flat 18.0
                          ]
                }
              , { name:   "Clown's Laughter A+"
                , icon:   IconHoodX
                , cd:     8
                , effect: [ Debuff Enemy 0 BuffBlock $ Flat 3.0
                          , Debuff Enemy 5 Curse $ 500.0 ~ 1000.0
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
              , effect: [ To Enemies DamageThruDef $ 400.0 ~ 600.0 ]
              , over:   [ Debuff Enemies 3 Curse $ 500.0 ~ 2500.0 ]
              , first:  false
              }
  , gen:      { starWeight: 50, starRate: 11.0, npAtk: 0.81, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    36.0
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Paracelsus von Hohenheim"
  , id:       79
  , rarity:   3
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1246,  hp: 1711 }
              , max:   { atk: 6711, hp: 9506 }
              , grail: { atk: 9082, hp: 12889 }
              }
  , actives:  [ { name:   "Rapid Casting A"
                , icon:   IconNoble
                , cd:     10
                , effect: [ To Self GaugeUp $ 55.0 ~ 80.0 ]
                }
              , { name:   "Elemental A+"
                , icon:   IconArtsUp
                , cd:     9
                , effect: [ Grant Party 3 (Boost Arts) $ 10.0 ~ 20.0 ]
                }
              , { name:   "Philosopher's Stone A"
                , icon:   IconKneel
                , cd:     10
                , effect: [ Grant Ally 3 Guts $ 1000.0 ~ 3000.0 ]
                }
              ]
  , passives: [territoryCreation A, itemConstruction EX]
  , phantasm: { name:   "Sword of Paracelsus"
              , desc:   "Magic Sword of the Elementalist"
              , rank:   APlus
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage $ 400.0 ~ 600.0 ]
              , over:   [ Debuff Enemies 3 AttackDown $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 50, starRate: 10.8, npAtk: 0.55, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    36.0
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Geronimo"
  , id:       104
  , rarity:   3
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1273,  hp: 1642 }
              , max:   { atk: 6857, hp: 9123 }
              , grail: { atk: 9280, hp: 12369 }
              }
  , actives:  [ { name:   "Bloody Devil B"
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Boost Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Shamanism B"
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Self 1 (Boost Arts) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Guardian Beast B"
                , icon:   IconQuickUp
                , cd:     7
                , effect: [ Grant Self 1 (Boost Quick) $ 30.0 ~ 50.0 ]
                }
              ]
  , passives: [territoryCreation B, itemConstruction C]
  , phantasm: { name:   "Tsago Degi Naleya"
              , desc:   "The One Who Makes the Earth"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage $ 400.0 ~ 700.0
                        , Debuff Enemies 3 CritChance $ Flat 20.0
                        ]
              , over:   [ To Party Heal $ 1000.0 ~ 2000.0
                        , Grant Party 3 DebuffResist $ 20.0 ~ 60.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 49, starRate: 11.0, npAtk: 0.9, npDef: 3 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 4 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    40.5
  , align:    Neutral:Good
  , limited:  false
  , free:     true
  }
, { name:     "Gilles de Rais (Caster)"
  , id:       32
  , rarity:   3
  , class:    Caster
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1209,  hp: 1711 }
              , max:   { atk: 6514, hp: 9506 }
              , grail: { atk: 8816, hp: 12889 }
              }
  , actives:  [ { name:   "Mental Corruption A"
                , icon:   IconStaffUp
                , cd:     7
                , effect: [ Grant Self 3 MentalSuccess $ 5.0 ~ 25.0
                          , Grant Self 3 MentalResist $ 50.0 ~ 100.0
                          ]
                }
              , { name:   "Aesthetic Appreciation E-"
                , icon:   IconBeamDown
                , cd:     7
                , effect: [ Debuff Enemy 1 NPDown $ 5.5 ~ 11.0 ]
                }
              , { name:   "Evil Eye of the Abyss C"
                , icon:   IconStun
                , cd:     10
                , effect: [ Debuff Enemies 5 Terror $ 30.0 ~ 40.0 ]
                }
              ]
  , passives: [territoryCreation B]
  , phantasm: { name:   "Prelati's Spellbook"
              , desc:   "Text of the Sunken Spiraled City "
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ Debuff Enemies 3 AttackDown $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 48, starRate: 10.8, npAtk: 1.58, npDef: 3 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    48.0
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }

]
