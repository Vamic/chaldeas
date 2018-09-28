module Database.Servant.Saber (sabers) where

import StandardLibrary
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
  , skills:   [ { name:   "Shukuchi"
                , rank:   B
                , icon:   IconQuickUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Quick) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Weak Constitution"
                , rank:   A
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 1 StarAbsorb $ 500.0 ~ 1000.0 ]
                }
              , { name:   "Mind's Eye (Fake)"
                , rank:   A
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
  , align:    [Neutral, Balanced]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 5]
              [Piece Saber: 12, Void'sDust: 15]
              [Monument Saber: 5, DragonFang: 24, EternalGear: 5]
              [Monument Saber: 12, EternalGear: 10, PhoenixFeather: 10]
  , skillUp:  Reinforcement
              [GemOf Saber: 5]
              [GemOf Saber: 12]
              [MagicGemOf Saber: 5]
              [MagicGemOf Saber: 12, DragonFang: 12]
              [SecretGemOf Saber: 5, DragonFang: 24]
              [SecretGemOf Saber: 12, Void'sDust: 10]
              [Void'sDust: 20, ClawOfChaos: 4]
              [ClawOfChaos: 11, PhoenixFeather: 20]
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
  , skills:   [ { name:   "Tactics"
                , rank:   B
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Natural Body"
                , rank:   D
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 OffensiveResist $ 40.0 ~ 80.0
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              , { name:   "Crest of the Star"
                , rank:   EX
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
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 5]
              [Piece Saber: 12, Void'sDust: 15]
              [Monument Saber: 5, ClawOfChaos: 6, MeteorHorseshoe: 5]
              [Monument Saber: 12, MeteorHorseshoe: 10, HeartOfTheForeignGod: 5]
  , skillUp:  Reinforcement
              [GemOf Saber: 5]
              [GemOf Saber: 12]
              [MagicGemOf Saber: 5]
              [MagicGemOf Saber: 12, ClawOfChaos: 3]
              [SecretGemOf Saber: 5, ClawOfChaos: 6]
              [SecretGemOf Saber: 12, Void'sDust: 10]
              [Void'sDust: 20, EvilBone: 18]
              [EvilBone: 54, HeartOfTheForeignGod: 10]
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
  , skills:   [ { name:   "Stars for the Sky"
                , rank:   A
                , icon:   IconNobleUp
                , cd:     9
                , effect: [ Grant Ally 3 NPGen $ 35.0 ~ 45.0 ]
                }
              , { name:   "Flowers for the Earth"
                , rank:   A
                , icon:   IconSwordUp
                , cd:     8
                , effect: [ Grant Ally 3 AttackUp $ 30.0 ~ 40.0
                          , Grant Ally 3 StarUp $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Love for the People"
                , rank:   A
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Ally Heal $ 1000.0 ~ 3000.0
                          , Grant Ally 3 DefenseUp $ 10.0 ~ 20.0
                          ]
                }
              ]
  , passives: [magicResistance C, riding B]
  , phantasm: { name:   "Fax Caelestis"
              , desc:   "Ending of the Rose of Prominence"
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
  , align:    [Chaotic, Bride]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 5]
              [Piece Saber: 12, OctupletCrystals: 8]
              [Monument Saber: 5, PhoenixFeather: 4, GhostLantern: 10]
              [Monument Saber: 12, SeedOfYggdrasil: 15, PhoenixFeather: 8]
  , skillUp:  Reinforcement
              [GemOf Saber: 5]
              [GemOf Saber: 12]
              [MagicGemOf Saber: 5]
              [MagicGemOf Saber: 12, GhostLantern: 5]
              [SecretGemOf Saber: 5, GhostLantern: 10]
              [SecretGemOf Saber: 12, OctupletCrystals: 5]
              [OctupletCrystals: 10, SerpentJewel: 5]
              [SerpentJewel: 15, EternalGear: 24]
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
  , skills:   [ { name:   "Mana Burst"
                , rank:   A
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Intuition"
                , rank:   B
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Secret of Pedigree"
                , rank:   EX
                , icon:   IconShieldUp
                , cd:     8
                , effect: [ Grant Self 1 DefenseUp $ 30.0 ~ 50.0
                          , To Self RemoveDebuffs Full
                          , To Self GaugeUp $ 10.0 ~ 30.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding B]
  , phantasm: { name:   "Clarent Blood Arthur"
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
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 5]
              [Piece Saber: 12, DragonFang: 18]
              [Monument Saber: 5, OctupletCrystals: 10, HeartOfTheForeignGod: 2]
              [ Monument Saber: 12, HeartOfTheForeignGod: 4
              , Dragon'sReverseScale: 5
              ]
  , skillUp:  Reinforcement
              [GemOf Saber: 5]
              [GemOf Saber: 12]
              [MagicGemOf Saber: 5]
              [MagicGemOf Saber: 12, OctupletCrystals: 5]
              [SecretGemOf Saber: 5, OctupletCrystals: 10]
              [SecretGemOf Saber: 12, DragonFang: 12]
              [DragonFang: 24, ClawOfChaos: 4]
              [ClawOfChaos: 11, Dragon'sReverseScale: 10]
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
  , skills:   [ { name:   "Mystic Eyes of Death Perception"
                , rank:   C
                , icon:   IconMystic
                , cd:     7
                , effect: [ Grant Self 1 IgnoreInvinc Full
                          , Grant Self 1 (Performance Arts) $ 25.0 ~ 40.0
                          , Debuff Enemies 1 DeathDown $ 60.0 ~ 80.0
                          ]
                }
              , { name:   "Unyou"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 15.0 ~ 25.0
                          , Grant Self 3 MentalResist $ 18.0 ~ 36.0
                          ]
                }
              , { name:   "Yin-Yang"
                , rank:   A
                , icon:   IconYinYang
                , cd:     6
                , effect: [ To Self Heal $ 2000.0 ~ 5000.0
                          , To Self DemeritGauge $ Flat 10.0
                          ]
                }
              ]
  , passives: [magicResistance A, connectionRoot A, independentManifestation C]
  , phantasm: { name:   "Amalavijñāna—Boundary of Emptiness"
              , desc:   "Mukushiki Kara no Kyoukai"
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
  , align:    [Neutral, Balanced]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 5]
              [Piece Saber: 12, Void'sDust: 15]
              [Monument Saber: 5, PhoenixFeather: 8, ClawOfChaos: 3]
              [Monument Saber: 12, ClawOfChaos: 6, HeartOfTheForeignGod: 5]
  , skillUp:  Reinforcement
              [GemOf Saber: 5]
              [GemOf Saber: 12]
              [MagicGemOf Saber: 5]
              [MagicGemOf Saber: 12, PhoenixFeather: 4]
              [SecretGemOf Saber: 5, PhoenixFeather: 8]
              [SecretGemOf Saber: 12, Void'sDust: 10]
              [Void'sDust: 20, OctupletCrystals: 6]
              [OctupletCrystals: 18, GhostLantern: 24]
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
  , skills:   [ { name:   "Charisma"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Mana Burst"
                , rank:   A
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Intuition"
                , rank:   A
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
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 5]
              [Piece Saber: 12, DragonFang: 18]
              [Monument Saber: 5, ProofOfHero: 29, PhoenixFeather: 4]
              [Monument Saber: 12, PhoenixFeather: 8, Dragon'sReverseScale: 5]
  , skillUp:  Reinforcement
              [GemOf Saber: 5]
              [GemOf Saber: 12]
              [MagicGemOf Saber: 5]
              [MagicGemOf Saber: 12, ProofOfHero: 15]
              [SecretGemOf Saber: 5, ProofOfHero: 29]
              [SecretGemOf Saber: 12, DragonFang: 12]
              [DragonFang: 24, ClawOfChaos: 4]
              [ClawOfChaos: 11, Dragon'sReverseScale: 10]
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
  , skills:   [ { name:   "Knight of the Lake"
                , rank:   A
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ Flat 30.0
                          , To Party GainStars $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Eternal Arms Mastery"
                , rank:   APlus
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 3 StarAbsorb $ 3000.0 ~ 6000.0 ]
                }
              , { name:   "Knight of Owner"
                , rank:   APlusPlus
                , icon:   IconStarTurn
                , cd:     8
                , effect: [ Grant Party 3 StarsPerTurn $ 5.0 ~ 15.0
                          , Grant Self 3 CritUp $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding B]
  , phantasm: { name:   "Arondight Overload"
              , desc:   "Severance of the Binding Chains—Lake's Overflowing Light"
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
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 10, EternalGear: 6]
              [Monument Saber: 4, GreatKnightMedal: 16, DragonFang: 10]
              [Monument Saber: 10, DragonFang: 20, Dragon'sReverseScale: 4]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 10]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 10, GreatKnightMedal: 8]
              [SecretGemOf Saber: 4, GreatKnightMedal: 16]
              [SecretGemOf Saber: 10, EternalGear: 4]
              [EternalGear: 8, Void'sDust: 10]
              [Void'sDust: 30, TearstoneOfBlood: 12]
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
  , skills:   [ { name:   "Mana Burst"
                , rank:   A
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Intuition"
                , rank:   B
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Charisma"
                , rank:   E
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
  , align:    [Lawful, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 10, DragonFang: 15]
              [Monument Saber: 4, ClawOfChaos: 5, Dragon'sReverseScale: 2]
              [ Monument Saber: 10, Dragon'sReverseScale: 4
              , HeartOfTheForeignGod: 4
              ]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 10]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 10, ClawOfChaos: 3]
              [SecretGemOf Saber: 4, ClawOfChaos: 5]
              [SecretGemOf Saber: 10, DragonFang: 10]
              [DragonFang: 20, Void'sDust: 10]
              [Void'sDust: 30, HeartOfTheForeignGod: 8]
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
  , skills:   [ { name:   "Blessing of Martial Arts"
                , rank:   A
                , icon:   IconStarUp
                , cd:     6
                , effect: [ Grant Self 1 StarAbsorb $ 300.0 ~ 500.0
                          , Grant Self 1 CritUp $ 50.0 ~ 100.0
                          ]
                }
              , { name:   "Charisma"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Curse of Separation"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 3 Guts $ Flat 1000.0
                          , To Self Heal $ 1000.0 ~ 3000.0
                          ]
                }
              ]
  , passives: [magicResistance A, riding APlus, divinity A]
  , phantasm: { name:   "Brahmastra"
              , desc:   "The Rakshasa-Piercing Immortal"
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
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 10, SeedOfYggdrasil: 8]
              [Monument Saber: 4, HeartOfTheForeignGod: 4, OctupletCrystals: 4]
              [Monument Saber: 10, OctupletCrystals: 8, SpiritRoot: 4]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 10]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 10, HeartOfTheForeignGod: 2]
              [SecretGemOf Saber: 4, HeartOfTheForeignGod: 4]
              [SecretGemOf Saber: 10, SeedOfYggdrasil: 5]
              [SeedOfYggdrasil: 10, ClawOfChaos: 3]
              [ClawOfChaos: 9, EvilBone: 60]
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
  , skills:   [ { name:   "Migraine"
                , rank:   B
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 MentalResist $ 50.0 ~ 100.0
                          , To Self Heal $ 500.0 ~ 2000.0
                          ]
                }
              , { name:   "Imperial Privilege"
                , rank:   EX
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Self Heal $ 1200.0 ~ 3400.0
                          , Chance 60 <<< Grant Self 3 AttackUp $ 22.0 ~ 44.0
                          , Chance 60 <<< Grant Self 3 DefenseUp $ 22.0 ~ 44.0
                          ]
                }
              , { name:   "Invictus Spiritus"
                , rank:   A
                , icon:   IconKneel
                , cd:     12
                , effect: [ Times 3 <<< Grant Self 5 Guts $ 300.0 ~ 600.0 ]
                }
              ]
  , passives: [magicResistance C, riding B]
  , phantasm: { name:   "Laus St. Claudius"
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
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 10, OctupletCrystals: 6]
              [Monument Saber: 4, PhoenixFeather: 7, GhostLantern: 4]
              [Monument Saber: 10, GhostLantern: 8, EternalGear: 10]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 10]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 10, PhoenixFeather: 4]
              [SecretGemOf Saber: 4, PhoenixFeather: 7]
              [SecretGemOf Saber: 10, OctupletCrystals: 4]
              [OctupletCrystals: 8, DragonFang: 12]
              [DragonFang: 36, EternalGear: 20]
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
  , skills:   [ { name:   "Mind's Eye (True)"
                , rank:   C
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 DefenseUp $ 8.0 ~ 16.0
                          ]
                }
              , { name:   "Self-Suggestion"
                , rank:   A
                , icon:   IconBubbles
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , Grant Self 3 DebuffResist $ 50.0 ~ 100.0
                          ]
                }
              , { name:   "Beautiful Appearance"
                , rank:   C
                , icon:   IconFace
                , cd:     7
                , effect: [ Grant Self 3 Taunt Full
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              ]
  , passives: [magicResistance C, riding B]
  , phantasm: { name:   "Fleur de Lis"
              , desc:   "Gorgeous Blooming Lilies"
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
  , traits:   [Nonbinary, Riding, EnumaElish]
  , death:    28.0
  , align:    [Neutral, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 10, ProofOfHero: 18]
              [Monument Saber: 4, SeedOfYggdrasil: 10, PhoenixFeather: 4]
              [Monument Saber: 10, PhoenixFeather: 7, SerpentJewel: 8]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 10]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 10, SeedOfYggdrasil: 5]
              [SecretGemOf Saber: 4, SeedOfYggdrasil: 10]
              [SecretGemOf Saber: 10, ProofOfHero: 12]
              [ProofOfHero: 24, ForbiddenPage: 5]
              [ForbiddenPage: 15, SerpentJewel: 16]
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
  , skills:   [ { name:   "Golden Rule"
                , rank:   CMinus
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 15.0 ~ 37.5 ]
                }
              , { name:   "Disengage"
                , rank:   A
                , icon:   IconBubbles
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              , { name:   "Dragon-Slayer"
                , rank:   A
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 3 (AttackVs Dragon) $ 50.0 ~ 80.0
                          , Grant Self 3 (DefenseVs Dragon) $ Flat 30.0
                          ]
                }
              ]
  , passives: [riding B]
  , phantasm: { name:  "Balmung"
              , desc:   "Illusory Greatsword: Felling of the Sky Demon"
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
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 10, DragonFang: 15]
              [Monument Saber: 4, SeedOfYggdrasil: 10, ProofOfHero: 12]
              [Monument Saber: 10, ProofOfHero: 24, Dragon'sReverseScale: 4]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 10]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 10, SeedOfYggdrasil: 5]
              [SecretGemOf Saber: 4, SeedOfYggdrasil: 10]
              [SecretGemOf Saber: 10, DragonFang: 10]
              [DragonFang: 20, HeartOfTheForeignGod: 2]
              [HeartOfTheForeignGod: 6, Dragon'sReverseScale: 8]
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
  , skills:   [ { name:   "Numeral of The Saint"
                , rank:   EX
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ Flat 20.0
                          , When "on Open Field with Sunshine" <<<
                            Grant Self 3 (Performance Buster) $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Charisma"
                , rank:   E
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 6.0 ~ 12.0 ]
                }
              , { name:   "Belt of Bertilak"
                , rank:   EX
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
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 10, OctupletCrystals: 6]
              [Monument Saber: 4, Dragon'sReverseScale: 4, GreatKnightMedal: 8]
              [Monument Saber: 10, GreatKnightMedal: 16, ProofOfHero: 30]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 10]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 10, Dragon'sReverseScale: 2]
              [SecretGemOf Saber: 4, Dragon'sReverseScale: 4]
              [SecretGemOf Saber: 10, OctupletCrystals: 4]
              [OctupletCrystals: 8, DragonFang: 12]
              [DragonFang: 36, SpiritRoot: 8]

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
  , skills:   [ { name:   "Intuition"
                , rank:   B
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Mana Burst"
                , rank:   A
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Journey of the Flowers"
                , rank:   EX
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Party 3 NPGen $ 10.0 ~ 20.0 ]
                }
              ]
  , passives: [magicResistance B, riding C]
  , phantasm: { name:   "Caliburn"
              , desc:   "Golden Sword of Assured Victory"
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
  , align:    [Lawful, Good]
  , limited:  true
  , free:     true
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 10, ProofOfHero: 18]
              [Monument Saber: 4, DragonFang: 20, SeedOfYggdrasil: 5]
              [Monument Saber: 10, SeedOfYggdrasil: 10, Dragon'sReverseScale: 4]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 10]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 10, DragonFang: 10]
              [SecretGemOf Saber: 4, DragonFang: 20]
              [SecretGemOf Saber: 10, ProofOfHero: 12]
              [ProofOfHero: 24, ClawOfChaos: 3]
              [ClawOfChaos: 9, Dragon'sReverseScale: 8]
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
  , skills:   [ { name:   "Tactics"
                , rank:   B
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Charisma"
                , rank:   C
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 8.0 ~ 16.0 ]
                }
              , { name:   "Incite"
                , rank:   EX
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Ally 3 CritUp $ 20.0 ~ 40.0
                          , Debuff Ally 3 DefenseDown $ Flat 20.0
                          ]
                }
              ]
  , passives: [magicResistance C, riding B, divinity D]
  , phantasm: { name:   "Crocea Mors"
              , desc:   "The Yellow Death"
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
  , align:    [Neutral, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 8, ProofOfHero: 15]
              [Monument Saber: 4, EternalGear: 7, ForbiddenPage: 4]
              [Monument Saber: 8, ForbiddenPage: 7, ClawOfChaos: 5]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 8]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 8, EternalGear: 4]
              [SecretGemOf Saber: 4, EternalGear: 7]
              [SecretGemOf Saber: 8, ProofOfHero: 10]
              [ProofOfHero: 20, MeteorHorseshoe: 4]
              [MeteorHorseshoe: 12, ClawOfChaos: 10]
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
  , skills:   [ { name:   "Valor"
                , rank:   A
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 10.0 ~ 30.0
                          , Grant Self 3 MentalResist $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Defiant"
                , rank:   B
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 15.0 ~ 25.0
                          , Debuff Self 3 BuffFail $ Flat 50.0
                          ]
                }
              , { name:   "Mind's Eye (True)"
                , rank:   A
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 DefenseUp $ 10.0 ~ 20.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding B]
  , phantasm: { name:   "Caladbolg"
              , desc:   "Rainbow Blade"
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
  , align:    [Lawful, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 8, ProofOfHero: 15]
              [Monument Saber: 4, OctupletCrystals: 7, HeartOfTheForeignGod: 2]
              [Monument Saber: 8, HeartOfTheForeignGod: 3, MeteorHorseshoe: 8]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 8]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 8, OctupletCrystals: 4]
              [SecretGemOf Saber: 4, OctupletCrystals: 7]
              [SecretGemOf Saber: 8, ProofOfHero: 10]
              [ProofOfHero: 20, SeedOfYggdrasil: 5]
              [SeedOfYggdrasil: 15, MeteorHorseshoe: 16]
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
  , skills:   [ { name:   "Tactics"
                , rank:   C
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 8.0 ~ 16.0 ]
                }
              , { name:   "Calm and Collected"
                , rank:   B
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ Flat 30.0
                          , Grant Self 3 DebuffResist $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Oath of Protection"
                , rank:   B
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Party 1 DefenseUp $ Flat 30.0
                          , Grant Self 1 DebuffResist $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding A]
  , phantasm: { name:   "Switch On—Airgetlám"
              , desc:   "Be my Sword, Silver Arm"
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
  , align:    [Lawful, Good]
  , limited:  false
  , free:     true
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 8, ProofOfHero: 15]
              [Monument Saber: 4, EternalGear: 4, Void'sDust: 13]
              [Monument Saber: 8, EternalGear: 7, GreatKnightMedal: 16]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 8]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 8, Void'sDust: 7]
              [SecretGemOf Saber: 4, Void'sDust: 13]
              [SecretGemOf Saber: 8, ProofOfHero: 10]
              [Fool'sChain: 12, ProofOfHero: 20]
              [Fool'sChain: 36, TearstoneOfBlood: 10]
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
  , skills:   [ { name:   "Tactics"
                , rank:   C
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 8.0 ~ 16.0 ]
                }
              , { name:   "Golden Rule"
                , rank:   B
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 18.0 ~ 45.0 ]
                }
              , { name:   "Prelati's Encouragement"
                , rank:   B
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
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Saber: 4]
              [Piece Saber: 8, ProofOfHero: 15]
              [Monument Saber: 4, Void'sDust: 13, EvilBone: 10]
              [Monument Saber: 8, EvilBone: 20, HeartOfTheForeignGod: 4]
  , skillUp:  Reinforcement
              [GemOf Saber: 4]
              [GemOf Saber: 8]
              [MagicGemOf Saber: 4]
              [MagicGemOf Saber: 8, Void'sDust: 7]
              [SecretGemOf Saber: 4, Void'sDust: 13]
              [SecretGemOf Saber: 8, ProofOfHero: 10]
              [ProofOfHero: 20, GhostLantern: 4]
              [GhostLantern: 12, HeartOfTheForeignGod: 7]
  }
]
