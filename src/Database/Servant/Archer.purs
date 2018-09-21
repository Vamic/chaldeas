module Database.Servant.Archer (archers) where

import StandardLibrary
import Database.Model

archers :: Array Servant
archers = Servant <$>
[ { name:     "Gilgamesh"
  , id:       12
  , rarity:   5
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    10
  , stats:    { base:  { atk: 1897,  hp: 1920 }
              , max:   { atk: 12280, hp: 13097 }
              , grail: { atk: 13442, hp: 14348 }
              }
  , skills:   [ { name:   "Charisma"
                , rank:   APlus
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.5 ~ 21.0 ]
                }
              , { name:   "Golden Rule"
                , rank:   A
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 20.0 ~ 50.0 ]
                }
              , { name:   "Collector"
                , rank:   EX
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
              , over:   [ To Enemies (DamageVs EnumaElish) $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 153, starRate: 7.9, npAtk: 0.34, npDef: 3 }
  , hits:     { quick: 5, arts: 5, buster: 5, ex: 8 }
  , traits:   [Male, Divine, EnumaElish, King]
  , death:    31.5
  , align:    [Chaotic, Good]
  , limited:  true
  , free:     false
  , ascendUp: Ascension 
              [Piece Archer: 5]
              [Piece Archer: 12, SerpentJewel: 6]
              [Monument Archer: 5, ProofOfHero: 29, Dragon'sReverseScale: 2]
              [ Monument Archer: 12, Dragon'sReverseScale: 4
              , HeartOfTheForeignGod: 5 
              ]
  , skillUp:  Reinforcement
              [GemOf Archer: 5]
              [GemOf Archer: 12]
              [MagicGemOf Archer: 5]
              [MagicGemOf Archer: 12, ProofOfHero: 15]
              [SecretGemOf Archer: 5, ProofOfHero: 29]
              [SecretGemOf Archer: 12, SerpentJewel: 4]
              [SerpentJewel: 8, Void'sDust: 12]
              [Void'sDust: 36, HeartOfTheForeignGod: 10]
  }
, { name:     "Altria Pendragon (Archer)"
  , id:       129
  , rarity:   5
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    5 --TODO
  , stats:    { base:  { atk: 1742,  hp: 2134 }
              , max:   { atk: 11276, hp: 14553 }
              , grail: { atk: 12343, hp: 15943 }
              }
  , skills:   [ { name:   "Summer Splash!"
                , rank:   APlus
                , icon:   IconArtsUp
                , cd:     8
                , effect: [ Grant Self 3 (Performance Arts)  $ 20.0 ~ 30.0
                          , Grant Party 3 DefenseUp $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Beach House Protection"
                , rank:   EX
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Self Heal $ 2000.0 ~ 5000.0
                          , To Self DemeritGauge $ Flat 10.0
                          ]
                }
              , { name:   "Beach Flower"
                , rank:   B
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
                        , Chance 70 <<< To Enemy GaugeDown $ Flat 1.0
                        ]
              , over:   [ To Self GaugeUp $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 153, starRate: 8.0, npAtk: 0.59, npDef: 3 }
  , hits:     { quick: 4, arts: 3, buster: 3, ex: 5 }
  , traits:   [Female, EnumaElish, Arthur, Dragon, King, Saberface ]
  , death:    25.8
  , align:    [Lawful, Good]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 5]
              [Piece Archer: 12, ShellOfReminiscence: 8]
              [Monument Archer: 5, DragonFang: 24, GreatKnightMedal: 10]
              [Dragon'sReverseScale: 5]
  , skillUp:  Reinforcement
              [GemOf Archer: 5]
              [GemOf Archer: 12]
              [MagicGemOf Archer: 5]
              [MagicGemOf Archer: 12, DragonFang: 12]
              [SecretGemOf Archer: 5, DragonFang: 24]
              [SecretGemOf Archer: 12, ShellOfReminiscence: 5]
              [ShellOfReminiscence: 10, Warhorse'sYoungHorn: 4]
              [Warhorse'sYoungHorn: 11, SpiritRoot: 10]
  }
, { name:     "Nikola Tesla"
  , id:       77
  , rarity:   5
  , class:    Archer
  , attr:     Star
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    10
  , stats:    { base:  { atk: 1820,  hp: 2027 }
              , max:   { atk: 11781, hp: 13825 }
              , grail: { atk: 12896, hp: 15146 }
              }
  , skills:   [ { name:   "Galvanism"
                , rank:   A
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Self 3 NPGen $ 30.0 ~ 50.0 ]
                }
              , { name:   "Inherent Wisdom"
                , rank:   A
                , icon:   IconKneel
                , cd:     7
                , effect: [ Times 1 <<< Grant Self 3 Guts $ 1000.0 ~ 3000.0
                          , Chance 80 <<< Grant Self 3 DefenseUp $ 20.0 ~ 30.0
                          , Chance 80 <<< Grant Self 1 NPUp $ 20.0 ~ 30.0
                          ]
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
              , over:   [ To Enemies (DamageVs HeavenOrEarth) $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 147, starRate: 7.9, npAtk: 0.87, npDef: 3 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Brynhild]
  , death:    31.5
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 5]
              [Piece Archer: 12, Void'sDust: 15]
              [Monument Archer: 5, ForbiddenPage: 10, EternalGear: 5]
              [Monument Archer: 12, EternalGear: 10, GhostLantern: 12]
  , skillUp:  Reinforcement
              [GemOf Archer: 5]
              [GemOf Archer: 12]
              [MagicGemOf Archer: 5]
              [MagicGemOf Archer: 12, ForbiddenPage: 5]
              [SecretGemOf Archer: 5, ForbiddenPage: 10]
              [SecretGemOf Archer: 12, Void'sDust: 10]
              [Void'sDust: 20, PhoenixFeather: 5]
              [PhoenixFeather: 15, GhostLantern: 24]

  }
, { name:     "Orion"
  , id:       60
  , rarity:   5
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    25
  , stats:    { base:  { atk: 1716,  hp: 2134 }
              , max:   { atk: 11107, hp: 14553 }
              , grail: { atk: 12158, hp: 15943 }
              }
  , skills:   [ { name:   "Grace of the Goddess"
                , rank:   EX
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 1 DefenseUp $ 30.0 ~ 50.0
                          , Grant Self 3 AttackUp $ Flat 20.0
                          , Grant Self 3 DebuffResist $ Flat 50.0
                          ]
                }
              , { name:   "Punish the Unfaithful"
                , rank:   APlus
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 1 (AttackVs Male) $ 50.0 ~ 100.0 ]
                }
              , { name:   "Mind's Eye (Fake)"
                , rank:   BMinus
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
  , hits:     { quick: 3, arts: 1, buster: 1, ex: 4 }
  , traits:   [Male, GreekMythMale, EnumaElish]
  , death:    27.0
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 5]
              [Piece Archer: 12, HeartOfTheForeignGod: 3]
              [Monument Archer: 5, SerpentJewel: 8, ClawOfChaos: 3]
              [Monument Archer: 12, ClawOfChaos: 6, PhoenixFeather: 10]
  , skillUp:  Reinforcement
              [GemOf Archer: 5]
              [GemOf Archer: 12]
              [MagicGemOf Archer: 5]
              [MagicGemOf Archer: 12, SerpentJewel: 4]
              [SecretGemOf Archer: 5, SerpentJewel: 8]
              [SecretGemOf Archer: 12, HeartOfTheForeignGod: 2]
              [HeartOfTheForeignGod: 4, Void'sDust: 12]
              [Void'sDust: 36, PhoenixFeather: 20]
  }
, { name:     "Arjuna"
  , id:       84
  , rarity:   5
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , curve:    25
  , stats:    { base:  { atk: 1907,  hp: 1940 }
              , max:   { atk: 12342, hp: 13230 }
              , grail: { atk: 13510, hp: 14494 }
              }
  , skills:   [ { name:   "Clairvoyance"
                , rank:   CPlus
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 StarUp $ 18.0 ~ 36.0 ]
                }
              , { name:   "Hero of the Endowed"
                , rank:   A
                , icon:   IconNoble
                , cd:     12
                , effect: [ To Self GaugeUp $ Flat 25.0
                          , Grant Self 5 HealPerTurn $ 1000.0 ~ 2000.0
                          , Grant Party 5 StarsPerTurn $ 4.0 ~ 8.0
                          ]
                }
              , { name:   "Mana Burst (Flame)"
                , rank:   A
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 20.0 ~ 30.0
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
  , hits:     { quick: 2, arts: 3, buster: 3, ex: 5 }
  , traits:   [Male, Divine, EnumaElish]
  , death:    31.5
  , align:    [Lawful, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 5]
              [Piece Archer: 12, ProofOfHero: 22]
              [Monument Archer: 5, PhoenixFeather: 8, SerpentJewel: 4]
              [Monument Archer: 12, SerpentJewel: 8, HeartOfTheForeignGod: 5]
  , skillUp:  Reinforcement
              [GemOf Archer: 5]
              [GemOf Archer: 12]
              [MagicGemOf Archer: 5]
              [MagicGemOf Archer: 12, PhoenixFeather: 4]
              [SecretGemOf Archer: 5, PhoenixFeather: 8]
              [SecretGemOf Archer: 12, ProofOfHero: 15]
              [ProofOfHero: 29, SeedOfYggdrasil: 8]
              [SeedOfYggdrasil: 22, HeartOfTheForeignGod: 10]
  }
, { name:     "Chloe von Einzbern"
  , id:       137
  , rarity:   4
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    9
  , stats:    { base:  { atk: 1640,  hp: 1746 }
              , max:   { atk: 9845, hp: 10914 }
              , grail: { atk: 11920, hp: 13239 }
              }
  , skills:   [ { name:   "Mind's Eye (Fake)"
                , rank:   B
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 18.0 ~ 36.0
                          ]
                }
              , { name:   "Projection"
                , rank:   B
                , icon:   IconAllUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Arts) $ 20.0 ~ 35.0
                          , Grant Self 1 (Performance Quick) $ 20.0 ~ 35.0
                          , Grant Self 1 (Performance Buster) $ 20.0 ~ 35.0
                          ]
                }
              , { name:   "Kissing Freak"
                , rank:   B
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
  , hits:     { quick: 3, arts: 6, buster: 2, ex: 4 }
  , traits:   [Female, PseudoServant, EnumaElish]
  , death:    36.0
  , align:    [Chaotic, Good]
  , limited:  true
  , free:     true
  , ascendUp: Welfare "Heart Bracelet"
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 10]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 10, EternalGear: 4]
              [SecretGemOf Archer: 4, EternalGear: 8]
              [SecretGemOf Archer: 10, HomunculusBaby: 4]
              [HomunculusBaby: 8, TearstoneOfBlood: 3]
              [TearstoneOfBlood: 9, HeartOfTheForeignGod: 8]
  }
, { name:     "Oda Nobunaga"
  , id:       69
  , rarity:   4
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    14
  , stats:    { base:  { atk: 1862,  hp: 1582 }
              , max:   { atk: 9494, hp: 11637 }
              , grail: { atk: 11495, hp: 14110 }
              }
  , skills:   [ { name:   "Strategy"
                , rank:   B
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Party 3 NPGen $ 20.0 ~ 30.0 ]
                }
              , { name:   "Unifying the Nation by Force"
                , rank:   A
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 1 (AttackVs Divine) $ 50.0 ~ 100.0 ]
                }
              , { name:   "The Demonic King"
                , rank:   A
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
              , over:   [ To Enemies (DamageVs Riding) $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 150, starRate: 7.9, npAtk: 0.43, npDef: 3 }
  , hits:     { quick: 2, arts: 4, buster: 4, ex: 5 }
  , traits:   [Female, EnumaElish, King]
  , death:    31.5
  , align:    [Lawful, Balanced]
  , limited:  true
  , free:     true
  , ascendUp: Welfare "Golden Skull"
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 10]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 10, EvilBone: 12]
              [SecretGemOf Archer: 4, EvilBone: 24]
              [SecretGemOf Archer: 10, OctupletCrystals: 4]
              [OctupletCrystals: 8, ClawOfChaos: 3]
              [ClawOfChaos: 9, HeartOfTheForeignGod: 8]
  }
, { name:     "Tristan"
  , id:       122
  , rarity:   4
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    14
  , stats:    { base:  { atk: 1622,  hp: 1862 }
              , max:   { atk: 9735, hp: 11637 }
              , grail: { atk: 11787, hp: 14110 }
              }
  , skills:   [ { name:   "Harp of Healing"
                , rank:   C
                , icon:   IconBubbles
                , cd:     8
                , effect: [ To Party RemoveMental Full
                          , Grant Party 0 Evasion Full
                          , To Party Heal $ 200.0 ~ 600.0
                          ]
                }
              , { name:   "Grace of the Unexpected Birth"
                , rank:   B
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0
                          , Debuff Self 1 SealNP Full
                          ]
                }
              , { name:   "Admonishment of the King of Knights"
                , rank:   Unknown
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
  , hits:     { quick: 4, arts: 3, buster: 5, ex: 6 }
  , traits:   [Male, EnumaElish]
  , death:    31.5
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 10, GreatKnightMedal: 12]
              [Monument Archer: 4, PhoenixFeather: 7, ForbiddenPage: 4]
              [Monument Archer: 10, ForbiddenPage: 8, TearstoneOfBlood: 6]
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 10]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 10, PhoenixFeather: 4]
              [SecretGemOf Archer: 4, PhoenixFeather: 7]
              [SecretGemOf Archer: 10, GreatKnightMedal: 8]
              [GreatKnightMedal: 16, GhostLantern: 5]
              [GhostLantern: 15, Fool'sChain: 60]
  }
, { name:     "EMIYA"
  , id:       11
  , rarity:   4
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , curve:    14
  , stats:    { base:  { atk: 1566,  hp: 1843 }
              , max:   { atk: 9398, hp: 11521 }
              , grail: { atk: 11379, hp: 13969 }
              }
  , skills:   [ { name:   "Mind's Eye (True)"
                , rank:   B
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 DefenseUp $ 9.0 ~ 18.0
                          ]
                }
              , { name:   "Clairvoyance"
                , rank:   C
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 StarUp $ 16.0 ~ 32.0 ]
                }
              , { name:   "Projection"
                , rank:   A
                , icon:   IconAllUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Arts) $ 25.0 ~ 40.0
                          , Grant Self 1 (Performance Quick) $ 25.0 ~ 40.0
                          , Grant Self 1 (Performance Buster) $ 25.0 ~ 40.0
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
  , hits:     { quick: 2, arts: 3, buster: 1, ex: 5 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    31.5
  , align:    [Neutral, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 10, ProofOfHero: 18]
              [Monument Archer: 4, HeartOfTheForeignGod: 2, EternalGear: 8]
              [Monument Archer: 10, HeartOfTheForeignGod: 4, Void'sDust: 20]
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 10]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 10, EternalGear: 4]
              [SecretGemOf Archer: 4, EternalGear: 8]
              [SecretGemOf Archer: 10, ProofOfHero: 12]
              [PhoenixFeather: 4, ProofOfHero: 24]
              [PhoenixFeather: 12, Void'sDust: 40]
  }
, { name:     "Atalante"
  , id:       14
  , rarity:   4
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    9
  , stats:    { base:  { atk: 1438,  hp: 1996 }
              , max:   { atk: 8633, hp: 12476 }
              , grail: { atk: 10453, hp: 15127 }
              }
  , skills:   [ { name:   "Beyond Arcadia"
                , rank:   A
                , icon:   IconQuickUp
                , cd:     7
                , effect: [ Grant Party 1 (Performance Quick) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Hunter's Aesthetic"
                , rank:   C
                , icon:   IconStarUp
                , cd:     6
                , effect: [ Grant Self 1 StarAbsorb $ 500.0 ~ 1000.0 ]
                }
              , { name:   "Calydonian Hunt"
                , rank:   A
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
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    31.5
  , align:    [Neutral, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 10, PhoenixFeather: 5]
              [Monument Archer: 4, DragonFang: 32]
              [Monument Archer: 10, SerpentJewel: 7, SeedOfYggdrasil: 12]
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 10]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 10, DragonFang: 10]
              [SecretGemOf Archer: 4, DragonFang: 20]
              [SecretGemOf Archer: 10, PhoenixFeather: 4]
              [PhoenixFeather: 7, Void'sDust: 10]
              [Void'sDust: 30, SeedOfYggdrasil: 24]
  }
, { name:     "Anne Bonny & Mary Read (Archer)"
  , id:       131
  , rarity:   4
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    29
  , stats:    { base:  { atk: 1574,  hp: 1843 }
              , max:   { atk: 9446, hp: 11521 }
              , grail: { atk: 11437, hp: 13969 }
              }
  , skills:   [ { name:   "Beach Flower"
                , rank:   APlus
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.5 ~ 19.5
                          , Grant (AlliesType Male) 3 StarUp $ 21.0 ~ 41.0
                          ]
                }
              , { name:   "Treasure Hunt (Sea)"
                , rank:   C
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 1 StarAbsorb $ 300.0 ~ 600.0
                          , To Party GainStars $ 5.0 ~ 15.0
                          ]
                }
              , { name:   "Pirate's Glory"
                , rank:   CPlus
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 8.5 ~ 25.5
                          , Times 1 <<< Grant Self 0 Guts $ Flat 1.0
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
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    40.5
  , align:    [Chaotic, Balanced]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 10, SerpentJewel: 5]
              [Monument Archer: 4, OctupletCrystals: 8, ShellOfReminiscence: 4]
              [Monument Archer: 10, ShellOfReminiscence: 8, ClawOfChaos: 6] 
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 10]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 10, OctupletCrystals: 4]
              [SecretGemOf Archer: 4, OctupletCrystals: 8]
              [SecretGemOf Archer: 10, SerpentJewel: 4]
              [SerpentJewel: 7, Fool'sChain: 15]
              [Fool'sChain: 45, BlackBeastGrease: 12]
  }
, { name:     "Robin Hood"
  , id:       13
  , rarity:   3
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    3
  , stats:    { base:  { atk: 1247,  hp: 1833 }
              , max:   { atk: 6715, hp: 10187 }
              , grail: { atk: 9088, hp: 13812 }
              }
  , skills:   [ { name:   "Sabotage"
                , rank:   A
                , icon:   IconSwordDown
                , cd:     7
                , effect: [ Debuff Enemies 3 AttackDown $ 5.0 ~ 15.0
                          , Debuff Enemies 5 Poison $ Flat 500.0
                          ]
                }
              , { name:   "Golden Rule"
                , rank:   E
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 12.0 ~ 30.0 ]
                }
              , { name:   "May King"
                , rank:   B
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
              , over:   [ To Enemy DamagePoison $ 200.0 ~ 250.0 ]
              , first:  false
              }
  , gen:      { starWeight: 150, starRate: 8.0, npAtk: 0.87, npDef: 3 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    31.5
  , align:    [Neutral, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 8, SeedOfYggdrasil: 6]
              [Monument Archer: 4, SerpentJewel: 6, DragonFang: 8]
              [Monument Archer: 8, DragonFang: 16, Void'sDust: 16]
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 8]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 8, SerpentJewel: 3]
              [SecretGemOf Archer: 4, SerpentJewel: 6]
              [SecretGemOf Archer: 8, SeedOfYggdrasil: 4]
              [SeedOfYggdrasil: 8, PhoenixFeather: 4]
              [PhoenixFeather: 10, Void'sDust: 32]
  }
, { name:     "Euryale"
  , id:       15
  , rarity:   3
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    3
  , stats:    { base:  { atk: 1306,  hp: 1711 }
              , max:   { atk: 7032, hp: 9506 }
              , grail: { atk: 9517, hp: 12889 }
              }
  , skills:   [ { name:   "Vampirism"
                , rank:   C
                , icon:   IconDarkMagic
                , cd:     8
                , effect: [ Chances 60 80 <<< To Enemy GaugeDown $ Flat 1.0
                          , To Self GaugeUp $ 18.0 ~ 27.0
                          ]
                }
              , { name:   "Siren Song"
                , rank:   A
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 70 100 $
                            Debuff (EnemyType Male) 1 Charm Full ]
                }
              , { name:   "Whim of the Goddess"
                , rank:   A
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Self 3 (Performance Arts) $ 20.0 ~ 30.0 ]
                }
              ]
  , passives: [magicResistance A, independentAction APlus, coreOfGoddess EX]
  , phantasm: { name:   "Eye of the Euryale"
              , desc:   "Gaze of the Goddess"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage $ Flat 1200.0
                        , To Enemy (DamageVs Male) $ 150.0 ~ 250.0
                        , Chance 150 <<< Debuff Enemy 3 AttackDown $ Flat 20.0
                        ]
              , over:   [ Chances 100 200 $
                          Debuff (EnemyType Male) 1 Charm Full ]
              , first:  false
              }
  , gen:      { starWeight: 156, starRate: 7.9, npAtk: 0.9, npDef: 3 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    22.5
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 8, SerpentJewel: 4]
              [Monument Archer: 4, Dragon'sReverseScale: 3, Void'sDust: 7]
              [Monument Archer: 8, Void'sDust: 13, HeartOfTheForeignGod: 4]
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 8]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 8, Dragon'sReverseScale: 2]
              [SecretGemOf Archer: 4, Dragon'sReverseScale: 3]
              [SecretGemOf Archer: 8, SerpentJewel: 3]
              [SerpentJewel: 6, ClawOfChaos: 3]
              [ClawOfChaos: 8, HeartOfTheForeignGod: 7]
  }
, { name:     "Arash"
  , id:       16
  , rarity:   1
  , class:    Archer
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    1
  , stats:    { base:  { atk: 1057, hp: 1424 }
              , max:   { atk: 5816, hp: 7122 }
              , grail: { atk: 9037, hp: 10979 }
              }
  , skills:   [ { name:   "Toughness"
                , rank:   EX
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 10.0 ~ 20.0
                          , Grant Self 3 (Resist Poison) $ 80.0 ~ 160.0
                          ]
                }
              , { name:   "Clairvoyance"
                , rank:   A
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 StarUp $ 20.0 ~ 40.0 ]
                }
              , { name:   "Arrow Construction"
                , rank:   A
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
              , hits:   1
              , effect: [ To Enemies Damage $ 800.0 ~ 1200.0
                        , To Self DemeritKill Full
                        ]
              , over:   [ To Enemies Damage $ 0.0 ~ 800.0 ]
              , first:  false
              }
  , gen:      { starWeight: 147, starRate: 8.0, npAtk: 0.84, npDef: 3 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    45.0
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 2]
              [Piece Archer: 4, SeedOfYggdrasil: 3]
              [Monument Archer: 2, ProofOfHero: 10, OctupletCrystals: 2]
              [Monument Archer: 4, OctupletCrystals: 4, Void'sDust: 8]
  , skillUp:  Reinforcement 
              [GemOf Archer: 2]
              [GemOf Archer: 4]
              [MagicGemOf Archer: 2]
              [MagicGemOf Archer: 4, ProofOfHero: 5]
              [SecretGemOf Archer: 2, ProofOfHero: 10]
              [SecretGemOf Archer: 4, SeedOfYggdrasil: 2]
              [SeedOfYggdrasil: 4, HomunculusBaby: 2]
              [HomunculusBaby: 2, Void'sDust: 16]
  }
, { name:     "David"
  , id:       63
  , rarity:   3
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Arts Buster
  , curve:    23
  , stats:    { base:  { atk: 1436,  hp: 1555 }
              , max:   { atk: 7736,  hp: 8643 }
              , grail: { atk: 10470, hp: 11719 }
              }
  , skills:   [ { name:   "Divine Protection"
                , rank:   A
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 1 DefenseUp $ Flat 50.0
                          , To Self Heal $ 1000.0 ~ 2000.0
                          ]
                }
              , { name:   "Harp of Healing"
                , rank:   B
                , icon:   IconBubbles
                , cd:     8
                , effect: [ To Party RemoveMental Full
                          , Times 1 $ Grant Party 0 Evasion Full
                          , To Party Heal $ 300.0 ~ 800.0
                          ]
                }
              , { name:   "Charisma"
                , rank:   B
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
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, King]
  , death:    36.0
  , align:    [Lawful, Balanced]
  , limited:  false
  , free:     true
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 8, Void'sDust: 10]
              [Monument Archer: 4, HomunculusBaby: 7, HeartOfTheForeignGod: 2]
              [Monument Archer: 8, HeartOfTheForeignGod: 3, OctupletCrystals: 8]
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 8]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 8, HomunculusBaby: 4]
              [SecretGemOf Archer: 4, HomunculusBaby: 7]
              [SecretGemOf Archer: 8, Void'sDust: 7]
              [Void'sDust: 13, ProofOfHero: 12]
              [ProofOfHero: 36, OctupletCrystals: 16]
  }
, { name:     "Gilgamesh (Child)"
  , id:       95
  , rarity:   3
  , class:    Archer
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    8
  , stats:    { base:  { atk: 1429,  hp: 1571 }
              , max:   { atk: 7696, hp: 8731 }
              , grail: { atk: 10415, hp: 11838 }
              }
  , skills:   [ { name:   "Charisma"
                , rank:   APlus
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.5 ~ 21.0 ]
                }
              , { name:   "Fair Youth"
                , rank:   C
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 45 75 $
                            Debuff (EnemyType Humanoid) 1 Charm Full ]
                }
              , { name:   "Golden Rule"
                , rank:   A
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
  , hits:     { quick: 3, arts: 3, buster: 3, ex: 3 }
  , traits:   [Male, Divine, EnumaElish, King]
  , death:    36.0
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 8, SeedOfYggdrasil: 6]
              [Monument Archer: 4, SerpentJewel: 6, Dragon'sReverseScale: 2]
              [Monument Archer: 8, Dragon'sReverseScale: 3, ProofOfHero: 24]
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 8]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 8, SerpentJewel: 3]
              [SecretGemOf Archer: 4, SerpentJewel: 6]
              [SecretGemOf Archer: 8, SeedOfYggdrasil: 4]
              [SeedOfYggdrasil: 8, Void'sDust: 8]
              [Void'sDust: 24, PhoenixFeather: 13]
  }
, { name:     "Billy the Kid"
  , id:       105
  , rarity:   3
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    3
  , stats:    { base:  { atk: 1279,  hp: 1711 }
              , max:   { atk: 6890, hp: 9506 }
              , grail: { atk: 9325, hp: 12889 }
              }
  , skills:   [ { name:   "Marksmanship"
                , rank:   APlusPlus
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 1 CritUp $ 60.0 ~ 120.0 ]
                }
              , { name:   "Quick Draw"
                , rank:   APlus
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0 ]
                }
              , { name:   "Mind's Eye (Fake)"
                , rank:   C
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
  , hits:     { quick: 2, arts: 3, buster: 4, ex: 4 }
  , traits:   [Male, Riding, EnumaElish]
  , death:    45.0
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 8, MeteorHorseshoe: 5]
              [Monument Archer: 4, EvilBone: 20, PhoenixFeather: 3]
              [Monument Archer: 8, PhoenixFeather: 6, ClawOfChaos: 5]
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 8]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 8, EvilBone: 10]
              [SecretGemOf Archer: 4, EvilBone: 20]
              [SecretGemOf Archer: 8, MeteorHorseshoe: 4]
              [MeteorHorseshoe: 7, Void'sDust: 8]
              [Void'sDust: 24, TearstoneOfBlood: 10]
  }
, { name:     "Tawara Touta"
  , id:       125
  , rarity:   3
  , class:    Archer
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    28
  , stats:    { base:  { atk: 1306,  hp: 1764 }
              , max:   { atk: 7032, hp: 9800 }
              , grail: { atk: 9517, hp: 13287 }
              }
  , skills:   [ { name:   "Protection of the Dragon King"
                , rank:   C
                , icon:   IconBusterUp
                , cd:     8
                , effect: [ Grant Self 3 (Performance Buster) $ 20.0 ~ 30.0
                          , To Self Heal $ 1000.0 ~ 2000.0
                          ]
                }
              , { name:   "Protection from Arrows"
                , rank:   C
                , icon:   IconDodge
                , cd:     7
                , effect: [ Times 2 $ Grant Self 0 Evasion Full
                          , Grant Self 3 DefenseUp $ 7.0 ~ 14.0
                          ]
                }
              , { name:   "Inexhaustible Straw Bag"
                , rank:   EX
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
              , over:   [ Grant Self 1 (AttackVs Demonic) $ 50.0 ~ 100.0 ]
              , first:  true
              }
  , gen:      { starWeight: 150, starRate: 7.8, npAtk: 0.57, npDef: 3 }
  , hits:     { quick: 2, arts: 3, buster: 1, ex: 5 }
  , traits:   [Male, Brynhild, EnumaElish]
  , death:    36.0
  , align:    [Neutral, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Archer: 4]
              [Piece Archer: 10, SeedOfYggdrasil: 6]
              [Monument Archer: 4, PhoenixFeather: 3, ProofOfHero: 20]
              [Monument Archer: 10, SpiritRoot: 4, PhoenixFeather: 5]
  , skillUp:  Reinforcement
              [GemOf Archer: 4]
              [GemOf Archer: 8]
              [MagicGemOf Archer: 4]
              [MagicGemOf Archer: 8, ProofOfHero: 10]
              [SecretGemOf Archer: 4, ProofOfHero: 20]
              [SecretGemOf Archer: 8, SeedOfYggdrasil: 4]
              [SeedOfYggdrasil: 8, OctupletCrystals: 4]
              [OctupletCrystals: 12, ScarabOfWisdom: 7]

  }
]
