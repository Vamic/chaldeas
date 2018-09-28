module Database.Servant.Assassin (assassins) where

import StandardLibrary
import Database.Model

assassins :: Array Servant
assassins = Servant <$>
[ { name:     "Jack the Ripper"
  , id:       75
  , rarity:   5
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    25
  , stats:    { base:  { atk: 1786,  hp: 1862 }
              , max:   { atk: 11557, hp: 12696 }
              , grail: { atk: 12651, hp: 13909 }
              }
  , skills:   [ { name:   "Murder on a Misty Night"
                , rank:   A
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 1 (Performance Quick) $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Information Erasure"
                , rank:   B
                , icon:   IconCircuits
                , cd:     7
                , effect: [ To Enemy RemoveBuffs Full
                          , Debuff Enemy 3 CritChance $ 10.0 ~ 30.0
                          ]
                }
              , { name:   "Surgery"
                , rank:   E
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Ally Heal $ 500.0 ~ 2500.0 ]
                }
              ]
  , passives: [presenceConcealment APlus]
  , phantasm: { name:   "Maria the Ripper"
              , desc:   "Holy Mother of Dismemberment"
              , rank:   DPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   4
              , effect: [ To Enemy DamageThruDef $ 1400.0 ~ 2200.0 ]
              , over:   [ Grant Self 1 (AttackVs Female) $ 50.0 ~ 100.0 ]
              , first:  true
              }
  , gen:      { starWeight: 97, starRate: 25.5, npAtk: 1.07, npDef: 4 }
  , hits:     { quick: 5, arts: 2, buster: 2, ex: 4 }
  , traits:   [Female, EnumaElish]
  , death:    44.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 5]
              [Piece Assassin: 12, EvilBone: 22]
              [Monument Assassin: 5, EternalGear: 5, Void'sDust: 20]
              [Monument Assassin: 12, ClawOfChaos: 8, EternalGear: 10]
  , skillUp:  Reinforcement
              [GemOf Assassin: 5]
              [GemOf Assassin: 12]
              [MagicGemOf Assassin: 5]
              [MagicGemOf Assassin: 12, Void'sDust: 10]
              [SecretGemOf Assassin: 5, Void'sDust: 20]
              [SecretGemOf Assassin: 12, EvilBone: 15]
              [HeartOfTheForeignGod: 3, EvilBone: 29]
              [HeartOfTheForeignGod: 8, ClawOfChaos: 15]
  }
, { name:     "Shuten-Douji"
  , id:       112
  , rarity:   5
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    25
  , stats:    { base:  { atk: 1853,  hp: 1881 }
              , max:   { atk: 11993, hp: 12825 }
              , grail: { atk: 13128, hp: 14050 }
              }
  , skills:   [ { name:   "Intoxicating Aroma of Fruits"
                , rank:   A
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chance 60 $ Debuff Enemies 1 Charm Full
                          , Debuff Enemies 3 DefenseDown $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Demonic Nature of Oni"
                , rank:   A
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.0 ~ 20.0
                          , Grant Self 3 NPUp $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Battle Continuation"
                , rank:   APlus
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1200.0 ~ 2700.0 ]
                }
              ]
  , passives: [presenceConcealment C, divinity C]
  , phantasm: { name:   "Multicolored Poison—Shinpen Kidoku"
              , desc:   ""
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage $ 450.0 ~ 750.0
                        , Debuff Enemies 3 DebuffVuln $ Flat 10.0
                        , Debuff Enemies 3 AttackDown $ Flat 10.0
                        , Debuff Enemies 3 NPDown $ Flat 10.0
                        , Debuff Enemies 3 CritChance $ Flat 10.0
                        , Debuff Enemies 3 DefenseDown $ Flat 10.0
                        , Debuff Enemies 1 SealSkills Full
                        ]
              , over:   [ Debuff Enemies 5 Poison $ 1000.0 ~ 5000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 25.0, npAtk: 0.55, npDef: 4 }
  , hits:     { quick: 4, arts: 3, buster: 1, ex: 6 }
  , traits:   [Female, Demonic, EnumaElish, Divine, Dragon]
  , death:    31.6
  , align:    [Chaotic, Evil]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 5]
              [Piece Assassin: 12, EvilBone: 22]
              [Monument Assassin: 5, SerpentJewel: 8, GhostLantern: 5]
              [Monument Assassin: 12, GhostLantern: 10, HeartOfTheForeignGod: 5]
  , skillUp:  Reinforcement
              [GemOf Assassin: 5]
              [GemOf Assassin: 12]
              [MagicGemOf Assassin: 5]
              [MagicGemOf Assassin: 12, SerpentJewel: 4]
              [SecretGemOf Assassin: 5, SerpentJewel: 8]
              [SecretGemOf Assassin: 12, EvilBone: 15]
              [EvilBone: 29, ClawOfChaos: 4]
              [ClawOfChaos: 11, SpiritRoot: 10]
  }
, { name:     "Mysterious Heroine X"
  , id:       86
  , rarity:   5
  , class:    Assassin
  , attr:     Star
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    5
  , stats:    { base:  { atk: 1817,  hp: 1862 }
              , max:   { atk: 11761, hp: 12696 }
              , grail: { atk: 12874, hp: 13909 }
              }
  , skills:   [ { name:   "Fire Support"
                , rank:   EX
                , icon:   IconStun
                , cd:     10
                , effect: [ Chances 60 80 $ Debuff Enemies 1 StunBomb Full ]
                }
              , { name:   "Intuition"
                , rank:   CPlus
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Galactic Meteor Sword"
                , rank:   C
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 (ClassAffinity Saber) $ 30.0 ~ 50.0
                          , Grant Self 3 (StarAffinity Saber) $ 50.0 ~ 100.0
                          ]
                }
              ]
  , passives: [riding EX, cosmoReactor A]
  , phantasm: { name:   "Secret Calibur"
              , desc:   "Sword of Unnamed Victory"
              , rank:   EX
              , card:   Quick
              , kind:   "Anti-Unit"
              , hits:   12
              , effect: [ To Enemy Damage $ 1600.0 ~ 2400.0 ]
              , over:   [ To Enemy (DamageVs Saberface) $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 25.6, npAtk: 0.81, npDef: 4 }
  , hits:     { quick: 4, arts: 2, buster: 1, ex: 4 }
  , traits:   [Female, Riding, Dragon, Saberface, Arthur, King]
  , death:    38.5
  , align:    [Chaotic, Good]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 5]
              [Piece Assassin: 12, DragonFang: 18]
              [Monument Assassin: 5, Dragon'sReverseScale: 2, PhoenixFeather: 8]
              [Monument Assassin: 12, Dragon'sReverseScale: 4, ProofOfHero: 36]
  , skillUp:  Reinforcement
              [GemOf Assassin: 5]
              [GemOf Assassin: 12]
              [MagicGemOf Assassin: 5]
              [MagicGemOf Assassin: 12, PhoenixFeather: 4]
              [SecretGemOf Assassin: 5, PhoenixFeather: 8]
              [SecretGemOf Assassin: 12, DragonFang: 12]
              [Void'sDust: 12, DragonFang: 24]
              [Void'sDust: 36, OctupletCrystals: 24]
  }
, { name:     "Ryougi Shiki (Assassin)"
  , id:       92
  , rarity:   4
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    4
  , stats:    { base:  { atk: 1477,  hp: 1768 }
              , max:   { atk: 8867, hp: 11055 }
              , grail: { atk: 10736, hp: 13404 }
              }
  , skills:   [ { name:   "Mystic Eyes of Death Perception"
                , rank:   A
                , icon:   IconMystic
                , cd:     7
                , effect: [ Grant Self 1 IgnoreInvinc Full
                          , Grant Self 1 (Performance Arts) $ 30.0 ~ 50.0
                          , Debuff Enemy 1 DeathDown $ 80.0 ~ 100.0
                          ]
                }
              , { name:   "Mind's Eye (Fake)"
                , rank:   A
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Yin-Yang"
                , rank:   B
                , icon:   IconYinYang
                , cd:     8
                , effect: [ To Self GaugeUp $ 20.0 ~ 30.0
                          , To Self DemeritDamage $ Flat 1000.0
                          ]
                }
              ]
  , passives: [presenceConcealment C, independentAction A]
  , phantasm: { name:   "Vijñāpti-mātratā—Mystic Eyes of Death Perception"
              , desc:   "Yuishiki・Chokushi no Magan"
              , rank:   EX
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   3
              , effect: [ To Enemy DamageThruDef $ 900.0 ~ 1500.0 ]
              , over:   [ To Enemy Kill $ 100.0 ~ 140.0 ]
              , first:  true
              }
  , gen:      { starWeight: 102, starRate: 25.6, npAtk: 0.8, npDef: 4 }
  , hits:     { quick: 4, arts: 2, buster: 1, ex: 5 }
  , traits:   [Female, EnumaElish, PseudoServant]
  , death:    44.0
  , align:    [Chaotic, Good]
  , limited:  true
  , free:     true
  , ascendUp: Welfare "Sharp Knife"
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 10]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 10, EvilBone: 12]
              [SecretGemOf Assassin: 4, EvilBone: 24]
              [SecretGemOf Assassin: 10, ClawOfChaos: 3]
              [ClawOfChaos: 5, EternalGear: 5]
              [EternalGear: 15, HomunculusBaby: 20]
  }
, { name:     "Carmilla"
  , id:       46
  , rarity:   4
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    14
  , stats:    { base:  { atk: 1568,  hp: 1675 }
              , max:   { atk: 9408, hp: 10473 }
              , grail: { atk: 11391, hp: 12698 }
              }
  , skills:   [ { name:   "Vampirism"
                , rank:   C
                , icon:   IconDarkMagic
                , cd:     8
                , effect: [ Chances 60 80 <<< To Enemy GaugeDown $ Flat 1.0
                          , To Self GaugeUp $ 18.0 ~ 27.0
                          ]
                }
              , { name:   "Torture Technique"
                , rank:   A
                , icon:   IconShieldDown
                , cd:     7
                , effect: [ Debuff Enemy 3 DefenseDown $ 10.0 ~ 20.0 ]
                }
              , { name:   "Bath of Fresh Blood"
                , rank:   A
                , icon:   IconExclamationDown
                , cd:     8
                , effect: [ Debuff Enemy 3 CritChance $ 30.0 ~ 50.0
                          , Grant Party 3 StarsPerTurn $ 5.0 ~ 10.0
                          ]
                }
              ]
  , passives: [presenceConcealment D]
  , phantasm: { name:   "Phantom Maiden"
              , desc:   "Phantasmal Iron Maiden"
              , rank:   C
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0
                        , To Self Heal $ Flat 2000.0
                        , Grant Self 3 AttackUp $ Flat 20.0
                        ]
              , over:   [ To Enemy (DamageVs Female) $ 120.0 ~ 170.0 ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 25.2, npAtk: 2.15, npDef: 4 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    44.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 4]
              [Piece Assassin: 10, SerpentJewel: 5]
              [Monument Assassin: 4, HeartOfTheForeignGod: 2, HomunculusBaby: 8]
              [Monument Assassin: 10, HeartOfTheForeignGod: 4, ClawOfChaos: 6]
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 10]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 10, HomunculusBaby: 4]
              [SecretGemOf Assassin: 4, HomunculusBaby: 8]
              [SecretGemOf Assassin: 10, SerpentJewel: 4]
              [EvilBone: 15, SerpentJewel: 7]
              [EvilBone: 45, ClawOfChaos: 12]
  }
, { name:     "Emiya (Assassin)"
  , id:       109
  , rarity:   4
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    24
  , stats:    { base:  { atk: 1493,  hp: 1786 }
              , max:   { atk: 8958, hp: 11168 }
              , grail: { atk: 10846, hp: 13541 }
              }
  , skills:   [ { name:   "Magecraft"
                , rank:   B
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Arts) $ 24.0 ~ 40.0 ]
                }
              , { name:   "Affection of the Holy Grail"
                , rank:   APlus
                , icon:   IconShieldBreak
                , cd:     7
                , effect: [ Grant Self 3 IgnoreInvinc Full
                          , Grant Self 3 CritUp $ 30.0 ~ 50.0
                          , Debuff Others 3 DebuffVuln $ Flat 20.0
                          ]
                }
              , { name:   "Scapegoat"
                , rank:   C
                , icon:   IconCrosshairUp
                , cd:     7
                , effect: [ Grant Ally 1 Taunt Full
                          , To Party GainStars $ 5.0 ~ 15.0
                          ]
                }
              ]
  , passives: [presenceConcealment APlus, independentAction A]
  , phantasm: { name:   "Chronos Rose"
              , desc:   "Gather Ye Rosebuds While Ye May"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   15
              , effect: [ To Enemy Damage $ 900.0 ~ 1500.0
                        , To Enemy GaugeDown $ Flat 1.0
                        ]
              , over:   [ Debuff Enemy 3 CritChance $ 10.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 97, starRate: 25.6, npAtk: 0.46, npDef: 4 }
  , hits:     { quick: 4, arts: 2, buster: 6, ex: 8 }
  , traits:   [Male, Brynhild, EnumaElish]
  , death:    44.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 4]
              [Piece Assassin: 10, ProofOfHero: 18]
              [Monument Assassin: 4, EvilBone: 24, Void'sDust: 8]
              [Monument Assassin: 10, Void'sDust: 16, TearstoneOfBlood: 6]
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 10]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 10, EvilBone: 12]
              [SecretGemOf Assassin: 4, EvilBone: 24]
              [SecretGemOf Assassin: 10, ProofOfHero: 12]
              [ProofOfHero: 24, HomunculusBaby: 5]
              [HomunculusBaby: 15, HeartOfTheForeignGod: 8]
  }
, { name:     "Scathach (Assassin)"
  , id:       133
  , rarity:   4
  , class:    Assassin
  , attr:     Star
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    4
  , stats:    { base:  { atk: 1851,  hp: 1786 }
              , max:   { atk: 9049, hp: 11168 }
              , grail: { atk: 10956, hp: 13541 }
              }
  , skills:   [ { name:   "Beach Crisis"
                , rank:   APlus
                , icon:   IconCrosshairUp
                , cd:     8
                , effect: [ Chances 100 300 $ Grant Self 1 Taunt Full
                          , Grant Self 1 CritUp $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Primordial Rune (Sea)"
                , rank:   A
                , icon:   IconHeal
                , cd:     8
                , effect: [ To Ally Heal $ 1000.0 ~ 3000.0
                          , Grant Ally 1 DamageDown $ 500.0 ~ 1000.0
                          ]
                }
              , { name:   "Midsummer Mistake"
                , rank:   C
                , icon:   IconShieldBreak
                , cd:     8
                , effect: [ Grant Self 1 IgnoreInvinc Full
                          , Grant Self 1 (Performance Quick) $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [presenceConcealment E]
  , phantasm: { name:   "Gáe Bolg Alternative"
              , desc:   "Soaring Spear of Kicked Piercing Death"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage $ 600.0 ~ 1000.0 ]
              , over:   [ To Enemies Kill $ 30.0 ~ 70.0 ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 25.6, npAtk: 0.71, npDef: 4 }
  , hits:     { quick: 3, arts: 3, buster: 3, ex: 5 }
  , traits:   [Female, King, EnumaElish]
  , death:    44.0
  , align:    [Neutral, Good]
  , limited:  true
  , free:     true
  , ascendUp: Welfare "Bell-Ringing Branch"
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 10]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 10, SeedOfYggdrasil: 5]
              [SecretGemOf Assassin: 4, SeedOfYggdrasil: 10]
              [SecretGemOf Assassin: 10, ShellOfReminiscence: 4]
              [ShellOfReminiscence: 8, SpiritRoot: 2]
              [SpiritRoot: 6, ScarabOfWisdom: 8]
  }
, { name:     "Stheno"
  , id:       41
  , rarity:   4
  , class:    Assassin
  , attr:     Heaven
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    4
  , stats:    { base:  { atk: 1497,  hp: 1843 }
              , max:   { atk: 8985, hp: 11518 }
              , grail: { atk: 10879, hp: 13965 }
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
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.0 ~ 20.0
                          , Grant (AlliesType Divine) 3 AttackUp $ 10.0 ~ 20.0
                          ]
                }
              ]
  , passives: [magicResistance A, presenceConcealment APlus, coreOfGoddess EX]
  , phantasm: { name:   "Smile of the Stheno"
              , desc:   "Goddess' Smile"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   0
              , effect: [ To (EnemyType Male) Kill $ 100.0 ~ 150.0
                        , Chance 150 <<< Debuff Enemy 3 DefenseDown $ Flat 20.0
                        ]
              , over:   [ Chances 100 200 $
                          Debuff (EnemyType Male) 1 Charm Full ]
              , first:  false
              }
  , gen:      { starWeight: 104, starRate: 25.0, npAtk: 2.26, npDef: 4 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    27.5
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 4]
              [Piece Assassin: 10, SerpentJewel: 5]
              [Monument Assassin: 4, HeartOfTheForeignGod: 4, Void'sDust: 8]
              [Monument Assassin: 10, Void'sDust: 16, Dragon'sReverseScale: 4]
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 10]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 10, HeartOfTheForeignGod: 2]
              [SecretGemOf Assassin: 4, HeartOfTheForeignGod: 4]
              [SecretGemOf Assassin: 10, SerpentJewel: 4]
              [SerpentJewel: 7, ClawOfChaos: 3]
              [ClawOfChaos: 9, Dragon'sReverseScale: 8]
  }
, { name:     "Fuuma \"Evil-wind\" Kotarou"
  , id:       117
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    8
  , stats:    { base:  { atk: 1316,  hp: 1592 }
              , max:   { atk: 7091, hp: 8844 }
              , grail: { atk: 9597, hp: 11991 }
              }
  , skills:   [ { name:   "Sabotage"
                , rank:   BPlus
                , icon:   IconSwordDown
                , cd:     7
                , effect: [ Debuff Enemies 3 AttackDown $ Flat 10.0
                          , Debuff Enemies 3 CritChance $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Ninjutsu"
                , rank:   APlus
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Ally 1 Evasion Full
                          , Grant Ally 1 StarUp $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Suspicious Shadow"
                , rank:   C
                , icon:   IconHoodDown
                , cd:     7
                , effect: [ Debuff Enemies 1 DebuffVuln $ 50.0 ~ 100.0 ]
                }
              ]
  , passives: [presenceConcealment APlus]
  , phantasm: { name:   "Immortal Chaos Brigade"
              , desc:   "Fumetsu no Konton Ryodan"
              , rank:   B
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage $ 600.0 ~ 1000.0 ]
              , over:   [ Debuff Enemies 5 Confusion $ 30.0 ~ 70.0 ]
              , first:  false
              }
  , gen:      { starWeight: 100, starRate: 25.6, npAtk: 0.54, npDef: 4 }
  , hits:     { quick: 4, arts: 4, buster: 1, ex: 4 }
  , traits:   [Male, EnumaElish]
  , death:    38.5
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 4]
              [Piece Assassin: 8, EvilBone: 15]
              [Monument Assassin: 4, Void'sDust: 13, OctupletCrystals: 4]
              [Monument Assassin: 8, OctupletCrystals: 7, BlackBeastGrease: 5]
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 8]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 8, Void'sDust: 7]
              [SecretGemOf Assassin: 4, Void'sDust: 13]
              [SecretGemOf Assassin: 8, EvilBone: 10]
              [EvilBone: 20, SeedOfYggdrasil: 5]
              [SeedOfYggdrasil: 15, ClawOfChaos: 10]
  }
, { name:     "Sasaki Kojirou"
  , id:       39
  , rarity:   1
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    6
  , stats:    { base:  { atk: 1042,  hp: 1244 }
              , max:   { atk: 5735, hp: 6220 }
              , grail: { atk: 8912, hp: 9588 }
              }
  , skills:   [ { name:   "Mind's Eye (Fake)"
                , rank:   A
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Vitrify"
                , rank:   BPlus
                , icon:   IconBubbles
                , cd:     6
                , effect: [ To Self RemoveMental Full
                          , To Party GainStars $ 5.0 ~ 15.0
                          ]
                }
              , { name:   "Knowledge of the Sowa"
                , rank:   B
                , icon:   IconBullseye
                , cd:     7
                , effect: [ Grant Self 3 SureHit Full
                          , Grant Self 3 StarUp $ 10.0 ~ 30.0
                          ]
                }
              ]
  , passives: [presenceConcealment D]
  , phantasm: { name:   "Swallow Reversal"
              , desc:   "Hiken - Tsubame Gaeshi"
              , rank:   Unknown
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   3
              , effect: [ To Enemy Damage $ 1200.0 ~ 2000.0 ]
              , over:   [ To Party GainStars $ 15.0 ~ 35.0 ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 25.3, npAtk: 1.05, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    55.0
  , align:    [Neutral, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 2]
              [Piece Assassin: 4, ProofOfHero: 8]
              [Monument Assassin: 2, EternalGear: 2, Void'sDust: 7]
              [Monument Assassin: 4, EternalGear: 4, OctupletCrystals: 4]
  , skillUp:  Reinforcement
              [GemOf Assassin: 2]
              [GemOf Assassin: 4]
              [MagicGemOf Assassin: 2]
              [MagicGemOf Assassin: 4, Void'sDust: 4]
              [SecretGemOf Assassin: 2, Void'sDust: 7]
              [SecretGemOf Assassin: 4, ProofOfHero: 5]
              [ProofOfHero: 10, EvilBone: 6]
              [OctupletCrystals: 8, EvilBone: 18]
  }
, { name:     "Hassan of the Cursed Arm"
  , id:       40
  , rarity:   2
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    2
  , stats:    { base:  { atk: 1114,  hp: 1429 }
              , max:   { atk: 6280, hp: 7594 }
              , grail: { atk: 9100, hp: 10960 }
              }
  , skills:   [ { name:   "Throw (Dagger)"
                , rank:   B
                , icon:   IconStar
                , cd:     6
                , effect: [ To Party GainStars $ 3.0 ~ 12.0 ]
                }
              , { name:   "Self-Modification"
                , rank:   C
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 3 CritUp $ 20.0 ~ 50.0 ]
                }
              , { name:   "Protection Against the Wind"
                , rank:   A
                , icon:   IconDodge
                , cd:     7
                , effect: [ Times 3 $ Grant Self 0 Evasion Full
                          , Grant Self 3 StarUp $ 10.0 ~ 30.0
                          ]
                }
              ]
  , passives: [presenceConcealment APlus]
  , phantasm: { name:   "Zabaniya"
              , desc:   "Delusional Heartbeat"
              , rank:   C
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage $ 1200.0 ~ 2000.0 ]
              , over:   [ To Enemy Kill $ 80.0 ~ 120.0 ]
              , first:  true
              }
  , gen:      { starWeight: 97, starRate: 25.2, npAtk: 1.07, npDef: 4 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    44.0
  , align:    [Lawful, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 3]
              [Piece Assassin: 6, EvilBone: 11]
              [Monument Assassin: 3, EternalGear: 5, Void'sDust: 5]
              [Monument Assassin: 6, HomunculusBaby: 6, Void'sDust: 10]
  , skillUp:  Reinforcement
              [GemOf Assassin: 3]
              [GemOf Assassin: 6]
              [MagicGemOf Assassin: 3]
              [MagicGemOf Assassin: 6, EternalGear: 3]
              [SecretGemOf Assassin: 3, EternalGear: 5]
              [SecretGemOf Assassin: 6, EvilBone: 8]
              [GhostLantern: 3, EvilBone: 15]
              [GhostLantern: 9, HomunculusBaby: 12]
  }
, { name:     "Henry Jekyll & Hyde"
  , id:       81
  , rarity:   3
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    23
  , stats:    { base:  { atk: 1173,  hp: 1741 }
              , max:   { atk: 6320, hp: 9675 }
              , grail: { atk: 8553, hp: 13118 }
              }
  , skills:   [ { name:   "Monstrous Strength"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 5.0 ~ 15.0
                          , When "transformed into Hyde" <<<
                            Grant Self 3 AttackUp $ 25.0 ~ 35.0
                          ]
                }
              , { name:   "Panicky Voice"
                , rank:   A
                , icon:   IconStun
                , cd:     8
                , effect: [ Times 1 <<<
                            Grant Self 0 (Success Stun) $ 5.0  ~ 15.0
                          , When "transformed into Hyde" <<< Times 1 <<<
                            Grant Self 0 (Success Stun) $ 85.0 ~ 135.0
                          , Chance 10 $ Debuff Enemy 1 Stun Full
                          ]
                }
              , { name:   "Self-Modification"
                , rank:   D
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 3 CritUp $ 5.0 ~ 15.0
                          , When "transformed into Hyde" <<<
                            Grant Self 3 CritUp $ 25.0 ~ 35.0
                          ]
                }
              ]
  , passives: [presenceConcealment A, madness A]
  , phantasm: { name:   "Dangerous Game"
              , desc:   "The Secret Game of Sin"
              , rank:   CPlus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   0
              , effect: [ To Self BecomeHyde Full
                        , Grant Self 0 MaxHP $ 3000.0 ~ 6000.0
                        , To Self Heal Full
                        ]
              , over:   [ Grant Self 0 (Performance Buster) $ 40.0 ~ 80.0 ]
              , first:  false
              }
  , gen:      { starWeight: 99, starRate: 25.6, npAtk: 1.05, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    55.0
  , align:    [Lawful, Good, Chaotic, Evil]
  , limited:  false
  , free:     true
  , ascendUp: Ascension
              [Piece Assassin: 4]
              [EvilBone: 15, Piece Assassin: 8]
              [Void'sDust: 13, OctupletCrystals: 4, Monument Assassin: 4]
              [Monument Assassin: 8, OctupletCrystals: 7, HomunculusBaby: 8]
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 8]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 8, Void'sDust: 7]
              [SecretGemOf Assassin: 4, Void'sDust: 13]
              [SecretGemOf Assassin: 8, EvilBone: 10]
              [EvilBone: 20, HeartOfTheForeignGod: 2]
              [HeartOfTheForeignGod: 5, HomunculusBaby: 16]
  }
, { name:     "Jing Ke"
  , id:       42
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    8
  , stats:    { base:  { atk: 1338,  hp: 1492 }
              , max:   { atk: 7207, hp: 8293 }
              , grail: { atk: 9754, hp: 11244 }
              }
  , skills:   [ { name:   "Restrain"
                , rank:   A
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 3 StarAbsorb $ Flat 200.0
                          , To Party GainStars $ 5.0 ~ 15.0
                          ]
                }
              , { name:   "Planning"
                , rank:   B
                , icon:   IconStarHaloUp
                , cd:     7
                , effect: [ Grant Self 3 StarUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Insolent"
                , rank:   A
                , icon:   IconQuickUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Quick) $ 20.0 ~ 30.0
                          , Grant Self 1 CritUp $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [presenceConcealment B]
  , phantasm: { name:   "All I Do Is Kill"
              , desc:   ""
              , rank:   B
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage $ 1200.0 ~ 2000.0
                        , To Self DemeritDamage $ Flat 1000.0
                        ]
              , over:   [ To Enemy Kill $ 50.0 ~ 100.0
                        , To Party GainStars $ 15.0 ~ 35.0
                        ]
              , first:  true
              }
  , gen:      { starWeight: 98, starRate: 25.2, npAtk: 1.05, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    55.0
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 4]
              [EvilBone: 15, Piece Assassin: 8]
              [GhostLantern: 4, OctupletCrystals: 7, Monument Assassin: 4]
              [GhostLantern: 7, Void'sDust: 16, Monument Assassin: 8]
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 8]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 8, OctupletCrystals: 4]
              [SecretGemOf Assassin: 4, OctupletCrystals: 7]
              [SecretGemOf Assassin: 8, EvilBone: 10]
              [PhoenixFeather: 4, EvilBone: 20]
              [PhoenixFeather: 10, Void'sDust: 32]
  }
, { name:     "Charles-Henri Sanson"
  , id:       43
  , rarity:   2
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    12
  , stats:    { base:  { atk: 968,  hp: 1568 }
              , max:   { atk: 5456, hp: 8309 }
              , grail: { atk: 7906, hp: 11991 }
              }
  , skills:   [ { name:   "Executioner"
                , rank:   APlusPlus
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 3 (AlignAffinity Evil) $ 40.0 ~ 60.0 ]
                }
              , { name:   "Medicine"
                , rank:   APlus
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Ally Heal $ 1000.0 ~ 3000.0
                          , To Ally RemoveDebuffs Full
                          ]
                }
              , { name:   "Human Study"
                , rank:   B
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 3 (AttackVs Human) $ 40.0 ~ 60.0 ]
                }
              ]
  , passives: [presenceConcealment D]
  , phantasm: { name:   "La Mort Espoir"
              , desc:   "Death is Hope For Tomorrow"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0 ]
              , over:   [ To Enemy Kill $ 30.0 ~ 80.0
                        , Debuff Enemy 3 DefenseDown $ 20.0 ~ 40.0
                        ]
              , first:  true
              }
  , gen:      { starWeight: 102, starRate: 24.8, npAtk: 1.06, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    49.5
  , align:    [Lawful, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 3]
              [Piece Assassin: 6, EvilBone: 11]
              [Monument Assassin: 3, HeartOfTheForeignGod: 1, Void'sDust: 10]
              [Monument Assassin: 6, HeartOfTheForeignGod: 2, HomunculusBaby: 6]
  , skillUp:  Reinforcement
              [GemOf Assassin: 3]
              [GemOf Assassin: 6]
              [MagicGemOf Assassin: 3]
              [MagicGemOf Assassin: 6, Void'sDust: 5]
              [SecretGemOf Assassin: 3, Void'sDust: 10]
              [SecretGemOf Assassin: 6, EvilBone: 8]
              [ForbiddenPage: 3, EvilBone: 15]
              [ForbiddenPage: 9, HomunculusBaby: 12]
  }
, { name:     "Hassan of the Hundred Personas"
  , id:       110
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    28
  , stats:    { base:  { atk: 1241,  hp: 1675 }
              , max:   { atk: 6686, hp: 9310 }
              , grail: { atk: 9049, hp: 12623 }
              }
  , skills:   [ { name:   "Librarian of Knowledge"
                , rank:   C
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Self 3 NPGen $ 10.0 ~ 20.0
                          , Grant Self 3 StarUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Wide Specialization"
                , rank:   APlus
                , icon:   IconAllUp
                , cd:     7
                , effect: [ Chances 60 80 <<<
                            Grant Self 3 (Performance Buster) $ Flat 30.0
                          , Chances 60 80 <<<
                            Grant Self 3 (Performance Quick) $ Flat 30.0
                          , Chances 60 80 <<<
                            Grant Self 3 (Performance Arts) $ Flat 30.0
                          , Grant Self 1 Evasion Full
                          ]
                }
              , { name:   "Battle Retreat"
                , rank:   B
                , icon:   IconHeal
                , cd:     8
                , effect: [ To Self Heal $ 2000.0 ~ 4000.0
                          , To Self DemeritBuffs Full
                          ]
                }
              ]
  , passives: [presenceConcealment A]
  , phantasm: { name:   "Zabaniya"
              , desc:   "Delusional Illusion"
              , rank:   BPlus
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   13
              , effect: [ To Enemy Damage $ 900.0 ~ 1500.0 ]
              , over:   [ Debuff Enemy 3 CritChance $ 10.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 97, starRate: 25.5, npAtk: 0.48, npDef: 4 }
  , hits:     { quick: 3, arts: 3, buster: 1, ex: 6 }
  , traits:   [Female, EnumaElish]
  , death:    44.0
  , align:    [Lawful, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 4]
              [Piece Assassin: 8, Void'sDust: 10]
              [Monument Assassin: 4, OctupletCrystals: 7, SerpentJewel: 3]
              [Monument Assassin: 4, SerpentJewel: 6, BlackBeastGrease: 5]
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 8]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 8, OctupletCrystals: 4]
              [SecretGemOf Assassin: 4, OctupletCrystals: 7]
              [SecretGemOf Assassin: 8, Void'sDust: 7]
              [Void'sDust: 13, EvilBone: 12]
              [EvilBone: 36, ClawOfChaos: 10]
  }
, { name:     "Hassan of the Serenity"
  , id:       124
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    23
  , stats:    { base:  { atk: 1232,  hp: 1675 }
              , max:   { atk: 6636, hp: 9310 }
              , grail: { atk: 8981, hp: 12623 }
              }
  , skills:   [ { name:   "Morph (Infiltration)"
                , rank:   C
                , icon:   IconExclamationDown
                , cd:     9
                , effect: [ Debuff Enemy 3 CritChance $ 10.0 ~ 20.0
                          , To Enemy GaugeDown $ Flat 1.0
                          ]
                }
              , { name:   "Throw (Dagger)"
                , rank:   C
                , icon:   IconStar
                , cd:     6
                , effect: [ To Party GainStars $ 2.0 ~ 12.0 ]
                }
              , { name:   "Dance of Silence"
                , rank:   B
                , icon:   IconReaperUp
                , cd:     8
                , effect: [ Grant Self 3 KillUp $ 20.0 ~ 50.0
                          , Grant Self 3 DebuffSuccess $ 20.0 ~ 50.0
                          ]
                }
              ]
  , passives: [presenceConcealment APlus, independentAction A]
  , phantasm: { name:   "Zabaniya"
              , desc:   "Delusional Poison Body"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   1
              , effect: [ Debuff Enemy 5 Poison $ Flat 1000.0
                        , Chance 40 $ Debuff Enemy 1 SealSkills Full
                        , Chance 40 $ Debuff Enemy 1 SealNP Full
                        , To Enemy Damage $ 900.0 ~ 1500.0
                        ]
              , over:   [ To Enemy Kill $ 60.0 ~ 100.0 ]
              , first:  true
              }
  , gen:      { starWeight: 102, starRate: 25.6, npAtk: 0.53, npDef: 4 }
  , hits:     { quick: 3, arts: 3, buster: 4, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    44.0
  , align:    [Lawful, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 4]
              [Piece Assassin: 8, EvilBone: 15]
              [Monument Assassin: 4, Fool'sChain: 20, TearstoneOfBlood: 2]
              [Monument Assassin: 8, TearstoneOfBlood: 4, BlackBeastGrease: 5]
  , skillUp:  Reinforcement
              [GemOf Assassin: 4]
              [GemOf Assassin: 8]
              [MagicGemOf Assassin: 4]
              [MagicGemOf Assassin: 8, Fool'sChain: 10]
              [SecretGemOf Assassin: 4, Fool'sChain: 20]
              [SecretGemOf Assassin: 8, EvilBone: 10]
              [EvilBone: 20, Void'sDust: 8]
              [Void'sDust: 10, LampOfEvilSealing: 10]
  }
, { name:     "Phantom of the Opera"
  , id:       44
  , rarity:   2
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    2
  , stats:    { base:  { atk: 1003,  hp: 1580 }
              , max:   { atk: 5654, hp: 8393 }
              , grail: { atk: 8193, hp: 12112 }
              }
  , skills:   [ { name:   "Innocent Monster"
                , rank:   D
                , icon:   IconStarTurn
                , cd:     7
                , effect: [ Grant Party 3 StarsPerTurn $ 3.0 ~ 9.0
                          , Debuff Self 3 DefenseDown $ Flat 14.0
                          ]
                }
              , { name:   "Siren Song"
                , rank:   B
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 60 90 $
                            Debuff (EnemyType Female) 1 Charm Full ]
                }
              , { name:   "Mental Corruption"
                , rank:   A
                , icon:   IconStaffUp
                , cd:     7
                , effect: [ Grant Self 3 MentalSuccess $ 5.0 ~ 25.0
                          , Grant Self 3 MentalResist $ 50.0 ~ 100.0
                          ]
                }
              ]
  , passives: [presenceConcealment A]
  , phantasm: { name:   "Christine Christine"
              , desc:   "Love Song Resounding through Hell"
              , rank:   BPlus
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies DamageThruDef $ 600.0 ~ 900.0 ]
              , over:   [ Debuff Enemies 6 DebuffVuln $ 50.0 ~ 100.0 ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 25.2, npAtk: 0.71, npDef: 4 }
  , hits:     { quick: 2, arts: 3, buster: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    49.5
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 3]
              [Piece Assassin: 6, GhostLantern: 4]
              [Monument Assassin: 3, Void'sDust: 10, EternalGear: 3]
              [Monument Assassin: 6, EternalGear: 5, EvilBone: 18]
  , skillUp:  Reinforcement
              [GemOf Assassin: 3]
              [GemOf Assassin: 6]
              [MagicGemOf Assassin: 3]
              [MagicGemOf Assassin: 6, Void'sDust: 5]
              [SecretGemOf Assassin: 3, Void'sDust: 10]
              [SecretGemOf Assassin: 6, GhostLantern: 3]
              [GhostLantern: 5, HeartOfTheForeignGod: 2]
              [HeartOfTheForeignGod: 4, EvilBone: 36]
  }
, { name:     "Mata Hari"
  , id:       45
  , rarity:   1
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , curve:    6
  , stats:    { base:  { atk: 977,  hp: 1313 }
              , max:   { atk: 5377, hp: 6565 }
              , grail: { atk: 8355, hp: 10120 }
              }
  , skills:   [ { name:   "Espionage"
                , rank:   APlusPlus
                , icon:   IconStarHaloUp
                , cd:     7
                , effect: [ Grant Self 3 StarUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Pheromone"
                , rank:   B
                , icon:   IconHeart
                , cd:     8
                , effect: [ Chances 30 60 $
                            Debuff (EnemiesType Male) 1 Charm Full
                          , Debuff Enemies 3 DefenseDown $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Double-cross"
                , rank:   B
                , icon:   IconCircuits
                , cd:     8
                , effect: [ Debuff Enemy 1 SealSkills Full
                          , Debuff Enemy 3 DefenseDown $ 10.0 ~ 20.0
                          ]
                }
              ]
  , passives: []
  , phantasm: { name:   "Mata Hari"
              , desc:   "The Woman with Sunny Eyes"
              , rank:   A
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Chances 40 60 $ Debuff Enemies 1 Charm Full ]
              , over:   [ Debuff Enemies 1 AttackDown $ 20.0 ~ 40.0
                        , Debuff Enemies 1 DefenseDown $ 20.0 ~ 40.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 24.6, npAtk: 2.1, npDef: 4 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    55.0
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Assassin: 2]
              [Piece Assassin: 4, PhoenixFeather: 2]
              [Monument Assassin: 2, EternalGear: 4, GhostLantern: 2]
              [Monument Assassin: 4, SerpentJewel: 4, GhostLantern: 4]
  , skillUp:  Reinforcement
              [GemOf Assassin: 2]
              [GemOf Assassin: 4]
              [MagicGemOf Assassin: 2]
              [MagicGemOf Assassin: 4, EternalGear: 2]
              [SecretGemOf Assassin: 2, EternalGear: 4]
              [SecretGemOf Assassin: 4, PhoenixFeather: 2]
              [HomunculusBaby: 2, PhoenixFeather: 3]
              [HomunculusBaby: 6, SerpentJewel: 7]
  }
]
