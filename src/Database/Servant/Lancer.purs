module Database.Servant.Lancer (lancers) where

import StandardLibrary
import Database.Model

lancers :: Array Servant
lancers = Servant <$>
[ { name:     "Scathach"
  , id:       70
  , rarity:   5
  , class:    Lancer
  , attr:     Star
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    4
  , stats:    { base:  { atk: 1758,  hp: 2174 }
              , max:   { atk: 11375, hp: 14825 }
              , grail: { atk: 12452, hp: 16241 }
              }
  , skills:   [ { name:   "Wisdom of Dun Scaith"
                , rank:   APlus
                , icon:   IconDodge
                , cd:     7
                , effect: [ Grant Self 1 Evasion Full
                          , Chance 80 <<<
                            Grant Self 3 CritUp $ 30.0 ~ 50.0
                          , Chance 80 <<<
                            Grant Self 3 StarAbsorb $ 300.0 ~ 500.0
                          ]
                }
              , { name:   "Primordial Rune"
                , rank:   Unknown
                , icon:   IconQuickUp
                , cd:     8
                , effect: [ Grant Ally 1 (Performance Quick) $ 30.0 ~ 50.0 ]
                }
              , { name:   "God-Slayer"
                , rank:   B
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 1 (AttackVs Divine) $ 50.0 ~ 100.0
                          , Grant Self 1 (AttackVs Undead) $ 50.0 ~ 100.0
                          ]
                }
              ]
  , passives: [magicResistance A]
  , phantasm: { name:   "Gáe Bolg Alternative"
              , desc:   "Soaring Spear that Pierces with Death"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ Chance 500 $ Debuff Enemy 1 Stun Full
                        , To Enemy Damage $ 1600.0 ~ 2400.0
                        ]
              , over:   [ To Enemy Kill $ 60.0 ~ 100.0 ]
              , first:  true
              }
  , gen:      { starWeight: 88, starRate: 12.2, npAtk: 0.71, npDef: 4 }
  , hits:     { quick: 2, arts: 3, buster: 6, ex: 7 }
  , traits:   [Female, EnumaElish, King]
  , death:    32.0
  , align:    [Neutral, Good]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 5]
              [Piece Lancer: 12, EvilBone: 22]
              [Monument Lancer: 5, Void'sDust: 20, ClawOfChaos: 3]
              [Monument Lancer: 12, ClawOfChaos: 6, HeartOfTheForeignGod: 5]
  , skillUp:  Reinforcement
              [GemOf Lancer: 5]
              [GemOf Lancer: 12]
              [MagicGemOf Lancer: 5]
              [MagicGemOf Lancer: 12, Void'sDust: 10]
              [SecretGemOf Lancer: 5, Void'sDust:20]
              [SecretGemOf Lancer: 12, EvilBone: 15]
              [EvilBone: 29, PhoenixFeather: 5]
              [PhoenixFeather: 15, HeartOfTheForeignGod: 10]
  }
, { name:     "Karna"
  , id:       85
  , rarity:   5
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    10
  , stats:    { base:  { atk: 1850,  hp: 1999 }
              , max:   { atk: 11976, hp: 13632 }
              , grail: { atk: 13110, hp: 14934 }
              }
  , skills:   [ { name:   "Knowledge of the Deprived"
                , rank:   A
                , icon:   IconCircuits
                , cd:     8
                , effect: [ Debuff Enemy 1 SealNP Full
                          , Debuff Enemy 1 DebuffVuln $ 30.0 ~ 50.0
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
              , { name:   "Uncrowned Arms Mastership"
                , rank:   Unknown
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ Flat 25.0
                          , Grant Self 3 StarUp $ 30.0 ~ 50.0
                          , Grant Self 3 CritUp $ 20.0 ~ 40.0
                          ]
                }
              ]
  , passives: [magicResistance C, divinity A, riding A]
  , phantasm: { name:   "Vasavi Shakti"
              , desc:   "O Sun, Abide to Death"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Divine"
              , hits:   5
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ To Enemies (DamageVs Divine) $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 88, starRate: 12.2, npAtk: 0.72, npDef: 4 }
  , hits:     { quick: 3, arts: 3, buster: 1, ex: 4 }
  , traits:   [Male, Riding, Brynhild, Divine, EnumaElish]
  , death:    28.0
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 5]
              [Piece Lancer: 12, OctupletCrystals: 8]
              [Monument Lancer: 5, EternalGear: 10, PhoenixFeather: 4]
              [Monument Lancer: 12, PhoenixFeather: 8, HeartOfTheForeignGod: 5]
  , skillUp:  Reinforcement
              [GemOf Lancer: 5]
              [GemOf Lancer: 12]
              [MagicGemOf Lancer: 5]
              [MagicGemOf Lancer: 12, EternalGear: 5]
              [SecretGemOf Lancer: 5, EternalGear: 10]
              [SecretGemOf Lancer: 12, OctupletCrystals: 5]
              [OctupletCrystals: 10, ProofOfHero: 18]
              [ProofOfHero: 54, HeartOfTheForeignGod: 10]
  }
, { name:     "Tamamo-no-Mae (Lancer)"
  , id:       128
  , rarity:   5
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    5
  , stats:    { base:  { atk: 1657,  hp: 2221 }
              , max:   { atk: 10726, hp: 15147 }
              , grail: { atk: 11741, hp: 16594 }
              }
  , skills:   [ { name:   "Beach Flower"
                , rank:   EX
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.0 ~ 20.0
                          , Grant (AlliesType Male) 3 StarUp $ 22.0 ~ 42.0
                          ]
                }
              , { name:   "Midsummer Curse"
                , rank:   A
                , icon:   IconHeart
                , cd:     9
                , effect: [ Debuff Enemy 1 Charm Full
                          , Debuff Enemy 3 DefenseDown $ 20.0 ~ 30.0
                          , Debuff Enemy 5 Curse $ 500.0 ~ 1000.0
                          , To Enemy DemeritCharge $ Flat 1.0
                          ]
                }
              , { name:   "Goddess Morph"
                , rank:   B
                , icon:   IconShield
                , cd:     8
                , effect: [ Grant Self 1 Invincibility Full
                          , Grant Self 1 CritUp $ 30.0 ~ 50.0
                          , Grant Self 1 StarUp $ 30.0 ~ 50.0
                          , Grant Self 1 NPGen $ 30.0 ~ 50.0
                          , Grant Self 1 DebuffResist $ 30.0 ~ 50.0
                          , Grant Self 1 HealingReceived $ 30.0 ~ 50.0
                          , Debuff Self 1 StunBomb Full
                          ]
                }
              ]
  , passives: [riding A, territoryCreation A, divinity APlusPlus]
  , phantasm: { name:   "Tokonatsu Nikkou—Goddess' Love Parasol"
              , desc:   ""
              , rank:   C
              , card:   Buster
              , kind:   "Anti-Unit"
              , hits:   4
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0 ]
              , over:   [ To Enemy (DamageVs Male) $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 91, starRate: 12.2, npAtk: 1.05, npDef: 4 }
  , hits:     { quick: 4, arts: 2, buster: 3, ex: 4 }
  , traits:   [Female, Divine, Riding, EnumaElish]
  , death:    40.0
  , align:    [Neutral, Summer]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 5]
              [Piece Lancer: 12, PhoenixFeather: 6]
              [Monument Lancer: 5, SeedOfYggdrasil: 12, Warhorse'sYoungHorn: 3]
              [ Monument Lancer: 12, Warhorse'sYoungHorn: 6
              , ShellOfReminiscence: 12
              ]
  , skillUp:  Reinforcement
              [GemOf Lancer: 5]
              [GemOf Lancer: 12]
              [MagicGemOf Lancer: 5]
              [MagicGemOf Lancer: 12, SeedOfYggdrasil: 6]
              [SecretGemOf Lancer: 5, SeedOfYggdrasil: 12]
              [SecretGemOf Lancer: 12, PhoenixFeather: 4]
              [PhoenixFeather: 8, ClawOfChaos: 4]
              [ClawOfChaos: 11, HeartOfTheForeignGod: 10]
  }
, { name:     "Brynhild"
  , id:       88
  , rarity:   5
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    25
  , stats:    { base:  { atk: 1766,  hp: 2174 }
              , max:   { atk: 11432, hp: 14825 }
              , grail: { atk: 12514, hp: 16241 }
              }
  , skills:   [ { name:   "Mana Burst (Flame)"
                , rank:   B
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 15.0 ~ 25.0
                          , Grant Self 1 NPUp $ 8.0 ~ 15.0
                          ]
                }
              , { name:   "Primordial Rune"
                , rank:   Unknown
                , icon:   IconExclamationDown
                , cd:     8
                , effect: [ Debuff Enemy 3 CritChance $ 30.0 ~ 50.0
                          , Debuff Enemy 1 NPDown $ 15.0 ~ 30.0
                          ]
                }
              , { name:   "Hero's Assistant"
                , rank:   C
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Ally 3 StarAbsorb $ 300.0 ~ 600.0
                          , To Ally Heal $ 1000.0 ~ 3000.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding A, divinity E]
  , phantasm: { name:   "Brynhild Romantia"
              , desc:   "Till Death Divides the Two"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0
                        , Grant Party 3 StarUp $ Flat 30.0
                        ]
              , over:   [ To Enemy (DamageVs Brynhild) $ 150.0 ~ 200.0 ]
              , first:  false
              }
  , gen:      { starWeight: 87, starRate: 12.2, npAtk: 1.07, npDef: 4 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 5 }
  , traits:   [Female, Riding, Divine, EnumaElish]
  , death:    32.0
  , align:    [Neutral, Good]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 5]
              [Piece Lancer: 12, HeartOfTheForeignGod: 3]
              [Monument Lancer: 5, SeedOfYggdrasil: 12, ProofOfHero:15]
              [Monument Lancer: 12, ProofOfHero: 29, GhostLantern: 12]
  , skillUp:  Reinforcement
              [GemOf Lancer: 5]
              [GemOf Lancer: 12]
              [MagicGemOf Lancer: 5]
              [MagicGemOf Lancer: 12, SeedOfYggdrasil: 6]
              [SecretGemOf Lancer: 5, SeedOfYggdrasil: 12]
              [SecretGemOf Lancer: 12, HeartOfTheForeignGod: 2]
              [HeartOfTheForeignGod: 4, PhoenixFeather: 5]
              [PhoenixFeather: 15, ClawOfChaos: 15]
  }
, { name:     "Altria Pendragon (Lancer)"
  , id:       119
  , rarity:   5
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    5
  , stats:    { base:  { atk: 1699,  hp: 2288 }
              , max:   { atk: 10995, hp: 15606 }
              , grail: { atk: 12036 , hp: 17097 }
              }
  , skills:   [ { name:   "Mana Burst"
                , rank:   A
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              , { name:   "Charisma"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 9.0 ~ 18.0 ]
                }
              , { name:   "Protection of World's End"
                , rank:   EX
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0
                          , To Self RemoveDebuffs Full
                          ]
                }
              ]
  , passives: [magicResistance B, riding A]
  , phantasm: { name:   "Rhongomyniad"
              , desc:   "Spear Shining at the End of the World"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   2
              , effect: [ Grant Self 1 IgnoreInvinc Full
                        , To Enemies Damage $ 300.0 ~ 500.0
                        ]
              , over:   [ To Self GaugeUp $ 20.0 ~ 60.0 ]
              , first:  false
              }
  , gen:      { starWeight: 89, starRate: 12.2, npAtk: 1.1, npDef: 4 }
  , hits:     { quick: 3, arts: 2, buster: 1, ex: 5 }
  , traits:   [Female, Arthur, Dragon, King, Riding, Saberface, EnumaElish]
  , death:    24.0
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 5]
              [Piece Lancer: 12, DragonFang: 18]
              [Monument Lancer: 5, GreatKnightMedal: 20, Warhorse'sYoungHorn: 3]
              [ Monument Lancer: 12, Warhorse'sYoungHorn: 6
              , Dragon'sReverseScale: 5
              ]
  , skillUp:  Reinforcement
              [GemOf Lancer: 5]
              [GemOf Lancer: 12]
              [MagicGemOf Lancer: 5]
              [MagicGemOf Lancer: 12, GreatKnightMedal: 10]
              [SecretGemOf Lancer: 5, GreatKnightMedal: 20]
              [SecretGemOf Lancer: 12, DragonFang: 12]
              [DragonFang: 24, MeteorHorseshoe: 6]
              [MeteorHorseshoe: 18, HeartOfTheForeignGod: 10]
  }
, { name:     "Altria Pendragon (Lancer Alter)"
  , id:       78
  , rarity:   4
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    29
  , stats:    { base:  { atk: 1661,  hp: 1881 }
              , max:   { atk: 9968, hp: 11761 }
              , grail: { atk: 12069, hp: 14260 }
              }
  , skills:   [ { name:   "Mana Burst"
                , rank:   APlus
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 35.0 ~ 55.0 ]
                }
              , { name:   "Protection of World's End"
                , rank:   A
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 1 StarAbsorb $ 500.0 ~ 1000.0
                          , Grant Self 1 CritUp $ 30.0 ~ 50.0
                          , To Party GainStars $ 5.0 ~ 10.0
                          ]
                }
              , { name:   "Charisma"
                , rank:   E
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 6.0 ~ 12.0 ]
                }
              ]
  , passives: [magicResistance A, riding A]
  , phantasm: { name:   "Rhongomyniad"
              , desc:   "Spear that Shines to the End of the World"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   4
              , effect: [ To Enemies DamageThruDef $ 400.0 ~ 600.0
                        , Chance 60 $ Debuff Enemies 1 SealNP Full
                        ]
              , over:   [ Debuff Enemies 5 Curse $ 1000.0 ~ 3000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 88, starRate: 11.8, npAtk: 0.74, npDef: 4 }
  , hits:     { quick: 2, arts: 3, buster: 1, ex: 4 }
  , traits:   [Female, Riding, Dragon, Saberface, Arthur, EnumaElish, King]
  , death:    23.0
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 10, MeteorHorseshoe: 6]
              [Monument Lancer: 4, PhoenixFeather: 7, Dragon'sReverseScale: 2]
              [ Monument Lancer: 10, Dragon'sReverseScale: 4
              , HeartOfTheForeignGod: 4
              ]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 10]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 10, PhoenixFeather: 4]
              [SecretGemOf Lancer: 4, PhoenixFeather: 7]
              [SecretGemOf Lancer: 10, MeteorHorseshoe: 4]
              [MeteorHorseshoe: 8, DragonFang: 12]
              [DragonFang: 36, HeartOfTheForeignGod: 8]
  }
, { name:     "Li Shuwen"
  , id:       102
  , rarity:   4
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    14
  , stats:    { base:  { atk: 1608,  hp: 1817 }
              , max:   { atk: 9653, hp: 11360 }
              , grail: { atk: 11688, hp: 13774 }
              }
  , skills:   [ { name:   "Chinese Martial Arts (Liu He Da Qiang)"
                , rank:   APlusPlus
                , icon:   IconBullseye
                , cd:     8
                , effect: [ Grant Self 1 SureHit Full
                          , Grant Self 1 CritUp $ 50.0 ~ 100.0
                          ]
                }
              , { name:   "Sphere Boundary"
                , rank:   B
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 1 StarAbsorb $ 300.0 ~ 500.0
                          ]
                }
              , { name:   "Juezhao"
                , rank:   B
                , icon:   IconShieldBreak
                , cd:     8
                , effect: [ Grant Self 1 IgnoreInvinc Full
                          , Grant Self 1 (Performance Arts) $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [magicResistance D]
  , phantasm: { name:   "Shen Qiang Wu Er Da"
              , desc:   "Divine Spear—No Second Strike"
              , rank:   Unknown
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   3
              , effect: [ To Enemy DamageThruDef $ 900.0 ~ 1500.0
                        , Debuff Enemy 3 DefenseDown $ Flat 20.0
                        ]
              , over:   [ To Enemy Kill $ 40.0 ~ 80.0 ]
              , first:  false
              }
  , gen:      { starWeight: 87, starRate: 12.2, npAtk: 0.52, npDef: 4 }
  , hits:     { quick: 3, arts: 3, buster: 1, ex: 5 }
  , traits:   [Male, EnumaElish]
  , death:    40.0
  , align:    [Neutral, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 10, DragonFang: 15]
              [Monument Lancer: 4, OctupletCrystals: 8, ClawOfChaos: 3]
              [Monument Lancer: 10, ClawOfChaos: 5, Dragon'sReverseScale: 4]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 10]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 10, OctupletCrystals: 4]
              [SecretGemOf Lancer: 4, OctupletCrystals: 8]
              [SecretGemOf Lancer: 10, DragonFang: 10]
              [DragonFang: 20, Void'sDust: 10]
              [Void'sDust: 30, Warhorse'sYoungHorn: 12]
  }
, { name:     "Kiyohime (Lancer)"
  , id:       134
  , rarity:   4
  , class:    Lancer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    4
  , stats:    { base:  { atk: 1489,  hp: 1899 }
              , max:   { atk: 8936, hp: 11870 }
              , grail: { atk: 10820, hp: 14392 }
              }
  , skills:   [ { name:   "Passionate Summer"
                , rank:   A
                , icon:   IconExclamationDown
                , cd:     7
                , effect: [ Debuff Enemies 3 CritChance $ 20.0 ~ 30.0 ]
                }
              , { name:   "Bath Transformation"
                , rank:   A
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 3 (Performance Buster) $ 20.0 ~ 30.0 ]
                }
              , { name:   "Stalking"
                , rank:   A
                , icon:   IconShieldDown
                , cd:     7
                , effect: [ Chance 500 <<<
                            Debuff Enemy 3 DefenseDown $ 20.0 ~ 30.0
                          , Grant Enemy 3 AttackUp $ Flat 20.0
                          ]
                }
              ]
  , passives: [madness EX, magicResistance D]
  , phantasm: { name:   "Dojo-ji Bell Form 108—Karyu-nagi"
              , desc:   ""
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   6
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0
                        , Chance 150 $ Debuff Enemy 1 SealSkills Full
                        ]
              , over:   [ Debuff Enemy 5 Burn $ 500.0 ~ 2500.0 ]
              , first:  false
              }
  , gen:      { starWeight: 92, starRate: 12.0, npAtk: 1.05, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 6, ex: 5 }
  , traits:   [Female, Dragon, EnumaElish]
  , death:    40.0
  , align:    [Chaotic, Evil]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 10, OctupletCrystals: 6]
              [Monument Lancer: 4, ShellOfReminiscence: 8, BlackBeastGrease: 3]
              [Monument Lancer: 10, BlackBeastGrease: 5, LampOfEvilSealing: 6]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 10]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 10, ShellOfReminiscence: 4]
              [SecretGemOf Lancer: 4, ShellOfReminiscence: 8]
              [SecretGemOf Lancer: 10, OctupletCrystals: 4]
              [OctupletCrystals: 8, DragonFang: 12]
              [DragonFang: 36, Dragon'sReverseScale: 8]
  }
, { name:     "Elisabeth Bathory"
  , id:       18
  , rarity:   4
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    4
  , stats:    { base:  { atk: 1520,  hp: 1899 }
              , max:   { atk: 9122, hp: 11870 }
              , grail: { atk: 11045, hp: 14392 }
              }
  , skills:   [ { name:   "Charisma"
                , rank:   C
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 8.0 ~ 16.0 ]
                }
              , { name:   "Torture Technique"
                , rank:   A
                , icon:   IconShieldDown
                , cd:     7
                , effect: [ Debuff Enemy 3 DefenseDown $ 10.0 ~ 20.0 ]
                }
              , { name:   "Battle Continuation"
                , rank:   B
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 4 Guts $ 750.0 ~ 2000.0 ]
                }
              ]
  , passives: [magicResistance A, territoryCreation B]
  , phantasm: { name:   "Báthory Erzsébet"
              , desc:   "Fresh Blood Demoness"
              , rank:   EMinus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemies DamageThruDef $ 300.0 ~ 500.0 ]
              , over:   [ Debuff Enemies 3 Curse $ 500.0 ~ 2500.0 ]
              , first:  false
              }
  , gen:      { starWeight: 90, starRate: 11.8, npAtk: 1.1, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, Dragon, EnumaElish]
  , death:    24.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 10, DragonFang: 15]
              [Monument Lancer: 4, Dragon'sReverseScale: 4, ClawOfChaos: 3]
              [Monument Lancer: 10, ClawOfChaos: 5, PhoenixFeather: 8]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 10]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 10, Dragon'sReverseScale: 2]
              [SecretGemOf Lancer: 4, Dragon'sReverseScale: 4]
              [SecretGemOf Lancer: 10, DragonFang: 10]
              [DragonFang: 20, ForbiddenPage: 5]
              [ForbiddenPage: 15, PhoenixFeather: 16]
  }
, { name:     "Fionn mac Cumhaill"
  , id:       87
  , rarity:   4
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    9
  , stats:    { base:  { atk: 1488,  hp: 2040 }
              , max:   { atk: 8930, hp: 12750 }
              , grail: { atk: 10812, hp: 15459 }
              }
  , skills:   [ { name:   "Clairvoyance"
                , rank:   B
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 StarUp $ 19.0 ~ 38.0 ]
                }
              , { name:   "Trouble with Women"
                , rank:   A
                , icon:   IconDodge
                , cd:     12
                , effect: [ Chances 60 100 $ Grant Self 1 Evasion Full
                          , Grant Self 1 Taunt Full
                          , Debuff Self 3 CharmVuln $ Flat 80.0
                          ]
                }
              , { name:   "Magecraft"
                , rank:   B
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Arts) $ 24.0 ~ 40.0 ]
                }
              ]
  , passives: [magicResistance B, divinity D]
  , phantasm: { name:   "Mac an Luin"
              , desc:   "Undefeated Violet Flower"
              , rank:   APlus
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage $ 600.0 ~ 900.0
                        , Grant Self 3 DebuffResist Full
                        ]
              , over:   [ Debuff Enemies 3 AttackDown $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 89, starRate: 12.3, npAtk: 0.55, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 4 }
  , traits:   [Male, Divine, EnumaElish]
  , death:    32.0
  , align:    [Neutral, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 10, PhoenixFeather: 5]
              [Monument Lancer: 4, EvilBone: 24, ClawOfChaos: 3]
              [Monument Lancer: 10, ClawOfChaos: 5, Void'sDust: 20]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 10]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 10, EvilBone: 12]
              [SecretGemOf Lancer: 4, EvilBone: 24]
              [SecretGemOf Lancer: 10, PhoenixFeather: 4]
              [PhoenixFeather: 7, OctupletCrystals: 5]
              [OctupletCrystals: 15, SerpentJewel: 16]
  }
, { name:     "Cu Chulainn"
  , id:       17
  , rarity:   3
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    8
  , stats:    { base:  { atk: 1334,  hp: 1726 }
              , max:   { atk: 7239, hp: 9593 }
              , grail: { atk: 9797, hp: 13007 }
              }
  , skills:   [ { name:   "Battle Continuation"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1000.0 ~ 2500.0 ]
                }
              , { name:   "Protection from Arrows"
                , rank:   B
                , icon:   IconDodge
                , cd:     7
                , effect: [ Times 3 $ Grant Self 0 Evasion Full
                          , Grant Self 3 DefenseUp $ 8.0 ~ 16.0
                          ]
                }
              , { name:   "Disengage"
                , rank:   C
                , icon:   IconBubbles
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , To Self Heal $ 500.0 ~ 1500.0
                          ]
                }
              ]
  , passives: [magicResistance C, divinity B]
  , phantasm: { name:   "Gáe Bolg"
              , desc:   "Barbed Spear that Pierces with Death"
              , rank:   B
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage $ 1200.0 ~ 2000.0 ]
              , over:   [ To Enemy Kill $ 50.0 ~ 100.0
                        , Debuff Enemy 3 DefenseDown $ 10.0 ~ 30.0
                        ]
              , first:  true
              }
  , gen:      { starWeight: 87, starRate: 12.1, npAtk: 1.07, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Divine, Brynhild, EnumaElish]
  , death:    32.0
  , align:    [Lawful, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 8, ClawOfChaos: 3]
              [Monument Lancer: 4, SeedOfYggdrasil: 8, OctupletCrystals: 4]
              [Monument Lancer: 8, OctupletCrystals: 7, PhoenixFeather: 7]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 8]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 8, SeedOfYggdrasil: 4]
              [SecretGemOf Lancer: 4, SeedOfYggdrasil: 8]
              [SecretGemOf Lancer: 8, ClawOfChaos: 2]
              [ClawOfChaos: 4, ProofOfHero: 12]
              [ProofOfHero: 36, PhoenixFeather: 13]
  }
, { name:     "Cu Chulainn (Prototype)"
  , id:       20
  , rarity:   3
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    13
  , stats:    { base:  { atk: 1315,  hp: 1817 }
              , max:   { atk: 7082, hp: 10098 }
              , grail: { atk: 9584, hp: 13691 }
              }
  , skills:   [ { name:   "Rune Spell"
                , rank:   B
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 3 CritUp $ 18.0 ~ 45.0
                          , Grant Self 3 DebuffResist $ 18.0 ~ 45.0
                          ]
                }
              , { name:   "Protection from Arrows"
                , rank:   B
                , icon:   IconDodge
                , cd:     7
                , effect: [ Times 3 $ Grant Self 0 Evasion Full
                          , Grant Self 3 DefenseUp $ 8.0 ~ 16.0
                          ]
                }
              , { name:   "Beast Slayer"
                , rank:   BPlus
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 3 (AttackVs Beast) $ 40.0 ~ 60.0 ]
                }
              ]
  , passives: [magicResistance C, divinity B]
  , phantasm: { name:   "Gáe Bolg"
              , desc:   "The Spear which Reverses Causality"
              , rank:   B
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage $ 1200.0 ~ 2000.0 ]
              , over:   [ To Enemy Kill $ 50.0 ~ 100.0
                        , Debuff Enemy 3 DefenseDown $ 10.0 ~ 30.0
                        ]
              , first:  true
              }
  , gen:      { starWeight: 88, starRate: 12.1, npAtk: 1.08, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Divine, EnumaElish]
  , death:    28.0
  , align:    [Lawful, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 8, Void'sDust: 10]
              [Monument Lancer: 4, SeedOfYggdrasil: 8, ClawOfChaos: 2]
              [Monument Lancer: 8, ClawOfChaos: 4, OctupletCrystals: 8]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 8]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 8, SeedOfYggdrasil: 4]
              [SecretGemOf Lancer: 4, SeedOfYggdrasil: 8]
              [SecretGemOf Lancer: 8, Void'sDust: 7]
              [Void'sDust: 13, ProofOfHero: 12]
              [ProofOfHero: 36, OctupletCrystals: 16]
  }
, { name:     "Leonidas I"
  , id:       21
  , rarity:   2
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    7
  , stats:    { base:  { atk: 1168,  hp: 1498 }
              , max:   { atk: 6583, hp: 7959 }
              , grail: { atk: 9539, hp: 11486 }
              }
  , skills:   [ { name:   "Rear Guard's Pride"
                , rank:   A
                , icon:   IconCrosshairUp
                , cd:     8
                , effect: [ Grant Self 1 Taunt Full
                          , Grant Self 3 NPGen $ 50.0 ~ 100.0
                          ]
                }
              , { name:   "Battle Continuation"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1000.0 ~ 2500.0 ]
                }
              , { name:   "Warrior's War Cry"
                , rank:   B
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Party 3 (Performance Buster) $ 15.0 ~ 25.0 ]
                }
              ]
  , passives: [magicResistance C]
  , phantasm: { name:   "Thermopylae Enomotia"
              , desc:   "Guardian of the Hot Gates"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   0
              , effect: [ Grant Self 3 Taunt Full
                        , To Party GainStars $ 5.0 ~ 25.0
                        ]
              , over:   [ Grant Self 3 DefenseUp $ 30.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 89, starRate: 11.8, npAtk: 1.07, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, King]
  , death:    32.0
  , align:    [Lawful, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 3]
              [Piece Lancer: 6, ClawOfChaos: 3]
              [Monument Lancer: 3, Void'sDust: 10, OctupletCrystals: 3]
              [Monument Lancer: 6, OctupletCrystals: 5, EvilBone: 18]
  , skillUp:  Reinforcement
              [GemOf Lancer: 3]
              [GemOf Lancer: 6]
              [MagicGemOf Lancer: 3]
              [MagicGemOf Lancer: 6, Void'sDust: 5]
              [SecretGemOf Lancer: 3, Void'sDust: 10]
              [SecretGemOf Lancer: 6, ClawOfChaos: 2]
              [ClawOfChaos: 3, SerpentJewel: 3]
              [SerpentJewel: 8, EvilBone: 36]
  }
, { name:     "Romulus"
  , id:       22
  , rarity:   3
  , class:    Lancer
  , attr:     Star
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    3
  , stats:    { base:  { atk: 1344,  hp: 1779 }
              , max:   { atk: 7239, hp: 9883 }
              , grail: { atk: 9797, hp: 13400 }
              }
  , skills:   [ { name:   "Natural Body"
                , rank:   C
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 OffensiveResist $ 50.0 ~ 100.0
                          , To Self Heal $ 1000.0 ~ 2500.0
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
              , { name:   "Seven Hills"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Ally 1 Guts $ Flat 1000.0
                          , Grant Ally 1 (Performance Buster) $ 10.0 ~ 30.0
                          ]
                }
              ]
  , passives: [magicResistance B]
  , phantasm: { name:   "Magna Voluisse Magnum"
              , desc:   "All Things Lead to My Spear"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   7
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ Grant Party 3 AttackUp $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 90, starRate: 12.1, npAtk: 1.07, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Roman, King]
  , death:    32.0
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 8, OctupletCrystals: 5]
              [Monument Lancer: 4, PhoenixFeather: 6, EvilBone: 10]
              [Monument Lancer: 8, EvilBone: 20, Void'sDust: 16]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 8]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 8, PhoenixFeather: 3]
              [SecretGemOf Lancer: 4, PhoenixFeather: 6]
              [SecretGemOf Lancer: 8, OctupletCrystals: 4]
              [OctupletCrystals: 7, EternalGear: 4]
              [EternalGear: 12, Void'sDust: 32]
  }
, { name:     "Hektor"
  , id:       64
  , rarity:   3
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , curve:    3
  , stats:    { base:  { atk: 1334,  hp: 1726 }
              , max:   { atk: 6928 , hp: 10200 }
              , grail: { atk: 9376, hp: 13829 }
              }
  , skills:   [ { name:   "Tactics"
                , rank:   CPlus
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Party 1 NPUp $ 8.5 ~ 17.0 ]
                }
              , { name:   "Proof of Friendship"
                , rank:   C
                , icon:   IconDarkMagic
                , cd:     7
                , effect: [ Chances 60 80 <<< To Enemy GaugeDown $ Flat 1.0
                          , Chances 60 80 $ Debuff Enemy 1 Stun Full
                          ]
                }
              , { name:   "Disengage"
                , rank:   B
                , icon:   IconBubbles
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , To Self Heal $ 800.0 ~ 2000.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding B]
  , phantasm: { name:   "Durindana"
              , desc:   "Ultimate Unbroken Spear"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies DamageThruDef $ 400.0 ~ 600.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 90, starRate: 12.2, npAtk: 1.08, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Riding, EnumaElish, GreekMythMale]
  , death:    28.0
  , align:    [Lawful, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 8, MeteorHorseshoe: 5]
              [Monument Lancer: 4, ProofOfHero: 20, SeedOfYggdrasil: 4]
              [Monument Lancer: 8, SeedOfYggdrasil: 8, PhoenixFeather: 7]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 8]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 8, ProofOfHero: 10]
              [SecretGemOf Lancer: 4, ProofOfHero: 20]
              [SecretGemOf Lancer: 8, MeteorHorseshoe: 4]
              [MeteorHorseshoe: 7, OctupletCrystals: 4]
              [OctupletCrystals: 12, PhoenixFeather: 13]
  }
, { name:     "Musashibou Benkei"
  , id:       19
  , rarity:   2
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    7
  , stats:    { base:  { atk: 1029,  hp: 1722 }
              , max:   { atk: 5801, hp: 9149 }
              , grail: { atk: 8406, hp: 13204 }
              }
  , skills:   [ { name:   "Vengeful Spirit Exorcism"
                , rank:   A
                , icon:   IconCircuits
                , cd:     7
                , effect: [ Chances 50 100 $ Debuff Enemy 1 SealSkills Full ]
                }
              , { name:   "Imposing Stance"
                , rank:   B
                , icon:   IconCrosshairUp
                , cd:     7
                , effect: [ Grant Self 1 Taunt Full
                          , Grant Self 1 DefenseUp $ 30.0 ~ 60.0
                          ]
                }
              , { name:   "Blank Subscription List"
                , rank:   Unknown
                , icon:   IconCircuits
                , cd:     10
                , effect: [ Chances 60 80 $ Debuff Enemies 1 SealNP Full ]
                }
              ]
  , passives: [magicResistance CPlus]
  , phantasm: { name:   "Pilgrimage of the Five Hundred Arhat"
              , desc:   ""
              , rank:   EX
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Chances 50 80 $ Debuff Enemies 1 Stun Full ]
              , over:   [ Debuff Enemies 3 Curse $ 500.0 ~ 2500.0 ]
              , first:  false
              }
  , gen:      { starWeight: 89, starRate: 11.9, npAtk: 0.79, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    36.0
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 3]
              [Piece Lancer: 6, EvilBone: 11]
              [Monument Lancer: 3, SeedOfYggdrasil: 6, HomunculusBaby: 3]
              [Monument Lancer: 6, HomunculusBaby: 5, Void'sDust: 12]
  , skillUp:  Reinforcement
              [GemOf Lancer: 3]
              [GemOf Lancer: 6]
              [MagicGemOf Lancer: 3]
              [MagicGemOf Lancer: 6, SeedOfYggdrasil: 3]
              [SecretGemOf Lancer: 3, SeedOfYggdrasil: 6]
              [SecretGemOf Lancer: 6, EvilBone: 8]
              [EvilBone: 15, Void'sDust: 6]
              [Void'sDust: 42]
  }
, { name:     "Diarmuid Ua Duibhne"
  , id:       71
  , rarity:   3
  , class:    Lancer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , curve:    8
  , stats:    { base:  { atk: 1277,  hp: 1817 }
              , max:   { atk: 6877, hp: 10098 }
              , grail: { atk: 9307, hp: 13691 }
              }
  , skills:   [ { name:   "Mind's Eye (True)"
                , rank:   B
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 DefenseUp $ 9.0 ~ 18.0
                          ]
                }
              , { name:   "Love Spot"
                , rank:   C
                , icon:   IconSwordDown
                , cd:     7
                , effect: [ Debuff (EnemiesType Female) 1 AttackDown $
                            30.0 ~ 50.0 ]
                }
              , { name:   "Knight's Strategy"
                , rank:   B
                , icon:   IconStarHaloUp
                , cd:     7
                , effect: [ Grant Self 3 StarUp $ 30.0 ~ 50.0 ]
                }
              ]
  , passives: [magicResistance B]
  , phantasm: { name:   "Gáe Dearg and Gáe Buidhe"
              , desc:   "Crimson Rose of Exorcism and Yellow Rose of Mortality"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   2
              , effect: [ To Enemy Damage $ 1600.0 ~ 2400.0
                        , To Enemy RemoveBuffs Full
                        ]
              , over:   [ Debuff Enemy 5 Curse $ 500.0 ~ 1500.0 ]
              , first:  false
              }
  , gen:      { starWeight: 87, starRate: 12.3, npAtk: 0.79, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 2, ex: 4 }
  , traits:   [Male, EnumaElish, Brynhild]
  , death:    36.0
  , align:    [Lawful, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Lancer: 4]
              [Piece Lancer: 10, SeedOfYggdrasil: 6]
              [Monument Lancer: 4, PhoenixFeather: 6, SerpentJewel: 3]
              [Monument Lancer: 8, SerpentJewel: 6, Void'sDust: 16]
  , skillUp:  Reinforcement
              [GemOf Lancer: 4]
              [GemOf Lancer: 8]
              [MagicGemOf Lancer: 4]
              [MagicGemOf Lancer: 8, PhoenixFeather: 3]
              [SecretGemOf Lancer: 4, PhoenixFeather: 6]
              [SecretGemOf Lancer: 8, SeedOfYggdrasil: 4]
              [SeedOfYggdrasil: 8, ProofOfHero: 12]
              [ProofOfHero: 36, Void'sDust: 32]
  }
]
