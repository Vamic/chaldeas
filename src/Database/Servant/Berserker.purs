module Database.Servant.Berserker (berserkers) where

import StandardLibrary
import Database.Model

berserkers :: Array Servant
berserkers = Servant <$>
[ { name:     "Cu Chulainn (Alter)"
  , id:       98
  , rarity:   5
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    10
  , stats:    { base:  { atk: 1979,  hp: 1790 }
              , max:   { atk: 12805, hp: 12210 }
              , grail: { atk: 14017, hp: 13377 }
              }
  , skills:   [ { name:   "Madness of the Spirits"
                , rank:   A
                , icon:   IconExclamationDown
                , cd:     8
                , effect: [ Debuff Enemies 3 AttackDown $ 10.0 ~ 20.0
                          , Debuff Enemies 3 CritChance $ 30.0 ~ 50.0
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
              , { name:   "Battle Continuation"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1000.0 ~ 2500.0 ]
                }
              ]
  , passives: [madness C, divinity C]
  , phantasm: { name:   "Curruid Coinchenn"
              , desc:   "Beast of Crunching Death Fangs"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   12
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0 ]
              , over:   [ Grant Self 1 AttackUp $ 30.0 ~ 70.0
                        , Grant Self 1 DefenseUp $ 30.0 ~ 70.0
                        ]
              , first:  true
              }
  , gen:      { starWeight: 9, starRate: 5.1, npAtk: 0.69, npDef: 5 }
  , hits:     { quick: 4, arts: 3, buster: 3, ex: 5 }
  , traits:   [Male, Divine, Brynhild, EnumaElish]
  , death:    52.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 5]
              [Piece Berserker: 12, EvilBone: 22]
              [Monument Berserker: 5, ClawOfChaos: 6, HeartOfTheForeignGod: 2]
              [ Monument Berserker: 12, HeartOfTheForeignGod: 4
              , TearstoneOfBlood: 8
              ]
  , skillUp:  Reinforcement
              [GemOf Berserker: 5]
              [GemOf Berserker: 12]
              [MagicGemOf Berserker: 5]
              [MagicGemOf Berserker: 12, ClawOfChaos: 3]
              [SecretGemOf Berserker: 5, ClawOfChaos: 6]
              [SecretGemOf Berserker: 12, EvilBone: 15]
              [EvilBone: 29, SerpentJewel: 5]
              [SerpentJewel: 15, Void'sDust: 48]
  }
, { name:     "Minamoto-no-Raikou"
  , id:       114
  , rarity:   5
  , class:    Berserker
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    30
  , stats:    { base:  { atk: 1786,  hp: 1980 }
              , max:   { atk: 11556, hp: 13500 }
              , grail: { atk: 12650, hp: 14790 }
              }
  , skills:   [ { name:   "Eternal Arms Mastery"
                , rank:   APlus
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 3 StarAbsorb $ 3000.0 ~ 6000.0 ]
                }
              , { name:   "Mana Burst (Lightning)"
                , rank:   A
                , icon:   IconBusterUp
                , cd:     8
                , effect: [ Grant Self 1 (Performance Buster) $ 20.0 ~ 30.0
                          , Grant Self 1 Evasion Full
                          ]
                }
              , { name:   "Mystic Slayer"
                , rank:   A
                , icon:   IconDamageUp
                , cd:     8
                , effect: [ Grant Self 3 (AttackVs Demonic) $ 30.0 ~ 50.0
                          , Grant Self 3 (AttackVs HeavenOrEarth) $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [magicResistance D, madness EX, riding APlus, divinity C]
  , phantasm: { name:   "Vengeful Lightning of the Ox-King"
              , desc:   "Goou Shourai—Tenmoukaikai"
              , rank:   BPlusPlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   7
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ Grant Self 1 StarUp $ 100.0 ~ 500.0 ]
              , first:  true
              }
  , gen:      { starWeight: 9, starRate: 4.9, npAtk: 0.46, npDef: 5 }
  , hits:     { quick: 3, arts: 4, buster: 1, ex: 5 }
  , traits:   [Female, Divine, Riding, EnumaElish]
  , death:    39.0
  , align:    [Chaotic, Good]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 5]
              [Piece Berserker: 12, ClawOfChaos: 5]
              [Monument Berserker: 5, EvilBone: 29, TearstoneOfBlood: 3]
              [ Monument Berserker: 12, TearstoneOfBlood: 6
              , Dragon'sReverseScale: 5
              ]
  , skillUp:  Reinforcement
              [GemOf Berserker: 5]
              [GemOf Berserker: 12]
              [MagicGemOf Berserker: 5]
              [MagicGemOf Berserker: 12, EvilBone: 15]
              [SecretGemOf Berserker: 5, EvilBone: 29]
              [SecretGemOf Berserker: 12, ClawOfChaos: 3]
              [ClawOfChaos: 6, OctupletCrystals: 6]
              [OctupletCrystals: 18, SpiritRoot: 10]
  }
, { name:     "Sakata Kintoki"
  , id:       51
  , rarity:   5
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    9
  , stats:    { base:  { atk: 1964,  hp: 1782 }
              , max:   { atk: 12712, hp: 12150 }
              , grail: { atk: 13915, hp: 13311 }
              }
  , skills:   [ { name:   "Monstrous Strength"
                , rank:   APlus
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 1 AttackUp $ 30.0 ~ 50.0 ]
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
  , passives: [madness E, divinity D]
  , phantasm: { name:   "Golden Spark"
              , desc:   "Golden Impact"
              , rank:   CMinus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy DamageThruDef $ 600.0 ~ 1000.0 ]
              , over:   [ Chances 50 100 $ Debuff Enemy 1 Stun Full ]
              , first:  false
              }
  , gen:      { starWeight: 9, starRate: 5.0, npAtk: 1.03, npDef: 5 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Divine, Brynhild, EnumaElish]
  , death:    52.0
  , align:    [Lawful, Good]
  , limited:  true
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 5]
              [Piece Berserker: 12, ProofOfHero: 22]
              [Monument Berserker: 5, OctupletCrystals: 10, SeedOfYggdrasil: 12]
              [ Monument Berserker: 12, SeedOfYggdrasil: 12
              , HeartOfTheForeignGod: 5
              ]
  , skillUp:  Reinforcement
              [GemOf Berserker: 5]
              [GemOf Berserker: 12]
              [MagicGemOf Berserker: 5]
              [MagicGemOf Berserker: 12, OctupletCrystals: 5]
              [SecretGemOf Berserker: 5, OctupletCrystals: 10]
              [SecretGemOf Berserker: 12, ProofOfHero: 15]
              [ProofOfHero: 29, Void'sDust: 12]
              [Void'sDust: 36, HeartOfTheForeignGod: 10]
  }
, { name:     "Vlad III"
  , id:       52
  , rarity:   5
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    15
  , stats:    { base:  { atk: 1777,  hp: 2019 }
              , max:   { atk: 11499, hp: 13770 }
              , grail: { atk: 12587, hp: 15086 }
              }
  , skills:   [ { name:   "Vampirism"
                , rank:   A
                , icon:   IconDarkMagic
                , cd:     8
                , effect: [ Chances 80 100 <<< To Enemy GaugeDown $ Flat 1.0
                          , To Self GaugeUp $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Morph"
                , rank:   C
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 16.0 ~ 24.0 ]
                }
              , { name:   "Battle Continuation"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1000.0 ~ 2500.0 ]
                }
              ]
  , passives: [madness EX]
  , phantasm: { name:   "Kazikli Bey"
              , desc:   "Bloodstained Demon King"
              , rank:   CPlus
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   10
              , effect: [ To Enemy Damage $ 900.0 ~ 1500.0 ]
              , over:   [ To Party GainStars $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 9, starRate: 4.9, npAtk: 0.5, npDef: 5 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhild, King]
  , death:    45.5
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 5]
              [Piece Berserker: 12, Dragon'sReverseScale: 3]
              [Monument Berserker: 5, ClawOfChaos: 6, ForbiddenPage: 5]
              [Monument Berserker: 12, Void'sDust: 24, ForbiddenPage: 10]
  , skillUp:  Reinforcement
              [GemOf Berserker: 5]
              [GemOf Berserker: 12]
              [MagicGemOf Berserker: 5]
              [MagicGemOf Berserker: 12, ClawOfChaos: 3]
              [SecretGemOf Berserker: 5, ClawOfChaos: 6]
              [SecretGemOf Berserker: 12, Dragon'sReverseScale: 2]
              [Dragon'sReverseScale: 4, EvilBone: 18]
              [Void'sDust: 48, EvilBone: 54]
  }
, { name:     "Florence Nightingale"
  , id:       97
  , rarity:   5
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , curve:    30
  , stats:    { base:  { atk: 1573,  hp: 2232 }
              , max:   { atk: 10184, hp: 15221 }
              , grail: { atk: 11148, hp: 16675 }
              }
  , skills:   [ { name:   "Nurse of Steel"
                , rank:   A
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Ally Heal $ 2000.0 ~ 4000.0 ]
                }
              , { name:   "Understanding of the Human Body"
                , rank:   A
                , icon:   IconDamageUp
                , cd:     8
                , effect: [ Grant Self 3 (AttackVs Humanoid) $ 30.0 ~ 50.0
                          , Grant Self 3 (DefenseVs Humanoid) $ 15.0 ~ 25.0
                          ]
                }
              , { name:   "Angel's Cry"
                , rank:   EX
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Ally 3 (Performance Buster) $ 30.0 ~ 50.0 ]
                }
              ]
  , passives: [madness EX]
  , phantasm: { name:   "Nightingale Pledge"
              , desc:   "I Will Abstain From Whatever Is Deleterious And Mischievous"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ To Party RemoveDebuffs Full
                        , To Party Heal $ 3000.0 ~ 5000.0
                        ]
              , over:   [ Debuff Enemies 1 NPDown $ 50.0 ~ 100.0 ]
              , first:  false
              }
  , gen:      { starWeight: 10, starRate: 5.0, npAtk: 0.77, npDef: 5 }
  , hits:     { quick: 6, arts: 2, buster: 1, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    56.8
  , align:    [Lawful, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 5]
              [Piece Berserker: 12, PhoenixFeather: 6]
              [Monument Berserker: 5, SeedOfYggdrasil: 12, ClawOfChaos: 3]
              [Monument Berserker: 12, ClawOfChaos: 6, HomunculusBaby: 12]
  , skillUp:  Reinforcement
              [GemOf Berserker: 5]
              [GemOf Berserker: 12]
              [MagicGemOf Berserker: 5]
              [MagicGemOf Berserker: 12, SeedOfYggdrasil: 6]
              [SecretGemOf Berserker: 5, SeedOfYggdrasil: 12]
              [SecretGemOf Berserker: 12, PhoenixFeather: 4]
              [PhoenixFeather: 8, TearstoneOfBlood: 4]
              [TearstoneOfBlood: 11, GhostLantern: 24]
  }
, { name:     "Heracles"
  , id:       47
  , rarity:   4
  , class:    Berserker
  , attr:     Heaven
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    9
  , stats:    { base:  { atk: 1775,  hp: 1652 }
              , max:   { atk: 10655, hp: 10327 }
              , grail: { atk: 12901, hp: 12521 }
              }
  , skills:   [ { name:   "Valor"
                , rank:   APlus
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 10.5 ~ 31.0
                          , Grant Self 3 MentalResist $ 21.0 ~ 42.0
                          ]
                }
              , { name:   "Mind's Eye (Fake)"
                , rank:   B
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 18.0 ~ 36.0
                          ]
                }
              , { name:   "Battle Continuation"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1000.0 ~ 2500.0 ]
                }
              ]
  , passives: [madness B, divinity A]
  , phantasm: { name:   "Nine Lives"
              , desc:   "Shooting Down a Hundred Heads"
              , rank:   APlus
              , card:   Buster
              , kind:   "Unknown"
              , hits:   15
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0 ]
              , over:   [ Debuff Enemy 3 DefenseDown $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 10, starRate: 5.0, npAtk: 1.07, npDef: 5 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Divine, EnumaElish, GreekMythMale]
  , death:    39.0
  , align:    [Chaotic, Mad]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 4]
              [Piece Berserker: 10, OctupletCrystals: 6]
              [Monument Berserker: 4, ClawOfChaos: 3, HeartOfTheForeignGod: 4]
              [Monument Berserker: 10, ClawOfChaos: 5, Dragon'sReverseScale: 4]
  , skillUp:  Reinforcement
              [GemOf Berserker: 4]
              [GemOf Berserker: 10]
              [MagicGemOf Berserker: 4]
              [MagicGemOf Berserker: 10, HeartOfTheForeignGod: 2]
              [SecretGemOf Berserker: 4, HeartOfTheForeignGod: 4]
              [SecretGemOf Berserker: 10, OctupletCrystals: 4]
              [ProofOfHero: 15, OctupletCrystals: 8]
              [ProofOfHero: 45, Dragon'sReverseScale: 8]
  }
, { name:     "Frankenstein"
  , id:       82
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    24
  , stats:    { base:  { atk: 1573,  hp: 1710 }
              , max:   { atk: 9441, hp: 10687 }
              , grail: { atk: 11431, hp: 12958 }
              }
  , skills:   [ { name:   "Galvanism"
                , rank:   B
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Self 3 NPGen $ 25.0 ~ 45.0 ]
                }
              , { name:   "Wail of the Living Dead"
                , rank:   C
                , icon:   IconStun
                , cd:     8
                , effect: [ Chance 60 $ Debuff Enemy 1 Stun Full
                          , Debuff Enemy 1 DefenseDown $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Overload"
                , rank:   C
                , icon:   IconBeamUp
                , cd:     7
                , effect: [ Grant Self 1 NPUp $ 20.0 ~ 30.0
                          , Debuff Self 5 Burn $ Flat 300.0
                          ]
                }
              ]
  , passives: [madness D]
  , phantasm: { name:   "Blasted Tree"
              , desc:   "Lightning Tree of Crucifixion"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage $ 900.0 ~ 1300.0
                        , Chance 500 $ Debuff Self 2 Stun Full
                        ]
              , over:   [ Debuff Enemies 3 CritChance $ 20.0 ~ 40.0 ]
              , first:  false
              }
  , gen:      { starWeight: 10, starRate: 4.9, npAtk: 0.83, npDef: 5 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    58.5
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 4]
              [Piece Berserker: 10, HomunculusBaby: 6]
              [Monument Berserker: 4, GhostLantern: 4, EvilBone: 24]
              [Monument Berserker: 10, GhostLantern: 8, EternalGear: 10]
  , skillUp:  Reinforcement
              [GemOf Berserker: 4]
              [GemOf Berserker: 10]
              [MagicGemOf Berserker: 4]
              [MagicGemOf Berserker: 10, EvilBone: 12]
              [SecretGemOf Berserker: 4, EvilBone: 24]
              [SecretGemOf Berserker: 10, HomunculusBaby: 5]
              [OctupletCrystals: 5, HomunculusBaby: 8]
              [OctupletCrystals: 15, EternalGear: 20]
  }
, { name:     "Ibaraki-Douji"
  , id:       116
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    29
  , stats:    { base:  { atk: 1606,  hp: 1752 }
              , max:   { atk: 9636, hp: 10954 }
              , grail: { atk: 11667, hp: 13282 }
              }
  , skills:   [ { name:   "Demonic Nature of Oni"
                , rank:   A
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.0 ~ 20.0
                          , Grant Self 3 NPUp $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Disengage"
                , rank:   A
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              , { name:   "Morph"
                , rank:   A
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 10.0 ~ 30.0
                          , Grant Self 1 DefenseUp $ Flat 30.0
                          ]
                }
              ]
  , passives: [madness B]
  , phantasm: { name:   "Great Grudge of Rashomon"
              , desc:   "Rashomon Daiengi"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemy Damage $ 600.0 ~ 1000.0
                        , To Enemy RemoveBuffs Full
                        ]
              , over:   [ Debuff Enemy 3 DefenseDown $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 10, starRate: 4.9, npAtk: 1.03, npDef: 5 }
  , hits:     { quick: 4, arts: 2, buster: 1, ex: 5 }
  , traits:   [Female, Demonic, EnumaElish]
  , death:    52.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 4]
              [Piece Berserker: 10, OctupletCrystals: 6]
              [Monument Berserker: 4, ClawOfChaos: 5, PhoenixFeather: 4]
              [Monument Berserker: 10, PhoenixFeather: 7
              , HeartOfTheForeignGod: 4
              ]
  , skillUp:  Reinforcement
              [GemOf Berserker: 4]
              [GemOf Berserker: 10]
              [MagicGemOf Berserker: 4]
              [MagicGemOf Berserker: 10, ClawOfChaos: 3]
              [SecretGemOf Berserker: 4, ClawOfChaos: 5]
              [SecretGemOf Berserker: 10, OctupletCrystals: 4]
              [OctupletCrystals: 8, Void'sDust: 10]
              [Void'sDust: 30, TearstoneOfBlood: 12]

  }
, { name:     "Lancelot"
  , id:       48
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    14
  , stats:    { base:  { atk: 1746,  hp: 1652 }
              , max:   { atk: 10477, hp: 10327 }
              , grail: { atk: 12685, hp: 12521 }
              }
  , skills:   [ { name:   "Eternal Arms Mastery"
                , rank:   APlus
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 3 StarAbsorb $ 3000.0 ~ 6000.0 ]
                }
              , { name:   "Protection of the Spirits"
                , rank:   A
                , icon:   IconStarHaloUp
                , cd:     7
                , effect: [ Grant Self 3 StarUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Mana Reversal"
                , rank:   A
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Self 1 NPGen $ 50.0 ~ 100.0
                          , Grant Self 3 CritUp $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [magicResistance E, madness C]
  , phantasm: { name:   "Knight of Owner"
              , desc:   "A Knight Does Not Die Empty-Handed"
              , rank:   APlusPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   10
              , effect: [ To Enemies Damage $ 600.0 ~ 1000.0 ]
              , over:   [ Grant Self 3 AttackUp $ 10.0 ~ 30.0 ]
              , first:  true
              }
  , gen:      { starWeight: 10, starRate: 5.0, npAtk: 0.5, npDef: 5 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    52.0
  , align:    [Lawful, Mad]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 4]
              [Piece Berserker: 10, Dragon'sReverseScale: 3]
              [Monument Berserker: 4, ClawOfChaos: 5, Void'sDust: 8]
              [Monument Berserker: 10, DragonFang: 24, Void'sDust: 16]
  , skillUp:  Reinforcement
              [GemOf Berserker: 4]
              [GemOf Berserker: 10]
              [MagicGemOf Berserker: 4]
              [MagicGemOf Berserker: 10, ClawOfChaos: 3]
              [SecretGemOf Berserker: 4, ClawOfChaos: 5]
              [SecretGemOf Berserker: 10, Dragon'sReverseScale: 2]
              [MeteorHorseshoe: 5, Dragon'sReverseScale: 4]
              [MeteorHorseshoe: 15, DragonFang: 48]
  }
, { name:     "Beowulf"
  , id:       89
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    4
  , stats:    { base:  { atk: 1707,  hp: 1652 }
              , max:   { atk: 10247, hp: 10327 }
              , grail: { atk: 12407, hp: 12521 }
              }
  , skills:   [ { name:   "Berserk"
                , rank:   A
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 1 AttackUp $ 20.0 ~ 30.0
                          , Grant Self 1 NPUp $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Intuition"
                , rank:   B
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Battle Continuation"
                , rank:   B
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 4 Guts $ 750.0 ~ 2000.0 ]
                }
              ]
  , passives: [madness EMinus]
  , phantasm: { name:   "Grendel Buster"
              , desc:   "Primal Conflict"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   12
              , effect: [ Grant Self 1 SureHit Full
                        , To Enemy Damage $ 700.0 ~ 1100.0
                        ]
              , over:   [ Debuff Enemy 3 CritChance $ 30.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 10, starRate: 4.9, npAtk: 0.68, npDef: 5 }
  , hits:     { quick: 3, arts: 3, buster: 1, ex: 4 }
  , traits:   [Male, EnumaElish, Brynhild, King]
  , death:    58.5
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 4]
              [Piece Berserker: 10, ProofOfHero: 18]
              [ Monument Berserker: 4, Dragon'sReverseScale: 2
              , OctupletCrystals: 8
              ]
              [Monument Berserker: 10, Dragon'sReverseScale: 4, EvilBone: 30]
  , skillUp:  Reinforcement
              [GemOf Berserker: 4]
              [GemOf Berserker: 10]
              [MagicGemOf Berserker: 4]
              [MagicGemOf Berserker: 10, OctupletCrystals: 4]
              [SecretGemOf Berserker: 4, OctupletCrystals: 8]
              [SecretGemOf Berserker: 10, ProofOfHero: 12]
              [SeedOfYggdrasil: 6, ProofOfHero: 24]
              [SeedOfYggdrasil: 18, DragonFang: 48]
  }
, { name:     "Tamamo Cat"
  , id:       58
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    9
  , stats:    { base:  { atk: 1504,  hp: 1833 }
              , max:   { atk: 9026, hp: 11458 }
              , grail: { atk: 10929, hp: 13893 }
              }
  , skills:   [ { name:   "Monstrous Strength"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 2 AttackUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Curse"
                , rank:   E
                , icon:   IconDarkMagic
                , cd:     7
                , effect: [ Chances 40 60 <<< To Enemy GaugeDown $ Flat 1.0 ]
                }
              , { name:   "Morph"
                , rank:   B
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 18.0 ~ 27.0 ]
                }
              ]
  , passives: [madness C]
  , phantasm: { name:   "Napping in the Dazzling Sunshine and Feasting"
              , desc:   "Sansan Nikkou Hiruyasumi Shuchi Nikurin"
              , rank:   D
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemies Damage $ 600.0 ~ 1000.0
                        , Chance 500 $ Debuff Self 2 Stun Full
                        ]
              , over:   [ Grant Self 3 HealPerTurn $ 2000.0 ~ 6000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 10, starRate: 5.0, npAtk: 0.71, npDef: 5 }
  , hits:     { quick: 2, arts: 3, buster: 2, ex: 3 }
  , traits:   [Female, Beast, EnumaElish]
  , death:    39.0
  , align:    [Chaotic, Good]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 4]
              [Piece Berserker: 10, GhostLantern: 6]
              [Monument Berserker: 4, HomunculusBaby: 8, ClawOfChaos: 3]
              [Monument Berserker: 10, ClawOfChaos: 5, HeartOfTheForeignGod: 4]
  , skillUp:  Reinforcement
              [GemOf Berserker: 4]
              [GemOf Berserker: 10]
              [MagicGemOf Berserker: 4]
              [MagicGemOf Berserker: 10, HomunculusBaby: 4]
              [SecretGemOf Berserker: 4, HomunculusBaby: 8]
              [SecretGemOf Berserker: 10, GhostLantern: 4]
              [GhostLantern: 8, OctupletCrystals: 5]
              [OctupletCrystals: 15, HeartOfTheForeignGod: 8]
  }
, { name:     "Lu Bu Fengxian"
  , id:       49
  , rarity:   3
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    8
  , stats:    { base:  { atk: 1507,  hp: 1494 }
              , max:   { atk: 8119, hp: 8302 }
              , grail: { atk: 10988, hp: 11256 }
              }
  , skills:   [ { name:   "Valor"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 9.0 ~ 27.0
                          , Grant Self 3 MentalResist $ 18.0 ~ 36.0
                          ]
                }
              , { name:   "Defiant"
                , rank:   B
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 15.0 ~ 25.0
                          , Debuff Self 3 BuffFail $ Flat 50.0
                          ]
                }
              , { name:   "Chaotic Villain"
                , rank:   A
                , icon:   IconBeamUp
                , cd:     8
                , effect: [ Grant Self 1 NPUp $ 20.0 ~ 30.0
                          , Grant Self 1 StarAbsorb $ Flat 3000.0
                          , Debuff Others 1 DefenseDown $ Flat 20.0
                          ]
                }
              ]
  , passives: [madness A]
  , phantasm: { name:   "God Force"
              , desc:   "Five Weapons of the War God"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel/Anti-Army/Anti-Fortress"
              , hits:   1
              , effect: [ To Enemy DamageThruDef $ 600.0 ~ 1000.0 ]
              , over:   [ Chances 30 70 $ Debuff Enemy 1 Stun Full ]
              , first:  false
              }
  , gen:      { starWeight: 9, starRate: 5.0, npAtk: 1.04, npDef: 5 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    50.3
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 4]
              [Piece Berserker: 8, MeteorHorseshoe: 5]
              [Monument Berserker: 4, ClawOfChaos: 2, EvilBone: 20]
              [Monument Berserker: 8, ClawOfChaos: 4, Void'sDust: 16]
  , skillUp:  Reinforcement
              [GemOf Berserker: 4]
              [GemOf Berserker: 8]
              [MagicGemOf Berserker: 4]
              [MagicGemOf Berserker: 8, EvilBone: 10]
              [SecretGemOf Berserker: 4, EvilBone: 20]
              [SecretGemOf Berserker: 8, MeteorHorseshoe: 4]
              [OctupletCrystals: 4, MeteorHorseshoe: 7]
              [OctupletCrystals: 12, Void'sDust: 32]
  }
, { name:     "Spartacus"
  , id:       50
  , rarity:   1
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    6
  , stats:    { base:  { atk: 922,  hp: 1544 }
              , max:   { atk: 5073, hp: 7722 }
              , grail: { atk: 7883, hp: 11904 }
              }
  , skills:   [ { name:   "Honor of Suffering"
                , rank:   BPlus
                , icon:   IconHealTurn
                , cd:     9
                , effect: [ Grant Self 5 HealPerTurn $ 500.0 ~ 1500.0 ]
                }
              , { name:   "Unyielding Will"
                , rank:   A
                , icon:   IconKneel
                , cd:     7
                , effect: [ Times 1 <<< Grant Self 5 Guts $ Flat 1.0
                          , To Self GaugeUp $ 10.0 ~ 30.0
                          ]
                }
              , { name:   "Triumphant Return of the Sword"
                , rank:   B
                , icon:   IconBusterUp
                , cd:     8
                , effect: [ Grant Self 1 (Performance Buster) $ 20.0 ~ 40.0
                          , To Self Heal $ 1000.0 ~ 2000.0
                          ]
                }
              ]
  , passives: [madness EX]
  , phantasm: { name:   "Crying Warmonger"
              , desc:   "Howl of the Wounded Beast"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemies DamageThruDef $ 300.0 ~ 500.0 ]
              , over:   [ To Self Heal $ 3000.0 ~ 7000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 9, starRate: 4.9, npAtk: 1.01, npDef: 5 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, Roman, EnumaElish]
  , death:    65.0
  , align:    [Neutral, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 2]
              [Piece Berserker: 4, OctupletCrystals: 3]
              [Monument Berserker: 2, HomunculusBaby: 2, ProofOfHero: 10]
              [Monument Berserker: 4, HomunculusBaby: 4, EvilBone: 12]
  , skillUp:  Reinforcement
              [GemOf Berserker: 2]
              [GemOf Berserker: 4]
              [MagicGemOf Berserker: 2]
              [MagicGemOf Berserker: 4, ProofOfHero: 5]
              [SecretGemOf Berserker: 2, ProofOfHero: 10]
              [SecretGemOf Berserker: 4, OctupletCrystals: 2]
              [SeedOfYggdrasil: 3, OctupletCrystals: 4]
              [SeedOfYggdrasil: 8, EvilBone: 24]
  }
, { name:     "Asterios"
  , id:       53
  , rarity:   1
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    1
  , stats:    { base:  { atk: 1097,  hp: 1320 }
              , max:   { atk: 6037, hp: 6604 }
              , grail: { atk: 9381, hp: 10181 }
              }
  , skills:   [ { name:   "Monstrous Strength"
                , rank:   A
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Natural Demon"
                , rank:   APlusPlus
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 OffensiveResist $ 50.0 ~ 100.0
                          , Grant Self 3 DefenseUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Labrys of the Abyss"
                , rank:   C
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 1 StarAbsorb $ 3000.0 ~ 6000.0
                          , Grant Self 1 (Performance Buster) $ 20.0 ~ 30.0
                          ]
                }
              ]
  , passives: [madness B]
  , phantasm: { name:   "Chaos Labyrinth"
              , desc:   "Eternally Unchanging Labyrinth"
              , rank:   EX
              , card:   Arts
              , kind:   "Maze"
              , hits:   0
              , effect: [ Debuff Enemies 6 AttackDown $ 10.0 ~ 20.0
                        , Debuff Enemies 1 AttackDown $ Flat 40.0
                        , Debuff Enemies 1 DefenseDown $ Flat 40.0
                        ]
              , over:   [ Debuff Enemies 6 DefenseDown $ 10.0 ~ 30.0 ]
              , first:  false
              }
  , gen:      { starWeight: 9, starRate: 4.9, npAtk: 0.68, npDef: 5 }
  , hits:     { quick: 2, arts: 3, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, GreekMythMale]
  , death:    58.5
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 2]
              [OctupletCrystals: 3, Piece Berserker: 4]
              [MeteorHorseshoe: 2, ProofOfHero: 10, Monument Berserker: 2]
              [MeteorHorseshoe: 4, SeedOfYggdrasil: 5, Monument Berserker: 4]
  , skillUp:  Reinforcement
              [GemOf Berserker: 2]
              [GemOf Berserker: 4]
              [MagicGemOf Berserker: 2]
              [MagicGemOf Berserker: 4, ProofOfHero: 5]
              [SecretGemOf Berserker: 2, ProofOfHero: 10]
              [SecretGemOf Berserker: 4, OctupletCrystals: 2]
              [Void'sDust: 4, OctupletCrystals: 4]
              [SeedOfYggdrasil: 10, Void'sDust: 12]
  }
, { name:     "Kiyohime"
  , id:       56
  , rarity:   3
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    3
  , stats:    { base:  { atk: 1233,  hp: 1649 }
              , max:   { atk: 6644, hp: 9166 }
              , grail: { atk: 8992, hp: 12428 }
              }
  , skills:   [ { name:   "Morph"
                , rank:   C
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 16.0 ~ 24.0 ]
                }
              , { name:   "Stalking"
                , rank:   B
                , icon:   IconShieldDown
                , cd:     7
                , effect: [ Debuff Enemy 4 DefenseDown $ 12.0 ~ 24.0
                          , Grant Enemy 3 AttackUp $ Flat 20.0
                          ]
                }
              , { name:   "Flame-Coloured Kiss"
                , rank:   A
                , icon:   IconBusterUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Buster) $ 20.0 ~ 30.0
                          , To Self RemoveDebuffs Full
                          ]
                }
              ]
  , passives: [madness EX]
  , phantasm: { name:   "Transforming, Flame-Emitting Meditation"
              , desc:   "Tenshin Kashou Zanmai"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   3
              , effect: [ To Enemies Damage $ 100.0 ~ 500.0 ]
              , over:   [ Chances 50 80 $ Debuff Enemies 1 Stun Full
                        , Debuff Enemies 10 Burn $ 500.0 ~ 900.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 9, starRate: 4.9, npAtk: 2.03, npDef: 5 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Female, Dragon, EnumaElish]
  , death:    65.0
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     true
  , ascendUp: Ascension
              [Piece Berserker: 4]
              [Piece Berserker: 8, GhostLantern: 5]
              [Monument Berserker: 4, EvilBone: 20, DragonFang: 8]
              [Monument Berserker: 8, Dragon'sReverseScale: 4, DragonFang: 16]
  , skillUp:  Reinforcement
              [GemOf Berserker: 4]
              [GemOf Berserker: 8]
              [MagicGemOf Berserker: 4]
              [MagicGemOf Berserker: 8, EvilBone: 10]
              [SecretGemOf Berserker: 4, EvilBone: 20]
              [SecretGemOf Berserker: 8, GhostLantern: 4]
              [SeedOfYggdrasil: 5, GhostLantern: 7]
              [SeedOfYggdrasil: 15, Dragon'sReverseScale: 7]
  }
, { name:     "Eric Bloodaxe"
  , id:       57
  , rarity:   2
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    2
  , stats:    { base:  { atk: 1116,  hp: 1447 }
              , max:   { atk: 6290, hp: 7688 }
              , grail: { atk: 9115, hp: 11095 }
              }
  , skills:   [ { name:   "Supporting Curse"
                , rank:   CPlus
                , icon:   IconSwordDown
                , cd:     7
                , effect: [ Debuff Enemy 2 AttackDown $ 5.0 ~ 15.0
                          , Debuff Enemy 2 DefenseDown $ 10.0 ~ 30.0
                          ]
                }
              , { name:   "Battle Continuation"
                , rank:   B
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 4 Guts $ 750.0 ~ 2000.0 ]
                }
              , { name:   "Half-Dead Bloodaxe"
                , rank:   APlus
                , icon:   IconBubbles
                , cd:     8
                , effect: [ To Self RemoveDebuffs Full
                          , Grant Self 3 MaxHP $ 1000.0 ~ 3000.0
                          ]
                }
              ]
  , passives: [madness B]
  , phantasm: { name:   "Bloodbath Crown"
              , desc:   "Bloodstained Coronation"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0
                        , To Self DemeritDamage $ Flat 1000.0
                        ]
              , over:   [ Grant Self 1 AttackUp $ 30.0 ~ 50.0 ]
              , first:  true
              }
  , gen:      { starWeight: 9, starRate: 4.9, npAtk: 1.02, npDef: 5 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, King]
  , death:    58.5
  , align:    [Chaotic, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 3]
              [Piece Berserker: 6, EvilBone: 11]
              [Monument Berserker: 3, SerpentJewel: 2, HomunculusBaby: 5]
              [Monument Berserker: 6, SerpentJewel: 4, Void'sDust: 12]
  , skillUp:  Reinforcement
              [GemOf Berserker: 3]
              [GemOf Berserker: 6, MagicGemOf Berserker: 3]
              [MagicGemOf Berserker: 3]
              [MagicGemOf Berserker: 6, HomunculusBaby: 3]
              [SecretGemOf Berserker: 3, HomunculusBaby: 5]
              [SecretGemOf Berserker: 6, EvilBone: 8]
              [ClawOfChaos: 2, EvilBone: 15]
              [ClawOfChaos: 6, Void'sDust: 24]
  }
, { name:     "Darius III"
  , id:       55
  , rarity:   3
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    3
  , stats:    { base:  { atk: 1412,  hp: 1577 }
              , max:   { atk: 7608, hp: 8763 }
              , grail: { atk: 10297, hp: 11881 }
              }
  , skills:   [ { name:   "Golden Rule"
                , rank:   B
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 18.0 ~ 45.0 ]
                }
              , { name:   "Disengage"
                , rank:   A
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              , { name:   "Battle Continuation"
                , rank:   A
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1000.0 ~ 2500.0 ]
                }
              ]
  , passives: [madness B]
  , phantasm: { name:   "Athanaton Ten Thousand"
              , desc:   "Ten Thousand Immortals"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   10
              , effect: [ To Enemies Damage $ 300.0 ~ 500.0 ]
              , over:   [ Debuff Enemies 3 AttackDown $ 10.0 ~ 30.0
                        , Debuff Enemies 3 DefenseDown $ 10.0 ~ 30.0
                        ]
              , first:  false
              }
  , gen:      { starWeight: 9, starRate: 5.0, npAtk: 0.67, npDef: 5 }
  , hits:     { quick: 2, arts: 3, buster: 1, ex: 3 }
  , traits:   [Male, EnumaElish, King]
  , death:    65.0
  , align:    [Lawful, Balanced]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 4]
              [Piece Berserker: 8, OctupletCrystals: 5]
              [Monument Berserker: 4, EvilBone: 10, PhoenixFeather: 6]
              [Monument Berserker: 8, EvilBone: 20, MeteorHorseshoe: 8]
  , skillUp:  Reinforcement
              [GemOf Berserker: 4]
              [GemOf Berserker: 8]
              [MagicGemOf Berserker: 4]
              [MagicGemOf Berserker: 8, PhoenixFeather: 3]
              [SecretGemOf Berserker: 4, PhoenixFeather: 6]
              [SecretGemOf Berserker: 8, OctupletCrystals: 4]
              [Void'sDust: 8, OctupletCrystals: 7]
              [Void'sDust: 24, MeteorHorseshoe: 16]
  }
, { name:     "Caligula"
  , id:       54
  , rarity:   2
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , curve:    12
  , stats:    { base:  { atk: 1374,  hp: 1211 }
              , max:   { atk: 6831, hp: 7303 }
              , grail: { atk: 9899, hp: 10540 }
              }
  , skills:   [ { name:   "Sadistic Streak"
                , rank:   A
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 10.0 ~ 30.0
                          , Debuff Self 3 DefenseDown $ Flat 10.0
                          ]
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
              , { name:   "Glory of Past Days"
                , rank:   B
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 (Performance Buster) $ 30.0 ~ 50.0
                          , To Self DemeritHealth $ Flat 500.0
                          ]
                }
              ]
  , passives: [madness APlus]
  , phantasm: { name:   "Flucticulus Diana"
              , desc:   "Moonlight, Devour my Soul"
              , rank:   C
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Chances 100 150 $ Debuff Enemies 3 SealSkills Full ]
              , over:   [ Chances 70 90 $ Debuff Enemies 3 SealNP Full ]
              , first:  false
              }
  , gen:      { starWeight: 9, starRate: 5.0, npAtk: 0.68, npDef: 5 }
  , hits:     { quick: 2, arts: 3, buster: 1, ex: 3 }
  , traits:   [Male, Roman, EnumaElish, King]
  , death:    56.8
  , align:    [Chaotic, Evil]
  , limited:  false
  , free:     false
  , ascendUp: Ascension
              [Piece Berserker: 3]
              [Piece Berserker: 6, OctupletCrystals: 4]
              [Monument Berserker: 3, GhostLantern: 3, SerpentJewel: 4]
              [Monument Berserker: 6, GhostLantern: 5, ForbiddenPage: 6]
  , skillUp:  Reinforcement
              [GemOf Berserker: 3]
              [GemOf Berserker: 6]
              [MagicGemOf Berserker: 3]
              [MagicGemOf Berserker: 6, SerpentJewel: 2]
              [SecretGemOf Berserker: 3, SerpentJewel: 4]
              [SecretGemOf Berserker: 6, OctupletCrystals: 3]
              [MeteorHorseshoe: 3, OctupletCrystals: 5]
              [MeteorHorseshoe: 9, ForbiddenPage: 12]
  }
]
