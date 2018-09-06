module Database.Servant.Assassin (assassins) where

import Prelude
import Operators
import Database.Model

assassins ∷ Array Servant
assassins = Servant <$>
[ { name:     "Jack the Ripper"
  , id:       75
  , rarity:   5
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1786,  hp: 1862 }
              , max:   { atk: 11557, hp: 12696 }
              , grail: { atk: 12651, hp: 13909 }
              }
  , actives:  [ { name:   "Murderer on a Misty Night A"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 1 (Performance Quick) $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Information Erasure B"
                , icon:   IconCircuits
                , cd:     7
                , effect: [ To Enemy RemoveBuffs Full
                          , Debuff Enemy 3 CritChance $ 10.0 ~ 30.0
                          ]
                }
              , { name:   "Surgery E"
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Ally Heal $ 500.0 ~ 2500.0 ]
                }
              ]
  , passives: [presenceConcealment APlus]
  , phantasm: { name:   "Maria the Ripper"
              , desc:   "The Holy Mother of Dismemberment"
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Shuten-Douji"
  , id:       112
  , rarity:   5
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1853,  hp: 1881 }
              , max:   { atk: 11993, hp: 12825 }
              , grail: { atk: 13128, hp: 14050 }
              }
  , actives:  [ { name:   "Intoxicating Aroma of Fruits A"
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chance 60 $ Debuff Enemies 1 Charm Full
                          , Debuff Enemies 3 DefenseDown $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Demonic Nature of Oni A"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.0 ~ 20.0
                          , Grant Self 3 NPUp $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Battle Continuation A+"
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ 1200.0 ~ 2700.0 ]
                }
              ]
  , passives: [presenceConcealment C, divinity C]
  , phantasm: { name:   "Multicolored Poison—Shinpen Kidoku"
              , desc:   "Senshibankou—Providential Oni Poison"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage $ 450.0 ~ 750.0
                        , Debuff Enemies 3 DebuffVuln $ Flat 10.0
                        , Debuff Enemies 3 AttackDown $ Flat 10.0
                        , Debuff Enemies 3 NPDown $ Flat 10.0
                        , Debuff Enemies 3 CritChance $ Flat 10.0
                        , Debuff Enemies 1 SealSkills Full
                        ]
              , over:   [ Debuff Enemies 5 Poison $ 1000.0 ~ 5000.0 ]
              , first:  false
              }
  , gen:      { starWeight: 98, starRate: 25.0, npAtk: 0.55, npDef: 4 }
  , hits:     { quick: 4, arts: 3, buster: 1, ex: 6 }
  , traits:   [Female, Demonic, EnumaElish, Divine, Dragon]
  , death:    31.6
  , align:    Chaotic:Evil
  , limited:  true
  , free:     false
  }
, { name:     "Mysterious Heroine X"
  , id:       86
  , rarity:   5
  , class:    Assassin
  , attr:     Star
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1817,  hp: 1862 }
              , max:   { atk: 11761, hp: 12696 }
              , grail: { atk: 12874, hp: 13909 }
              }
  , actives:  [ { name:   "Fire Support EX"
                , icon:   IconStun
                , cd:     10
                , effect: [ Chances 60 80 $ Debuff Enemies 1 StunBomb Full ]
                }
              , { name:   "Intuition C+"
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Galactic Meteor Sword C"
                , icon:   IconStarHaloUp
                , cd:     8
                , effect: [ Grant Self 3 (ClassAffinity Saber) $ 30.0 ~ 50.0
                          , Grant Self 3 (StarAffinity Saber) $ 50.0 ~ 100.0
                          ]
                }
              ]
  , passives: [riding EX, cosmoReactor A]
  , phantasm: { name:   "Secret Calibur"
              , desc:   "Sword of Unsigned Victory"
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
  , align:    Chaotic:Good
  , limited:  true
  , free:     false
  }
, { name:     "Ryougi Shiki (Assassin)"
  , id:       92
  , rarity:   4
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1477,  hp: 1768 }
              , max:   { atk: 8867, hp: 11055 }
              , grail: { atk: 10736, hp: 13404 }
              }
  , actives:  [ { name:   "Mystic Eyes of Death Perception A"
                , icon:   IconMystic
                , cd:     7
                , effect: [ Grant Self 1 IgnoreInvinc Full
                          , Grant Self 1 (Performance Arts) $ 30.0 ~ 50.0
                          , Debuff Enemy 1 DeathDown $ 80.0 ~ 100.0
                          ]
                }
              , { name:   "Mind's Eye (Fake) A"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Yin-Yang B"
                , icon:   IconYinYang
                , cd:     8
                , effect: [ To Self GaugeUp $ 20.0 ~ 30.0
                          , To Self DemeritHealth $ Flat 1000.0
                          ]
                }
              ]
  , passives: [presenceConcealment C, independentAction A]
  , phantasm: { name:   "Vijñāpti-mātratā: Mystic Eyes of Death Perception"
              , desc:   "Yuishiki—Chokushi no Magan"
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
  , align:    Chaotic:Good
  , limited:  true
  , free:     true
  }
, { name:     "Carmilla"
  , id:       46
  , rarity:   4
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1568,  hp: 1675 }
              , max:   { atk: 9408, hp: 10473 }
              , grail: { atk: 11391, hp: 12698 }
              }
  , actives:  [ { name:   "Vampirism C"
                , icon:   IconDarkMagic
                , cd:     8
                , effect: [ Chances 60 80 <<< To Enemy GaugeDown $ Flat 1.0
                          , To Self GaugeUp $ 18.0 ~ 27.0
                          ]
                }
              , { name:   "Torture Technique A"
                , icon:   IconShieldDown
                , cd:     7
                , effect: [ Debuff Enemy 3 DefenseDown $ 10.0 ~ 20.0 ]
                }
              , { name:   "Bath of Fresh Blood A"
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Emiya (Assassin)"
  , id:       109
  , rarity:   4
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1493,  hp: 1786 }
              , max:   { atk: 8958, hp: 11168 }
              , grail: { atk: 10846, hp: 13541 }
              }
  , actives:  [ { name:   "Magecraft B"
                , icon:   IconArtsUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Arts) $ 24.0 ~ 40.0 ]
                }
              , { name:   "Affection of the Holy Grail A+"
                , icon:   IconShieldBreak
                , cd:     7
                , effect: [ Grant Self 3 IgnoreInvinc Full
                          , Grant Self 3 CritUp $ 30.0 ~ 50.0
                          , Debuff Others 3 DebuffVuln $ Flat 20.0
                          ]
                }
              , { name:   "Scapegoat C"
                , icon:   IconCrosshairUp
                , cd:     7
                , effect: [ Grant Ally 1 Taunt Full
                          , To Party GainStars $ 5.0 ~ 15.0
                          ]
                }
              ]
  , passives: [presenceConcealment APlus, independentAction A]
  , phantasm: { name:   "Chronos Rose"
              , desc:   "Pick Ye Rosebuds While Ye May"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   15
              , effect: [ To Enemy Damage $ 900.0 ~ 1500.0
                        , To Enemy GaugeDown Full
                        ]
              , over:   [ Debuff Enemy 3 CritChance $ 10.0 ~ 50.0 ]
              , first:  false
              }
  , gen:      { starWeight: 97, starRate: 25.6, npAtk: 0.46, npDef: 4 }
  , hits:     { quick: 4, arts: 2, buster: 6, ex: 8 }
  , traits:   [Male, Brynhild, EnumaElish]
  , death:    44.0
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Scathach (Assassin)"
  , id:       133
  , rarity:   4
  , class:    Assassin
  , attr:     Star
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1851,  hp: 1786 }
              , max:   { atk: 9049, hp: 11168 }
              , grail: { atk: 10956, hp: 13541 }
              }
  , actives:  [ { name:   "Beach Crisis A+"
                , icon:   IconCrosshairUp
                , cd:     8
                , effect: [ Grant Self 1 Taunt Full
                          , Grant Self 1 CritUp $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Primordial Rune (Sea) A"
                , icon:   IconHeal
                , cd:     8
                , effect: [ To Ally Heal $ 1000.0 ~ 3000.0
                          , Grant Ally 1 DamageDown $ 500.0 ~ 1000.0
                          ]
                }
              , { name:   "Midsummer Mistake C"
                , icon:   IconShieldBreak
                , cd:     8
                , effect: [ Grant Self 1 IgnoreInvinc Full
                          , Grant Self 1 (Performance Quick) $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [presenceConcealment E]
  , phantasm: { name:   "Gae Bolg Alternative"
              , desc:   "Soaring Spear of Kick-Piercing Death"
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
  , align:    Neutral:Good
  , limited:  true
  , free:     true
  }
, { name:     "Stheno"
  , id:       41
  , rarity:   4
  , class:    Assassin
  , attr:     Heaven
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1497,  hp: 1843 }
              , max:   { atk: 8985, hp: 11518 }
              , grail: { atk: 10879, hp: 13965 }
              }
  , actives:  [ { name:   "Vampirism C"
                , icon:   IconDarkMagic
                , cd:     9
                , effect: [ Chances 60 80 <<< To Enemy GaugeDown $ Flat 1.0
                          , To Self GaugeUp $ 18.0 ~ 27.0
                          ]
                }
              , { name:   "Siren Song A"
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 70 100
                            $ Debuff (EnemyType Male) 1 Charm Full ]
                }
              , { name:   "Whim of the Goddess A"
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
              , over:   [ Chances 100 200
                          $ Debuff (EnemyType Male) 1 Charm Full ]
              , first:  false
              }
  , gen:      { starWeight: 104, starRate: 25.0, npAtk: 2.26, npDef: 4 }
  , hits:     { quick: 2, arts: 1, buster: 1, ex: 3 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    27.5
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Fuuma \"Evil-wind\" Kotarou"
  , id:       117
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1316,  hp: 1592 }
              , max:   { atk: 7091, hp: 8844 }
              , grail: { atk: 9597, hp: 11991 }
              }
  , actives:  [ { name:   "Sabotage B+"
                , icon:   IconSwordDown
                , cd:     7
                , effect: [ Debuff Enemies 3 AttackDown $ Flat 10.0
                          , Debuff Enemies 3 CritChance $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Ninjutsu A<>+"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Ally 1 Evasion Full
                          , Grant Ally 1 StarUp $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Suspicious Shadow C"
                , icon:   IconHoodDown
                , cd:     7
                , effect: [ Debuff Enemies 1 DebuffVuln $ 50.0 ~ 100.0 ]
                }
              ]
  , passives: [presenceConcealment APlus]
  , phantasm: { name:   "Immortal Chaos Brigade"
              , desc:   "Undying Chaotic Brigade"
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Sasaki Kojirou"
  , id:       39
  , rarity:   1
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1042,  hp: 1244 }
              , max:   { atk: 5735, hp: 6220 }
              , grail: { atk: 8912, hp: 9588 }
              }
  , actives:  [ { name:   "Mind's Eye (Fake) A"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Vitrify B+"
                , icon:   IconBubbles
                , cd:     6
                , effect: [ To Self RemoveMental Full
                          , To Party GainStars $ 5.0 ~ 15.0
                          ]
                }
              , { name:   "Knowledge of the Sowa B"
                , icon:   IconBullseye
                , cd:     7
                , effect: [ Grant Self 3 SureHit Full
                          , Grant Self 3 StarUp $ 10.0 ~ 30.0
                          ]
                }
              ]
  , passives: [presenceConcealment D]
  , phantasm: { name:   "Swallow Reversal"
              , desc:   "Hidden Blade"
              , rank:   Unknown
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   3
              , effect: [ To Enemy Damage $ 1200.0 ~ 2000.0 ]
              , over:   [ To Party GainStars $ 15.0 ~ 35.0 ]
              , first:  false
              }
  , gen:      { starWeight: 102, starRate: 25.3, npAtk: 1.05, npDef: 4 }
  , hits:     { quick: 2, arts: 2, buster: 1, ex: 4 }
  , traits:   [Male, EnumaElish]
  , death:    55.0
  , align:    Neutral:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Hassan of the Cursed Arm"
  , id:       40
  , rarity:   2
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1114,  hp: 1429 }
              , max:   { atk: 6280, hp: 7594 }
              , grail: { atk: 9100, hp: 10960 }
              }
  , actives:  [ { name:   "Throw (Dagger) B"
                , icon:   IconStar
                , cd:     6
                , effect: [ To Party GainStars $ 3.0 ~ 12.0 ]
                }
              , { name:   "Self-Modification C"
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 3 CritUp $ 20.0 ~ 50.0 ]
                }
              , { name:   "Protection Against the Wind A"
                , icon:   IconDodge
                , cd:     7
                , effect: [ Times 3 $ Grant Self 0 Evasion Full
                          , Grant Self 3 StarUp $ 10.0 ~ 30.0
                          ]
                }
              ]
  , passives: [presenceConcealment APlus]
  , phantasm: { name:   "Zabaniya"
              , desc:   "Delusiona Heartbeat"
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
  , align:    Lawful:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Henry Jekyll & Hyde"
  , id:       81
  , rarity:   3
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1173,  hp: 1741 }
              , max:   { atk: 6320, hp: 9675 }
              , grail: { atk: 8553, hp: 13118 }
              }
  , actives:  [ { name:   "Monstrous Strength B (Jekyll & Hyde)"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 5.0 ~ 15.0
                          , When "transformed into Hyde"
                            <<< Grant Self 3 AttackUp $ 25.0 ~ 35.0
                          ]
                }
              , { name:   "Panicky Voice A"
                , icon:   IconStun
                , cd:     8
                , effect: [ Times 1
                            <<< Grant Self 0 (Success Stun) $ 5.0  ~ 15.0
                          , When "transformed into Hyde" <<< Times 1
                            <<< Grant Self 0 (Success Stun) $ 85.0 ~ 135.0
                          , Chance 10 $ Debuff Enemy 1 Stun Full
                          ]
                }
              , { name:   "Self-Modification D"
                , icon:   IconExclamationUp
                , cd:     7
                , effect: [ Grant Self 3 CritUp $ 5.0 ~ 15.0
                          , When "transformed into Hyde"
                            <<< Grant Self 3 CritUp $ 25.0 ~ 35.0
                          ]
                }
              ]
  , passives: [presenceConcealment A, madness A]
  , phantasm: { name:   "Dangerous Game C"
              , desc:   "Game of Silent Crime"
              , rank:   CPlus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   0
              , effect: [ To Self (ChangeClass Berserker) Full
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
  , align:    Lawful:Good
  , limited:  false
  , free:     true
  }
, { name:     "Jing Ke"
  , id:       42
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1338,  hp: 1492 }
              , max:   { atk: 7207, hp: 8293 }
              , grail: { atk: 9754, hp: 11244 }
              }
  , actives:  [ { name:   "Restrain A"
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 3 StarAbsorb $ Flat 200.0
                          , To Party GainStars $ 5.0 ~ 15.0
                          ]
                }
              , { name:   "Planning B"
                , icon:   IconStarHaloUp
                , cd:     7
                , effect: [ Grant Self 3 StarUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Insolent A"
                , icon:   IconQuickUp
                , cd:     7
                , effect: [ Grant Self 1 (Performance Quick) $ 20.0 ~ 30.0
                          , Grant Self 1 CritUp $ 30.0 ~ 50.0
                          ]
                }
              ]
  , passives: [presenceConcealment B]
  , phantasm: { name:   "All I Do Is Kill"
              , desc:   "Unreturning Dagger"
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
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Charles-Henri Sanson"
  , id:       43
  , rarity:   2
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 968,  hp: 1568 }
              , max:   { atk: 5456, hp: 8309 }
              , grail: { atk: 7906, hp: 11991 }
              }
  , actives:  [ { name:   "Executioner A++"
                , icon:   IconDamageUp
                , cd:     7
                , effect: [ Grant Self 3 (AlignAffinity Evil) $ 40.0 ~ 60.0 ]
                }
              , { name:   "Medicine A+"
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Ally Heal $ 1000.0 ~ 3000.0
                          , To Ally RemoveDebuffs Full
                          ]
                }
              , { name:   "Human Study B"
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
  , align:    Lawful:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Hassan of the Hundred Personas"
  , id:       110
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1241,  hp: 1675 }
              , max:   { atk: 6686, hp: 9310 }
              , grail: { atk: 9049, hp: 12623 }
              }
  , actives:  [ { name:   "Librarian of Knowledge C"
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Self 3 NPGen $ 10.0 ~ 20.0
                          , Grant Self 3 StarUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Wide Specialization A+"
                , icon:   IconAllUp
                , cd:     7
                , effect: [ Chances 60 80
                            <<< Grant Self 3 (Performance Buster) $ Flat 30.0
                          , Chances 60 80
                            <<< Grant Self 3 (Performance Quick) $ Flat 30.0
                          , Chances 60 80
                            <<< Grant Self 3 (Performance Arts) $ Flat 30.0
                          , Grant Self 1 Evasion Full
                          ] -- TODO
                }
              , { name:   "Battle Retreat B"
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
  , align:    Lawful:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Hassan of the Serenity"
  , id:       124
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1232,  hp: 1675 }
              , max:   { atk: 6636, hp: 9310 }
              , grail: { atk: 8981, hp: 12623 }
              }
  , actives:  [ { name:   "Morph (Infiltration) C"
                , icon:   IconExclamationDown
                , cd:     9
                , effect: [ Debuff Enemy 3 CritChance $ 10.0 ~ 20.0
                          , To Enemy GaugeDown $ Flat 1.0
                          ]
                }
              , { name:   "Throw (Dagger) C"
                , icon:   IconStar
                , cd:     6
                , effect: [ To Party GainStars $ 2.0 ~ 12.0 ]
                }
              , { name:   "Dance of Silence B"
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
  , align:    Lawful:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Phantom of the Opera"
  , id:       44
  , rarity:   2
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1003,  hp: 1580 }
              , max:   { atk: 5654, hp: 8393 }
              , grail: { atk: 8193, hp: 12112 }
              }
  , actives:  [ { name:   "Innocent Monster D"
                , icon:   IconStarTurn
                , cd:     7
                , effect: [ Grant Party 3 StarsPerTurn $ 3.0 ~ 9.0
                          , Debuff Self 3 DefenseDown $ Flat 14.0
                          ]
                }
              , { name:   "Siren Song B"
                , icon:   IconHeart
                , cd:     9
                , effect: [ Chances 60 90
                            $ Debuff (EnemyType Female) 1 Charm Full ]
                }
              , { name:   "Mental Corruption A"
                , icon:   IconStaffUp
                , cd:     7
                , effect: [ Grant Self 3 MentalSuccess $ 5.0 ~ 25.0
                          , Grant Self 3 MentalResist $ 50.0 ~ 100.0
                          ]
                }
              ]
  , passives: [presenceConcealment A]
  , phantasm: { name:   "Christine Christine"
              , desc:   "My Love Song Shall Resound Through Hell"
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Mata Hari"
  , id:       45
  , rarity:   1
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 977,  hp: 1313 }
              , max:   { atk: 5377, hp: 6565 }
              , grail: { atk: 8355, hp: 10120 }
              }
  , actives:  [ { name:   "Espionage A++"
                , icon:   IconStarHaloUp
                , cd:     7
                , effect: [ Grant Self 3 StarUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Pheromone B"
                , icon:   IconHeart
                , cd:     8
                , effect: [ Chances 30 60
                            $ Debuff (EnemiesType Male) 1 Charm Full
                          , Debuff Enemies 3 DefenseDown $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Double-Cross B"
                , icon:   IconCircuits
                , cd:     8
                , effect: [ Debuff Enemy 1 SealSkills Full
                          , Debuff Enemy 3 DefenseDown $ 10.0 ~ 20.0
                          ]
                }
              ]
  , passives: []
  , phantasm: { name:   "Mata Hari"
              , desc:   "The Woman with Dazzling Eyes"
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
  , align:    Chaotic:Balanced
  , limited:  false
  , free:     false
  }
]
