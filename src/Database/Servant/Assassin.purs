module Database.Servant.Assassin where

import Prelude
import Operators
import Database.Model

assassins ∷ Array Servant
assassins = Servant <$>
[ { name:     "Jack the Ripper"
  , rarity:   5
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1786,  hp: 1862 }
              , max:   { atk: 11557, hp: 12696 }
              , grail: { atk: 12651, hp: 13909 }
              }
  , ratings:  { damage:4, np:5, critical:5, utility:4, support:2, durability:3 }
  , actives:  [ { name:   "Murderer on a Misty Night A"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 1 QuickUp 50.0
                          ]
                }
              , { name:   "Information Erasure B"
                , icon:   IconCircuits
                , cd:     5
                , effect: [ To Enemy RemoveBuffs 0.0 
                          , Debuff Enemy 3 CritChance 30.0
                          ]
                }
              , { name:   "Surgery E"
                , icon:   IconHeal
                , cd:     4
                , effect: [ To Ally Heal 2500.0 ]
                }
              ]
  , passives: [presenceConcealment APlus]
  , phantasm: { name:   "Maria the Ripper"
              , desc:   "The Holy Mother of Dismemberment"
              , rank:   DPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   4
              , effect: [ To Enemy DamageThruDef 2200.0 ]
              , over:   [ Grant Self 1 (AttackUpVs Female) 50.0 ]
              }
  , gen:      { starWeight: 97, starRate: 25.5, npPerHit: 1.07, npPerDefend: 4 }
  , hits:     { a: 2, b: 2, q: 5, ex: 4 }
  , traits:   [Female, EnumaElish]
  , death:    44.0
  , align:    Chaotic:Evil
  , limited:  false
  }
, { name:     "Shuten-Douji"
  , rarity:   5
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1853,  hp: 1881 }
              , max:   { atk: 11993, hp: 12825 }
              , grail: { atk: 13128, hp: 14050 }
              }
  , ratings:  { damage:3, np:4, critical:3, utility:5, support:3, durability:3 }
  , actives:  [ { name:   "Intoxicating Aroma of Fruits A"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Chance 60 $ Debuff Enemies 1 Charm 0.0
                          , Debuff Enemies 3 DefenseDown 20.0
                          ]
                }
              , { name:   "Demonic Nature of Oni A"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 20.0 
                          , Grant Self 3 NPUp 30.0
                          ]
                }
              , { name:   "Battle Continuation A+"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              ]
  , passives: [presenceConcealment C, divinity C]
  , phantasm: { name:   "Multicolored Poison—Shinpen Kidoku"
              , desc:   "Senshibankou—Providential Oni Poison"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage 750.0 
                        , Debuff Enemies 3 DebuffVuln 10.0
                        , Debuff Enemies 3 AttackDown 10.0
                        , Debuff Enemies 3 NPDown 10.0
                        , Debuff Enemies 3 CritChance 10.0
                        , Debuff Enemies 1 SealSkills 0.0
                        ]
              , over:   [ Debuff Enemies 5 Poison 1000.0 ]
              }
  , gen:      { starWeight: 98, starRate: 25.0, npPerHit: 0.55, npPerDefend: 4 }
  , hits:     { a: 3, b: 1, q: 4, ex: 6 }
  , traits:   [Female, Demonic, EnumaElish, Divine, Dragon]
  , death:    31.6
  , align:    Chaotic:Evil
  , limited:  true
  }
, { name:     "Mysterious Heroine X"
  , rarity:   5
  , class:    Assassin
  , attr:     Star
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1817,  hp: 1862 }
              , max:   { atk: 11761, hp: 12696 }
              , grail: { atk: 12874, hp: 13909 }
              }
  , ratings:  { damage:4, np:5, critical:3, utility:2, support:1, durability:1 }
  , actives:  [ { name:   "Fire Support EX"
                , icon:   IconStun
                , cd:     8
                , effect: [ Chance 80 $ Debuff Enemies 1 StunBomb 0.0 ]
                }
              , { name:   "Intuition C+"
                , icon:   IconStar
                , cd:     5
                , effect: [ To Party GainStars 14.0 ]
                }
              , { name:   "Galactic Meteor Sword C"
                , icon:   IconStarHaloUp
                , cd:     6
                , effect: [ Grant Self 3 (DamageAffinity Saber) 50.0 
                          , Grant Self 3 (StarAffinity Saber) 100.0
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
              , effect: [ To Enemy Damage 2400.0 ]
              , over:   [ To (EnemyType Saberface) Damage 150.0 ]
              }
  , gen:      { starWeight: 98, starRate: 25.6, npPerHit: 0.81, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 4, ex: 4 }
  , traits:   [Female, Riding, Dragon, Saberface, Arthur]
  , death:    38.5
  , align:    Chaotic:Balanced
  , limited:  true
  }
, { name:     "Ryougi Shiki (Assassin)"
  , rarity:   4
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1477,  hp: 1768 }
              , max:   { atk: 8867, hp: 11055 }
              , grail: { atk: 10736, hp: 13404 }
              }
  , ratings:  { damage:5, np:5, critical:3, utility:2, support:2, durability:3 }
  , actives:  [ { name:   "Mystic Eyes of Death Perception A"
                , icon:   IconMystic
                , cd:     5
                , effect: [ Grant Self 1 IgnoreInvinc 0.0 
                          , Grant Self 1 ArtsUp 50.0
                          , Debuff Enemy 1 DeathDown 100.0
                          ]
                }
              , { name:   "Mind's Eye (Fake) A"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 CritUp 40.0
                          ]
                }
              , { name:   "Yin-Yang B"
                , icon:   IconYinYang
                , cd:     6
                , effect: [ To Self GaugeUp 30.0 
                          , To Self DemeritHealth 1000.0
                          ]
                }
              ]
  , passives: [presenceConcealment A, independentAction A]
  , phantasm: { name:   "Vijñāpti-mātratā: Mystic Eyes of Death Perception"
              , desc:   "Yuishiki—Chokushi no Magan"
              , rank:   EX
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   3
              , effect: [ To Enemy DamageThruDef 1500.0 ]
              , over:   [ To Enemy Kill 100.0 ]
              }
  , gen:      { starWeight: 102, starRate: 25.6, npPerHit: 0.8, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 4, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    44.0
  , align:    Chaotic:Good
  , limited:  true
  }
, { name:     "Carmilla"
  , rarity:   4
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1568,  hp: 1675 }
              , max:   { atk: 9408, hp: 10473 }
              , grail: { atk: 11391, hp: 12698 }
              }
  , ratings:  { damage:4, np:5, critical:3, utility:3, support:3, durability:3 }
  , actives:  [ { name:   "Vampirism C"
                , icon:   IconDarkMagic
                , cd:     6
                , effect: [ Chance 80 $ To Enemy GaugeDown 1.0
                          , To Self GaugeUp 27.0
                          ]
                }
              , { name:   "Torture Technique A"
                , icon:   IconShieldDown
                , cd:     5
                , effect: [ Debuff Enemy 3 DefenseDown 20.0 ]
                }
              , { name:   "Bath of Fresh Blood A"
                , icon:   IconExclamationDown
                , cd:     6
                , effect: [ Debuff Enemy 3 CritChance 50.0
                          , Grant Self 3 StarsPerTurn 10.0
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
              , effect: [ To Enemy Damage 1000.0 
                        , To Self Heal 2000.0
                        , Grant Self 3 AttackUp 20.0
                        ]
              , over:   [ To (EnemyType Female) Damage 120.0 ]
              }
  , gen:      { starWeight: 98, starRate: 25.2, npPerHit: 21.5, npPerDefend: 4 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    44.0
  , align:    Chaotic:Evil
  , limited:  true
  }
, { name:     "EMIYA (Assassin)"
  , rarity:   4
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1493,  hp: 1786 }
              , max:   { atk: 8958, hp: 11168 }
              , grail: { atk: 10846, hp: 13541 }
              }
  , ratings:  { damage:4, np:3, critical:4, utility:3, support:2, durability:3 }
  , actives:  [ { name:   "Magecraft B"
                , icon:   IconArtsUp
                , cd:     5
                , effect: [ Grant Self 1 ArtsUp 40.0 ]
                }
              , { name:   "Affection of the Holy Grail A+"
                , icon:   IconShieldBreak
                , cd:     5
                , effect: [ Grant Self 3 IgnoreInvinc 0.0 
                          , Grant Self 3 CritUp 50.0
                          , Debuff Others 3 DebuffVuln 20.0
                          ]
                }
              , { name:   "Spacegoat C"
                , icon:   IconCrosshairUp
                , cd:     5
                , effect: [ Grant Ally 1 Taunt 0.0 
                          , To Party GainStars 15.0
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
              , effect: [ To Enemy Damage 1500.0 
                        , To Enemy GaugeDown 1.0
                        ]
              , over:   [ Debuff Enemy 3 CritChance 10.0 ]
              }
  , gen:      { starWeight: 97, starRate: 25.6, npPerHit: 0.46, npPerDefend: 4 }
  , hits:     { a: 2, b: 6, q: 4, ex: 8 }
  , traits:   [Male, Brynhildr, EnumaElish]
  , death:    44.0
  , align:    Lawful:Evil
  , limited:  false
  }
, { name:     "Scathach (Assassin)"
  , rarity:   4
  , class:    Assassin
  , attr:     Star
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1508,  hp: 1786 }
              , max:   { atk: 9049, hp: 11168 }
              , grail: { atk: 10956, hp: 13541 }
              }
  , ratings:  { damage:4, np:3, critical:4, utility:3, support:3, durability:3 }
  , actives:  [ { name:   "Beach Crisis A+"
                , icon:   IconCrosshairUp
                , cd:     6
                , effect: [ Grant Self 1 Taunt 0.0 
                          , Grant Self 1 CritUp 50.0
                          ]
                }
              , { name:   "Primordial Rune (Sea) A"
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Ally Heal 3000.0 
                          , Grant Ally 1 ReduceDamage 1000.0
                          ]
                }
              , { name:   "Midsummer Mistake C"
                , icon:   IconShieldBreak
                , cd:     6
                , effect: [ Grant Self 1 IgnoreInvinc 0.0 
                          , Grant Self 1 QuickUp 50.0
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
              , effect: [ To Enemies Damage 1000.0 ]
              , over:   [ To Enemies Kill 30.0 ]
              }
  , gen:      { starWeight: 98, starRate: 25.6, npPerHit: 0.71, npPerDefend: 4 }
  , hits:     { a: 3, b: 3, q: 3, ex: 5 }
  , traits:   [Female, King, EnumaElish]
  , death:    44.0
  , align:    Neutral:Good
  , limited:  true
  }
, { name:     "Stheno"
  , rarity:   4
  , class:    Assassin
  , attr:     Heaven
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1497,  hp: 1843 }
              , max:   { atk: 8985, hp: 11518 }
              , grail: { atk: 10879, hp: 13965 }
              }
  , ratings:  { damage:1, np:5, critical:3, utility:3, support:3, durability:2 }
  , actives:  [ { name:   "Vampirism C"
                , icon:   IconDarkMagic
                , cd:     7
                , effect: [ Chance 80 $ To Enemy GaugeDown 1.0
                          , To Self GaugeUp 27.0
                          ]
                }
              , { name:   "Siren Song A"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Chance 100 $ Debuff (EnemyType Male) 1 Charm 0.0 ] 
                }
              , { name:   "Whim of the Goddess A (Stheno)"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 20.0 
                          , Grant (AlliesType Divine) 3 AttackUp 20.0
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
              , effect: [ To (EnemyType Male) Kill 150.0 
                        , Debuff Enemy 3 DefenseDown 20.0
                        ] 
              , over:   [ Chance 100 $ Debuff (EnemyType Male) 1 Charm 0.0 ]
              }
  , gen:      { starWeight: 104, starRate: 25.0, npPerHit: 2.26, npPerDefend: 4 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Divine, EnumaElish]
  , death:    27.5
  , align:    Chaotic:Good
  , limited:  false
  }
, { name:     "Fuuma Kotarou"
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1316,  hp: 1592 }
              , max:   { atk: 7091, hp: 8844 }
              , grail: { atk: 9597, hp: 11991 }
              }
  , ratings:  { damage:2, np:4, critical:5, utility:4, support:3, durability:3 }
  , actives:  [ { name:   "Sabotage B+"
                , icon:   IconSwordDown
                , cd:     5
                , effect: [ Debuff Enemies 3 AttackDown 10.0 
                          , Debuff Enemies 3 CritChance 20.0
                          ]
                }
              , { name:   "Ninjutsu A+++"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Ally 1 Evasion 0.0 
                          , Grant Ally 1 StarUp 50.0
                          ]
                }
              , { name:   "Suspicious Shadow C"
                , icon:   IconHoodDown
                , cd:     5
                , effect: [ Debuff Enemies 1 DebuffVuln 100.0 ]
                }
              ]
  , passives: [presenceConcealment APlus]
  , phantasm: { name:   "Immortal Chaos Brigade"
              , desc:   "Undying Chaotic Brigade"
              , rank:   B
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage 1000.0 ]
              , over:   [ Debuff Enemies 5 Disorder 30.0 ]
              }
  , gen:      { starWeight: 100, starRate: 25.6, npPerHit: 0.54, npPerDefend: 4 }
  , hits:     { a: 4, b: 1, q: 4, ex: 4 }
  , traits:   [Male, EnumaElish]
  , death:    38.5
  , align:    Chaotic:Evil
  , limited:  false
  }
, { name:     "Sasaki Kojiro"
  , rarity:   1
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1042,  hp: 1244 }
              , max:   { atk: 5735, hp: 6220 }
              , grail: { atk: 8912, hp: 9588 }
              }
  , ratings:  { damage:3, np:3, critical:4, utility:4, support:2, durability:3 }
  , actives:  [ { name:   "Mind's Eye (Fake) A"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 CritUp 40.0
                          ]
                }
              , { name:   "Vitrify B+"
                , icon:   IconBubbles
                , cd:     4
                , effect: [ To Self RemoveMental 0.0 
                          , To Party GainStars 15.0
                          ]
                }
              , { name:   "Knowledge of the Sowa B"
                , icon:   IconBullseye
                , cd:     5
                , effect: [ Grant Self 3 SureHit 0.0 
                          , Grant Self 3 StarUp 30.0
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
              , effect: [ To Enemy Damage 2000.0 ]
              , over:   [ To Party GainStars 15.0 ]
              }
  , gen:      { starWeight: 102, starRate: 25.3, npPerHit: 1.05, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    55.0
  , align:    Neutral:Evil
  , limited:  false
  }
, { name:     "Hassan of the Cursed Arm"
  , rarity:   2
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1114,  hp: 1429 }
              , max:   { atk: 6280, hp: 7594 }
              , grail: { atk: 9100, hp: 10960 }
              }
  , ratings:  { damage:3, np:4, critical:5, utility:2, support:2, durability:4 }
  , actives:  [ { name:   "Throw (Dagger) B"
                , icon:   IconStar
                , cd:     4
                , effect: [ To Party GainStars 12.0 ]
                }
              , { name:   "Self-Modification C"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 3 CritUp 50.0 ]
                }
              , { name:   "Protection Against the Wind A"
                , icon:   IconDodge
                , cd:     5
                , effect: [ Grant Self 0 Evasion 3.0
                          , Grant Self 3 StarUp 30.0
                          ]
                }
              ]
  , passives: [presenceConcealment APlus]
  , phantasm: { name:   "Zabaniya"
              , desc:   "Delusiona Heartbeat"
              , rank:   Unknown
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage 2000.0 ]
              , over:   [ To Enemy Kill 80.0 ]
              }
  , gen:      { starWeight: 97, starRate: 25.2, npPerHit: 1.07, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 3, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    44.0
  , align:    Lawful:Evil
  , limited:  false
  }
, { name:     "Henry Jekyll & Hyde"
  , rarity:   3
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1173,  hp: 1741 }
              , max:   { atk: 6320, hp: 9675 }
              , grail: { atk: 8553, hp: 13118 }
              }
  , ratings:  { damage:4, np:2, critical:3, utility:3, support:1, durability:3 }
  , actives:  [ { name:   "Monstrous Strength B (Jekyll & Hyde)"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 15.0 
                          , When "transformed into Hyde" 
                            $ Grant Self 3 AttackUp 35.0
                          ]
                }
              , { name:   "Panicky Voice A"
                , icon:   IconStun
                , cd:     6
                , effect: [ Grant Self 0 StunSuccess 25.0 
                          , When "transformed into Hyde" 
                            $ Grant Self 0 StunSuccess 135.0
                          , Chance 10 $ Debuff Enemy 1 Stun 0.0
                          ]
                }
              , { name:   "Self-Modification D"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 3 CritUp 15.0 
                          , When "transformed into Hyde" 
                            $ Grant Self 3 CritUp 35.0
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
              , effect: [ To Self (ChangeClass Berserker) 0.0
                        , Grant Self 0 MaxHP 6000.0
                        , To Self HealToFull 0.0
                        ]
              , over:   [ Grant Self 0 BusterUp 40.0 ] 
              }
  , gen:      { starWeight: 99, starRate: 25.6, npPerHit: 1.05, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    55.0
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "Jing Ke"
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1338,  hp: 1492 }
              , max:   { atk: 7207, hp: 8293 }
              , grail: { atk: 9754, hp: 11244 }
              }
  , ratings:  { damage:3, np:4, critical:4, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Restrain A"
                , icon:   IconStarUp
                , cd:     5
                , effect: [ Grant Self 3 StarAbsorb 200.0 
                          , To Party GainStars 15.0
                          ]
                }
              , { name:   "Planning B"
                , icon:   IconStarHaloUp
                , cd:     5
                , effect: [ Grant Self 3 StarUp 30.0 ]
                }
              , { name:   "Insolent A"
                , icon:   IconQuickUp
                , cd:     5
                , effect: [ Grant Self 1 QuickUp 30.0 
                          , Grant Self 1 CritUp 50.0
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
              , effect: [ To Enemy Damage 2000.0 
                        , To Self DemeritDamage 1000.0
                        ]
              , over:   [ To Enemy Kill 50.0 
                        , To Party GainStars 15.0
                        ]
              }
  , gen:      { starWeight: 98, starRate: 25.2, npPerHit: 1.05, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    55.0
  , align:    Chaotic:Good
  , limited:  false
  }
, { name:     "Charles-Henri Sanson"
  , rarity:   2
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 968,  hp: 1564 }
              , max:   { atk: 5456, hp: 8309 }
              , grail: { atk: 7906, hp: 11991 }
              }
  , ratings:  { damage:3, np:4, critical:3, utility:4, support:3, durability:2 }
  , actives:  [ { name:   "Executioner A++"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 3 (AlignAffinity Evil) 60.0 ]
                }
              , { name:   "Medicine A+"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Ally Heal 3000.0 
                          , To Ally RemoveDebuffs 0.0
                          ]
                }
              , { name:   "Human Study B"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 3 (AttackUpVs Human) 60.0 ]
                }
              ]
  , passives: [presenceConcealment D]
  , phantasm: { name:   "La Mort Espoir"
              , desc:   "Death is Hope For Tomorrow"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ To Enemy Kill 30.0 
                        , Debuff Enemy 3 DefenseDown 20.0
                        ]
              }
  , gen:      { starWeight: 102, starRate: 24.8, npPerHit: 1.06, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    49.5
  , align:    Lawful:Evil
  , limited:  false
  }
, { name:     "Hassan of the Hundred Personas"
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1241,  hp: 1675 }
              , max:   { atk: 6686, hp: 9310 }
              , grail: { atk: 9049, hp: 12623 }
              }
  , ratings:  { damage:0, np:0, critical:0, utility:0, support:0, durability:0 }
  , actives:  [ { name:   "Librarian of Knowledge C"
                , icon:   IconNobleUp
                , cd:     5
                , effect: [ Grant Self 3 NPGen 20.0 
                          , Grant Self 3 StarUp 40.0
                          ]
                }
              , { name:   "Wide Specialization A+"
                , icon:   IconAllUp
                , cd:     5
                , effect: [ Chance 80 $ Grant Self 3 BusterUp 30.0 
                          , Chance 80 $ Grant Self 3 QuickUp 30.0
                          , Chance 80 $ Grant Self 3 ArtsUp 30.0
                          , Grant Self 1 Evasion 0.0
                          ]
                }
              , { name:   "Battle Retreat B"
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Self Heal 4000.0 
                          , To Self DemeritBuffs 0.0
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
              , effect: [ To Enemy Damage 1500.0 ]
              , over:   [ Debuff Enemy 3 CritChance 10.0 ]
              }
  , gen:      { starWeight: 97, starRate: 25.5, npPerHit: 0.38, npPerDefend: 4 }
  , hits:     { a: 3, b: 1, q: 3, ex: 6 }
  , traits:   [Female, EnumaElish]
  , death:    44.0
  , align:    Chaotic:Evil
  , limited:  false
  }
, { name:     "Hassan of the Serenity"
  , rarity:   3
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1232,  hp: 1675 }
              , max:   { atk: 6636, hp: 9310 }
              , grail: { atk: 8981, hp: 12623 }
              }
  , ratings:  { damage:2, np:3, critical:4, utility:4, support:3, durability:2 }
  , actives:  [ { name:   "Morph (Infiltration) C"
                , icon:   IconExclamationDown
                , cd:     7
                , effect: [ Debuff Enemy 3 CritChance 20.0 
                          , To Enemy GaugeDown 1.0
                          ]
                }
              , { name:   "Throw (Dagger) C"
                , icon:   IconStar
                , cd:     4
                , effect: [ To Party GainStars 12.0 ]
                }
              , { name:   "Dance of Silence B"
                , icon:   IconReaperUp
                , cd:     6
                , effect: [ Grant Self 3 KillUp 50.0 
                          , Grant Self 3 DebuffSuccess 50.0
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
              , effect: [ Debuff Enemy 5 Poison 1000.0 
                        , Chance 40 $ Debuff Enemy 1 SealSkills 0.0
                        , Chance 40 $ Debuff Enemy 1 SealNP 0.0
                        , To Enemy Damage 1500.0
                        ]
              , over:   [ To Enemy Kill 60.0 ]
              }
  , gen:      { starWeight: 102, starRate: 25.6, npPerHit: 0.53, npPerDefend: 4 }
  , hits:     { a: 3, b: 4, q: 3, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    44.0
  , align:    Lawful:Evil
  , limited:  false
  }
, { name:     "Phantom of the Opera"
  , rarity:   2
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1003,  hp: 1580 }
              , max:   { atk: 5654, hp: 8393 }
              , grail: { atk: 8193, hp: 12112 }
              }
  , ratings:  { damage:2, np:3, critical:4, utility:3, support:3, durability:2 }
  , actives:  [ { name:   "Innocent Monster D"
                , icon:   IconStarTurn
                , cd:     5
                , effect: [ Grant Self 3 StarsPerTurn 9.0 
                          , Debuff Self 3 DefenseDown 14.0
                          ]
                }
              , { name:   "Siren Song B"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Chance 90 $ Debuff (EnemyType Female) 1 Charm 0.0 ]
                }
              , { name:   "Mental Corruption A"
                , icon:   IconStaffUp
                , cd:     5
                , effect: [ Grant Self 3 MentalSuccess 25.0 
                          , Grant Self 3 MentalResist 100.0
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
              , effect: [ To Enemies DamageThruDef 900.0 ]
              , over:   [ Debuff Enemies 6 DebuffVuln 50.0 ]
              }
  , gen:      { starWeight: 98, starRate: 25.2, npPerHit: 0.71, npPerDefend: 4 }
  , hits:     { a: 2, b: 2, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    49.5
  , align:    Chaotic:Evil
  , limited:  false
  }
, { name:     "Mata Hari"
  , rarity:   1
  , class:    Assassin
  , attr:     Mankind
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 977,  hp: 1313 }
              , max:   { atk: 5377, hp: 6565 }
              , grail: { atk: 8355, hp: 10120 }
              }
  , ratings:  { damage:1, np:5, critical:4, utility:4, support:4, durability:2 }
  , actives:  [ { name:   "Espionage A++"
                , icon:   IconStarHaloUp
                , cd:     5
                , effect: [ Grant Self 3 StarUp 30.0 ]
                }
              , { name:   "Pheromone B"
                , icon:   IconHeart
                , cd:     6
                , effect: [ Chance 60 $ Debuff (EnemiesType Male) 1 Charm 0.0 
                          , Debuff (EnemiesType Male) 3 DefenseDown 20.0
                          ]
                }
              , { name:   "Double-Cross B"
                , icon:   IconCircuits
                , cd:     6
                , effect: [ Debuff Enemy 1 SealSkills 0.0 
                          , Debuff Enemy 3 DefenseDown 20.0
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
              , effect: [ Chance 60 $ Debuff Enemies 1 Charm 0.0 ]
              , over:   [ Debuff Enemies 1 AttackDown 20.0 
                        , Debuff Enemies 1 DefenseDown 20.0
                        ]
              }
  , gen:      { starWeight: 98, starRate: 24.6, npPerHit: 2.1, npPerDefend: 4 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    55.0
  , align:    Chaotic:Balanced
  , limited:  false
  }

]
