module Database.Servant.Lancer where

import Prelude
import Operators
import Database.Model

lancers ∷ Array Servant
lancers = Servant <$>
[ { name:     "Scathach"
  , rarity:   5
  , class:    Lancer
  , attr:     Star
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1758,  hp: 2174 }
              , max:   { atk: 11375, hp: 14825 }
              , grail: { atk: 12452, hp: 16241 }
              }
  , ratings:  { damage:5, np:3, critical:4, utility:3, support:3, durability:4 }
  , actives:  [ { name:   "Wisdom of Dun Scaith A+"
                , icon:   IconDodge
                , cd:     5
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Chance 80 $ Grant Self 3 CritUp 50.0
                          , Chance 80 $ Grant Self 3 StarAbsorb 500.0
                          ]
                }
              , { name:   "Primordial Rune"
                , icon:   IconQuickUp
                , cd:     6
                , effect: [ Grant Ally 1 QuickUp 50.0 ]
                }
              , { name:   "God-Slayer B"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 1 (AttackUpVs Divine) 100.0 
                          , Grant Self 1 (AttackUpVs Undead)  100.0
                          ] 
                }
              ]
  , passives: [magicResistance A]
  , phantasm: { name:   "Gae Bolg Alternative"
              , desc:   "Soaring Spear of Piercing Death"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage 2400.0  
                        , Debuff Enemy 1 Stun 0.0
                        ]
              , over:   [ To Enemy Kill 50.0 ]
              }
  , gen:      { starWeight: 88, starRate: 12.2, npPerHit: 0.71, npPerDefend: 4 }
  , hits:     { a: 3, b: 6, q: 2, ex: 7 }
  , traits:   [Female, EnumaElish, King]
  , death:    32.0
  , align:    Neutral:Good
  }
, { name:     "Karna"
  , rarity:   5
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1850,  hp: 1999 }
              , max:   { atk: 11976, hp: 13632 }
              , grail: { atk: 13110, hp: 14934 }
              }
  , ratings:  { damage:4, np:4, critical:4, utility:4, support:3, durability:2 }
  , actives:  [ { name:   "Knowledge of the Deprived A"
                , icon:   IconCircuits
                , cd:     6
                , effect: [ Debuff Enemy 1 SealNP 0.0 
                          , Debuff Enemy 1 DebuffVuln 50.0
                          ]
                }
              , { name:   "Mana Burst (Flame) A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 30.0 
                          , Grant Self 1 NPUp 20.0
                          ]
                }
              , { name:   "Uncrowned Arms Mastery"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 25.0 
                          , Grant Self 3 StarUp 50.0
                          , Grant Self 3 CritUp 40.0
                          ]
                }
              ]
  , passives: [magicResistance C, divinity A, riding A]
  , phantasm: { name:   "Vasavi Shakti"
              , desc:   "O' Sun, Abide to Death"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Divine"
              , hits:   5
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ To (EnemiesType Divine) Damage 150.0 ]
              }
  , gen:      { starWeight: 88, starRate: 12.2, npPerHit: 0.72, npPerDefend: 4 }
  , hits:     { a: 3, b: 1, q: 3, ex: 4 }
  , traits:   [Male, Riding, Brynhildr, Divine, EnumaElish]
  , death:    28.0
  , align:    Lawful:Good
  }
, { name:     "Tamamo no Mae (Lancer)"
  , rarity:   5
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1657,  hp: 2221 }
              , max:   { atk: 10726, hp: 15147 }
              , grail: { atk: 11741, hp: 16594 }
              }
  , ratings:  { damage:4, np:5, critical:4, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Beach Flower EX"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 10.0 
                          , Grant (AlliesType Male) 3 StarUp 42.0
                          ]
                }
              , { name:   "Midsummer Curse A"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Debuff Enemy 1 Charm 0.0 
                          , Debuff Enemy 3 DefenseDown 30.0
                          , Debuff Enemy 5 Curse 1000.0
                          , To Enemy DemeritCharge 1.0
                          ]
                }
              , { name:   "Goddess Morph B"
                , icon:   IconShield
                , cd:     6
                , effect: [ Grant Self 1 Invincibility 0.0 
                          , Grant Self 1 CritUp 50.0
                          , Grant Self 1 StarUp 50.0
                          , Grant Self 1 NPGen 50.0
                          , Grant Self 1 DebuffResist 50.0
                          , Grant Self 1 HealingReceived 50.0
                          , Debuff Self 1 StunBomb 0.0
                          ]
                }
              ]
  , passives: [riding A, territoryCreation A, divinity APlusPlus]
  , phantasm: { name:   "Tokonatsu Nikkou—Goddess' Love Parasol"
              , desc:   "Everlasting Summer Sunlight—Hiyoke Kasa Chouai I-Shin"
              , rank:   C
              , card:   Buster
              , kind:   "Anti-Unit"
              , hits:   4
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ To (EnemyType Male) Damage 150.0 ]
              }
  , gen:      { starWeight: 91, starRate: 12.2, npPerHit: 1.05, npPerDefend: 4 }
  , hits:     { a: 2, b: 3, q: 4, ex: 4 }
  , traits:   [Female, Divine, Riding, EnumaElish]
  , death:    40.0
  , align:    Neutral:Summer
  }
, { name:     "Brynhild"
  , rarity:   5
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1766,  hp: 2174 }
              , max:   { atk: 11432, hp: 14825 }
              , grail: { atk: 12514, hp: 16241 }
              }
  , ratings:  { damage:4, np:4, critical:4, utility:3, support:4, durability:3 }
  , actives:  [ { name:   "Mana Burst (Flame) B"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 25.0 
                          , Grant Self 1 NPUp 15.0
                          ]
                }
              , { name:   "Primordial Rune"
                , icon:   IconExclamationDown
                , cd:     6
                , effect: [ Debuff Enemy 3 CritChance 50.0 
                          , Debuff Enemy 1 NPDown 30.0
                          ]
                }
              , { name:   "Hero's Assistant C"
                , icon:   IconStarHaloUp
                , cd:     5
                , effect: [ Grant Ally 3 StarAbsorb 600.0 
                          , To Ally Heal 1000.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding A, divinity E]
  , phantasm: { name:   "Brynhild Romantia"
              , desc:   "Until Death Divide the Two Apart"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemy Damage 1000.0
                        , Grant Allies 3 StarUp 30.0
                        ]
              , over:   [ To (EnemyType Brynhildr) Damage 150.0 ]
              }
  , gen:      { starWeight: 87, starRate: 12.2, npPerHit: 1.07, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 3, ex: 5 }
  , traits:   [Female, Riding, Divine, EnumaElish]
  , death:    32.0
  , align:    Neutral:Good
  }
, { name:     "Altria Pendragon (Lancer)"
  , rarity:   5
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1699,  hp: 2288 }
              , max:   { atk: 10995, hp: 15606 }
              , grail: { atk: 12036 , hp: 17097 }
              }
  , ratings:  { damage:3, np:5, critical:2, utility:3, support:2, durability:2 }
  , actives:  [ { name:   "Mana Burst A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 50.0 ]
                }
              , { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 18.0 ]
                }
              , { name:   "Protection of World's End EX"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0 
                          , To Self RemoveDebuffs 0.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding A]
  , phantasm: { name:   "Rhongomyniad"
              , desc:   "The Spear That Shines To the Ends of the World"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   2
              , effect: [ Grant Self 1 IgnoreInvinc 0.0 
                        , To Enemies Damage 500.0
                        ]
              , over:   [ To Self GaugeUp 20.0 ]
              }
  , gen:      { starWeight: 89, starRate: 12.2, npPerHit: 1.1, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 3, ex: 5 }
  , traits:   [Female, Arthur, Dragon, King, Riding, Saberface, EnumaElish]
  , death:    24.0
  , align:    Lawful:Good
  }
, { name:     "Altria Pendragon (Lancer Alter)"
  , rarity:   4
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1661,  hp: 1881 }
              , max:   { atk: 9968, hp: 11761 }
              , grail: { atk: 12069, hp: 14260 }
              }
  , ratings:  { damage:5, np:3, critical:5, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Mana Burst A+"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 55.0 ]
                }
              , { name:   "Protection of World's End A"
                , icon:   IconStarHaloUp
                , cd:     5
                , effect: [ Grant Self 1 StarAbsorb 1000.0 
                          , Grant Self 1 CritUp 50.0
                          , To Party GainStars 10.0
                          ]
                }
              , { name:   "Charisma E"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 12.0 ]
                }
              ]
  , passives: [magicResistance A, riding A]
  , phantasm: { name:   "Rhongomyniad"
              , desc:   "The Spear That Shines To the Ends of the World"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Fortress"
              , hits:   4
              , effect: [ To Enemies DamageThruDef 600.0 
                        , Chance 60 $ Debuff Enemies 1 SealNP 0.0
                        ]
              , over:   [ Debuff Enemies 5 Curse 1000.0 ]
              }
  , gen:      { starWeight: 88, starRate: 11.8, npPerHit: 0.74, npPerDefend: 4 }
  , hits:     { a: 3, b: 1, q: 2, ex: 4 }
  , traits:   [Female, Riding, Dragon, Saberface, Arthur, EnumaElish]
  , death:    23.0
  , align:    Lawful:Good
  }
, { name:     "Li Shuwen"
  , rarity:   4
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1608,  hp: 1817 }
              , max:   { atk: 9653, hp: 11360 }
              , grail: { atk: 11688, hp: 13774 }
              }
  , ratings:  { damage:5, np:3, critical:4, utility:3, support:1, durability:3 }
  , actives:  [ { name:   "Chinese Martial Arts (Liu He Da Qiang) A++"
                , icon:   IconBullseye
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0
                          , Grant Self 1 CritUp 100.0
                          ]
                }
              , { name:   "Sphere Boundary B"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 1 StarAbsorb 500.0
                          ]
                }
              , { name:   "Juezhao B"
                , icon:   IconShieldBreak
                , cd:     6
                , effect: [ Grant Self 1 IgnoreInvinc 0.0 
                          , Grant Self 1 ArtsUp 50.0
                          ]
                }
              ]
  , passives: [magicResistance D]
  , phantasm: { name:   "Shen Qiang Wu Er Da"
              , desc:   "God Spear, No Second Strike"
              , rank:   Unknown
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   3
              , effect: [ To Enemy DamageThruDef 1500.0 
                        , Debuff Enemy 3 DefenseDown 20.0
                        ]
              , over:   [ To Enemy Kill 40.0 ]
              }
  , gen:      { starWeight: 87, starRate: 12.2, npPerHit: 0.52, npPerDefend: 4 }
  , hits:     { a: 3, b: 1, q: 3, ex: 5 }
  , traits:   [Male, EnumaElish]
  , death:    40.0
  , align:    Neutral:Evil
  }
, { name:     "Kiyohime (Lancer)"
  , rarity:   4
  , class:    Lancer
  , attr:     Earth 
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1489,  hp: 1899 }
              , max:   { atk: 8936, hp: 11870 }
              , grail: { atk: 10820, hp: 14392 }
              }
  , ratings:  { damage:5, np:3, critical:2, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Passionate Summer A"
                , icon:   IconExclamationDown
                , cd:     5
                , effect: [ Debuff Enemies 3 CritChance 30.0 ]
                }
              , { name:   "Bath Transformation A"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 3 BusterUp 30.0 ]
                }
              , { name:   "Pursuer of Love A"
                , icon:   IconShieldDown
                , cd:     5
                , effect: [ Debuff Enemy 3 DefenseDown 30.0 
                          , Grant Enemy 3 AttackUp 20.0
                          ]
                }
              ]
  , passives: [madness EX, magicResistance D]
  , phantasm: { name:   "Dojo-ji Bell Form 108—Karyu-nagi"
              , desc:   "Doujou-ji Kane, Hyakuhachi-shiki—Fire Dragon Mower"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   6
              , effect: [ To Enemy Damage 1000.0 
                        , Chance 150 $ Debuff Enemy 1 SealSkills 0.0
                        ]
              , over:   [ Debuff Enemy 5 Burn 500.0 ]
              }
  , gen:      { starWeight: 92, starRate: 12.0, npPerHit: 1.05, npPerDefend: 4 }
  , hits:     { a: 2, b: 6, q: 2, ex: 5 }
  , traits:   [Female, Dragon, EnumaElish]
  , death:    40.0
  , align:    Chaotic:Evil
  }
, { name:     "Elisabeth Bathory"
  , rarity:   4
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1520,  hp: 1899 }
              , max:   { atk: 9122, hp: 11870 }
              , grail: { atk: 11045, hp: 14392 }
              }
  , ratings:  { damage:2, np:3, critical:2, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Charisma C"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 16.0 ]
                }
              , { name:   "Torture Technique A"
                , icon:   IconShieldDown
                , cd:     5
                , effect: [ Debuff Enemy 3 DefenseDown 20.0 ]
                }
              , { name:   "Battle Continuation B"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 4 Guts 1.0 ]
                }
              ]
  , passives: [magicResistance A, territoryCreation B]
  , phantasm: { name:   "Bathory Erzsebet"
              , desc:   "Demon Daughter of Fresh Blood"
              , rank:   EMinus
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   5
              , effect: [ To Enemies DamageThruDef 500.0 ]
              , over:   [ Debuff Enemies 3 Curse 500.0 ]
              }
  , gen:      { starWeight: 90, starRate: 11.0, npPerHit: 1.1, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Dragon, EnumaElish]
  , death:    24.0
  , align:    Chaotic:Evil
  }
, { name:     "Fionn mac Cumhaill"
  , rarity:   4
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1488,  hp: 2040 }
              , max:   { atk: 8930, hp: 12750 }
              , grail: { atk: 10812, hp: 15459 }
              }
  , ratings:  { damage:2, np:1, critical:1, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Clairvoyance B"
                , icon:   IconStarHaloUp
                , cd:     6
                , effect: [ Grant Self 3 StarUp 38.0 ]
                }
              , { name:   "Trouble with Women A"
                , icon:   IconDodge
                , cd:     10
                , effect: [ Chance 100 $ Grant Self 1 Evasion 0.0 
                          , Grant Self 1 Taunt 0.0
                          , Debuff Self 3 CharmVuln 80.0
                          ]
                }
              , { name:   "Magecraft B"
                , icon:   IconArtsUp
                , cd:     5
                , effect: [ Grant Self 1 ArtsUp 40.0 ]
                }
              ]
  , passives: [magicResistance B, divinity D]
  , phantasm: { name:   "Mac an Luin"
              , desc:   "Undefeated Violet Flower"
              , rank:   APlus
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage 900.0 
                        , Grant Self 3 DebuffImmunity 0.0
                        ]
              , over:   [ Debuff Enemies 3 AttackDown 10.0 ]
              }
  , gen:      { starWeight: 89, starRate: 12.3, npPerHit: 0.55, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 4 }
  , traits:   [Male, Divine, EnumaElish]
  , death:    32.0
  , align:    Neutral:Balanced
  }
, { name:     "Cu Chulainn"
  , rarity:   3
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1344,  hp: 1726 }
              , max:   { atk: 7239, hp: 9593 }
              , grail: { atk: 9797, hp: 13007 }
              }
  , ratings:  { damage:3, np:3, critical:2, utility:3, support:1, durability:5 }
  , actives:  [ { name:   "Battle Continuation A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              , { name:   "Protection from Arrows B"
                , icon:   IconDodge
                , cd:     5
                , effect: [ Grant Self 0 Evasion 3.0 
                          , Grant Self 3 DefenseUp 16.0
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
  , passives: [magicResistance C, divinity B]
  , phantasm: { name:   "Gae Bolg"
              , desc:   "Barbed Spear that Pierces with Death"
              , rank:   B
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage 2000.0 ]
              , over:   [ To Enemy Kill 50.0 
                        , Debuff Enemy 3 DefenseDown 10.0
                        ]
              }
  , gen:      { starWeight: 87, starRate: 12.1, npPerHit: 1.07, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Divine, Brynhildr, EnumaElish]
  , death:    32.0
  , align:    Lawful:Balanced
  }
, { name:     "Cu Chulainn (Prototype)"
  , rarity:   3
  , class:    Lancer
  , attr:     Heaven
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1315,  hp: 1817 }
              , max:   { atk: 7082, hp: 10098 }
              , grail: { atk: 9584, hp: 13691 }
              }
  , ratings:  { damage:3, np:3, critical:4, utility:4, support:1, durability:4 }
  , actives:  [ { name:   "Rune Spell B"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 3 CritUp 45.0 
                          , Grant Self 3 DebuffResist 45.0
                          ]
                }
              , { name:   "Protection from Arrows B"
                , icon:   IconDodge
                , cd:     5
                , effect: [ Grant Self 0 Evasion 3.0 
                          , Grant Self 3 DefenseUp 16.0
                          ]
                }
              , { name:   "Beast-Slayer B+"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 3 (AttackUpVs Beast) 60.0 ]
                }
              ]
  , passives: [magicResistance C, divinity B]
  , phantasm: { name:   "Gae Bolg"
              , desc:   "Barbed Spear that Pierces with Death"
              , rank:   B
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage 2000.0 ]
              , over:   [ To Enemy Kill 50.0 
                        , Debuff Enemy 3 DefenseDown 50.0
                        ]
              }
  , gen:      { starWeight: 88, starRate: 12.1, npPerHit: 1.08, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Divine, EnumaElish]
  , death:    28.0
  , align:    Lawful:Balanced
  }
, { name:     "Leonidas"
  , rarity:   2
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1168,  hp: 1498 }
              , max:   { atk: 6583, hp: 7959 }
              , grail: { atk: 9539, hp: 11486 }
              }
  , ratings:  { damage:2, np:5, critical:3, utility:4, support:3, durability:4 }
  , actives:  [ { name:   "Rear Guard's Pride A"
                , icon:   IconCrosshairUp
                , cd:     5
                , effect: [ Grant Self 1 Taunt 0.0 
                          , Grant Self 3 NPGen 100.0
                          ]
                }
              , { name:   "Battle Continuation A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              , { name:   "Warrior's War Cry B"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Party 3 BusterUp 25.0 ]
                }
              ]
  , passives: [magicResistance C]
  , phantasm: { name:   "Thermopylae Enomotia"
              , desc:   "Guardian of the Flaming Gates"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Personnel"
              , hits:   0
              , effect: [ Grant Self 3 Taunt 0.0 
                        , To Party GainStars 25.0
                        ]
              , over:   [ Grant Self 3 DefenseUp 30.0 ]
              }
  , gen:      { starWeight: 89, starRate: 11.8, npPerHit: 1.07, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    32.0
  , align:    Lawful:Balanced
  }
, { name:     "Romulus"
  , rarity:   3
  , class:    Lancer
  , attr:     Star
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1344,  hp: 1779 }
              , max:   { atk: 7239, hp: 9883 }
              , grail: { atk: 9797, hp: 13400 }
              }
  , ratings:  { damage:4, np:3, critical:2, utility:2, support:2, durability:4 }
  , actives:  [ { name:   "Natural Body C"
                , icon:   IconHoodUp
                , cd:     5
                , effect: [ Grant Self 3 DebuffResist 100.0 
                          , To Self Heal 2500.0
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
              , { name:   "Seven Hills A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Ally 1 Guts 1.0 ]
                }
              ]
  , passives: [magicResistance B]
  , phantasm: { name:   "Magna Voluisse Magnum"
              , desc:   "All Things Lead to My Spear"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   7
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Grant Party 3 AttackUp 10.0 ]
              }
  , gen:      { starWeight: 90, starRate: 12.1, npPerHit: 1.07, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Roman]
  , death:    32.0
  , align:    Chaotic:Balanced
  }
, { name:     "Hektor"
  , rarity:   3
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1286,  hp: 1836 }
              , max:   { atk: 6928 , hp: 10200 }
              , grail: { atk: 9376, hp: 13829 }
              }
  , ratings:  { damage:4, np:3, critical:2, utility:3, support:3, durability:3 }
  , actives:  [ { name:   "Tactics C+"
                , icon:   IconBeamUp
                , cd:     4
                , effect: [ Grant Party 1 NPUp 17.0 ]
                }
              , { name:   "Proof of Friendship C"
                , icon:   IconDarkMagic
                , cd:     5
                , effect: [ Chance 80 $ To Enemy GaugeDown 1.0 
                          , Chance 80 $ Debuff Enemy 1 Stun 0.0
                          ]
                }
              , { name:   "Disengage B"
                , icon:   IconBubbles
                , cd:     5
                , effect: [ To Self RemoveDebuffs 0.0 
                          , To Self Heal 2000.0
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
              , effect: [ To Enemies DamageThruDef 600.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown 20.0 ]
              }
  , gen:      { starWeight: 90, starRate: 12.2, npPerHit: 1.08, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Riding, EnumaElish]
  , death:    28.0
  , align:    Lawful:Balanced
  }
, { name:     "Musashibou Benkei"
  , rarity:   2
  , class:    Lancer
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1029,  hp: 1722 }
              , max:   { atk: 5801, hp: 9149 }
              , grail: { atk: 8406, hp: 13204 }
              }
  , ratings:  { damage:1, np:3, critical:2, utility:4, support:3, durability:4 }
  , actives:  [ { name:   "Vengeful Spirit Exorcism A"
                , icon:   IconCircuits
                , cd:     5
                , effect: [ Chance 100 $ Debuff Enemy 1 SealSkills 0.0 ]
                }
              , { name:   "Imposing Stance B"
                , icon:   IconCrosshairUp
                , cd:     5
                , effect: [ Grant Self 1 Taunt 0.0 
                          , Grant Self 1 DefenseUp 60.0
                          ]
                }
              , { name:   "Blank Subscription List"
                , icon:   IconCircuits
                , cd:     8
                , effect: [ Chance 80 $ Debuff Enemies 1 SealNP 0.0 ]
                }
              ]
  , passives: [magicResistance CPlus]
  , phantasm: { name:   "Pilgrimage of the Five Hundred Arhat"
              , desc:   "Gohyaku Rakan Fudarakutokai"
              , rank:   EX
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Chance 80 $ Debuff Enemies 1 Stun 0.0 ]
              , over:   [ Debuff Enemies 3 Curse 500.0 ]
              }
  , gen:      { starWeight: 89, starRate: 11.9, npPerHit: 0.79, npPerDefend: 4 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    36.0
  , align:    Chaotic:Good
  }
, { name:     "Diarmuid ua Duibhne"
  , rarity:   3
  , class:    Lancer
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1277,  hp: 1817 }
              , max:   { atk: 6877, hp: 10098 }
              , grail: { atk: 9307, hp: 13691 }
              }
  , ratings:  { damage:3, np:3, critical:2, utility:3, support:3, durability:4 }
  , actives:  [ { name:   "Mind's Eye (True) B"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 3 DefenseUp 18.0
                          ]
                }
              , { name:   "Love Spot C"
                , icon:   IconSwordDown
                , cd:     5
                , effect: [ Debuff (EnemiesType Female) 1 AttackDown 50.0 ]
                }
              , { name:   "Knight's Strategy B"
                , icon:   IconStarHaloUp
                , cd:     5
                , effect: [ Grant Self 3 StarUp 50.0 ]
                }
              ]
  , passives: [magicResistance B]
  , phantasm: { name:   "Gae Dearg & Gae Buidhe B+"
              , desc:   "Crimson Rose of Exorcism & Yellow Rose of Mortality"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   2
              , effect: [ To Enemy Damage 2400.0 
                        , To Enemy RemoveDebuffs 0.0
                        ]
              , over:   [ Debuff Enemy 5 Curse 500.0 ]
              }
  , gen:      { starWeight: 87, starRate: 12.3, npPerHit: 0.78, npPerDefend: 4 }
  , hits:     { a: 2, b: 2, q: 2, ex: 4 }
  , traits:   [Male, EnumaElish, Brynhildr]
  , death:    36.0
  , align:    Lawful:Balanced
  }

]
