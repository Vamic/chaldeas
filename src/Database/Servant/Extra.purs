module Database.Servant.Extra where

import Prelude
import Operators
import Database.Model

extras ∷ Array Servant
extras = Servant <$>
[ { name:     "Jeanne d'Arc"
  , rarity:   5
  , class:    Ruler
  , attr:     Star
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1482,  hp: 2420 }
              , max:   { atk: 9593, hp: 16500 }
              , grail: { atk: 10501, hp: 18076 }
              }
  , ratings:  { damage:2, np:4, critical:3, utility:3, support:4, durability:5 }
  , actives:  [ { name:   "Revelation A"
                , icon:   IconStarTurn
                , cd:     6
                , effect: [ Grant Self 3 StarsPerTurn 9.0 ]
                }
              , { name:   "True Name Revelation B"
                , icon:   IconBeamDown
                , cd:     5
                , effect: [ Debuff Enemy 1 NPDown 30.0 ]
                }
              , { name:   "Divine Judgment A"
                , icon:   IconStun
                , cd:     6
                , effect: [ Chance 120 $ Debuff Enemy 1 Stun 0.0 ]
                }
              ]
  , passives: [magicResistance EX]
  , phantasm: { name:   "Luminosite Eternelle"
              , desc:   "God is Here With Me"
              , rank:   A
              , card:   Arts
              , kind:   "Barrier"
              , hits:   0
              , effect: [ Grant Party 3 DefenseUp 25.0 
                        , Grant Party 1 Invincibility 0.0
                        , To Party RemoveDebuffs 0.0
                        ]
              , over:   [ Grant Party 2 HealPerTurn 1000.0 ]
              }
  , gen:      { starWeight: 99, starRate: 10.1, npPerHit: 0.76, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, EnumaElish, Saberface]
  , death:    21.0
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "Amakusa Shirou"
  , rarity:   5
  , class:    Ruler
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1695,  hp: 2069 }
              , max:   { atk: 10972, hp: 14107 }
              , grail: { atk: 12011, hp: 15455 }
              }
  , ratings:  { damage:3, np:5, critical:3, utility:5, support:2, durability:4 }
  , actives:  [ { name:   "Revelation A"
                , icon:   IconStarTurn
                , cd:     6
                , effect: [ Grant Self 3 StarsPerTurn 9.0 ]
                }
              , { name:   "Baptism Rite B+"
                , icon:   IconNobleTurn
                , cd:     10
                , effect: [ Grant Self 5 GaugePerTurn 20.0 
                          , To (EnemyType Undead) GaugeDown 1.0
                          , To (EnemyType Demon) GaugeDown 1.0
                          ]
                }
              , { name:   "Divine Judgment C"
                , icon:   IconStun
                , cd:     6
                , effect: [ Chance 100 $ Debuff Enemy 1 Stun 0.0 ]
                }
              ]
  , passives: [magicResistance A]
  , phantasm: { name:   "Twin Arm—Big Crunch"
              , desc:   "Dual Arm Zero-Order Convergence"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies RemoveBuffs 0.0 
                        , To Enemies Damage 500.0
                        ]
              , over:   [ Debuff Enemies 1 CritChance 30.0 ]
              }
  , gen:      { starWeight: 100, starRate: 10.0, npPerHit: 0.86, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 7 }
  , traits:   [Male, EnumaElish]
  , death:    21.0
  , align:    Lawful:Good
  , limited:  true
  }
, { name:     "Jeanne d'Arc (Alter)"
  , rarity:   5
  , class:    Avenger
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 2046,  hp: 1724 }
              , max:   { atk: 13244, hp: 11761 }
              , grail: { atk: 14498, hp: 12885 }
              }
  , ratings:  { damage:5, np:4, critical:5, utility:5, support:3, durability:3 }
  , actives:  [ { name:   "Self-Modification EX"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 3 CritUp 50.0 
                          , Grant Self 3 StarAbsorb 800.0
                          ]
                }
              , { name:   "Dragon Witch EX"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Allies 3 AttackUp 20.0 
                          , Grant (AlliesType Dragon) 3 AttackUp 20.0
                          ]
                }
              , { name:   "Ephemeral Dream A"
                , icon:   IconBusterUp
                , cd:     6
                , effect: [ Grant Self 1 BusterUp 50.0 
                          , Grant Self 1 Invincibility 0.0
                          , To Self DemeritHealth 1000.0
                          ]
                }
              ]
  , passives: [avenger B, oblivionCorrection A, selfRestoreMagic APlus]
  , phantasm: { name:   "La Grondement Du Haine"
              , desc:   "How Loudly, My Resentment"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   10
              , effect: [ To Enemy Damage 1000.0 
                        , Debuff Enemy 0 BuffBlock 1.0
                        ]
              , over:   [ Debuff Enemy 5 Curse 500.0 ]
              }
  , gen:      { starWeight: 29, starRate: 6.0, npPerHit: 0.83, npPerDefend: 5 }
  , hits:     { a: 2, b: 4, q: 3, ex: 7 }
  , traits:   [Female, EnumaElish, Saberface]
  , death:    5.7
  , align:    Chaotic:Evil
  , limited:  true
  }
, { name:     "Edmond Dantes"
  , rarity:   5
  , class:    Avenger
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1953,  hp: 1785 }
              , max:   { atk: 12641, hp: 12177 }
              , grail: { atk: 13838, hp: 13340 }
              }
  , ratings:  { damage:4, np:3, critical:3, utility:3, support:3, durability:2 }
  , actives:  [ { name:   "Iron Determination EX"
                , icon:   IconShieldBreak
                , cd:     6
                , effect: [ Grant Self 1 IgnoreInvinc 0.0 
                          , Grant Self 1 AttackUp 50.0
                          , Grant Self 3 DebuffResist 32.0
                          ]
                }
              , { name:   "Golden Rule A"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 50.0 ]
                }
              , { name:   "Wisdom of Crisis A"
                , icon:   IconDarkMagic
                , cd:     6
                , effect: [ To Enemy GaugeDown 1.0 
                          , To Self RemoveDebuffs 0.0
                          , To Party GainStars 20.0
                          ]
                }
              ]
  , passives: [avenger A, oblivionCorrection B, selfRestoreMagic D]
  , phantasm: { name:   "Enfer Château d'If"
              , desc:   "Tyger, Burning Bright"
              , rank:   A
               , card:   Quick
              , kind:   "Anti-Army"
              , hits:   8
              , effect: [ To Enemies Damage 1000.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown 20.0 
                        , Debuff Enemies 5 Curse 500.0
                        ]
              }
  , gen:      { starWeight: 30, starRate: 5.9, npPerHit: 0.62, npPerDefend: 5 }
  , hits:     { a: 2, b: 3, q: 4, ex: 5 }
  , traits:   [Male, EnumaElish]
  , death:    7.0
  , align:    Chaotic:Evil
  , limited:  true
  }
, { name:     "Saint Martha (Ruler)"
  , rarity:   4
  , class:    Ruler
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1591,  hp: 1800 }
              , max:   { atk: 9546, hp: 11250 }
              , grail: { atk: 11558, hp: 13640 }
              }
  , ratings:  { damage:4, np:3, critical:2, utility:2, support:2, durability:4 }
  , actives:  [ { name:   "Saint of the Shore B+"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 30.0 
                          , When "on Waterside or Beach field" 
                            $ Grant Self 3 AttackUp 20.0
                          ] 
                }
              , { name:   "Natural Body (Sea) A"  
                , icon:   IconFlex
                , cd:     5
                , effect: [ Grant Self 0 DebuffImmunity 1.0 ]
                }
              , { name:   "Jacob's Limbs B"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 1 (AttackUpVs Demon) 100.0
                          , Grant Self 1 (AttackUpVs Divine) 100.0
                          , Grant Self 1 (AttackUpVs Undead) 100.0
                          ]
                }
              ]
  , passives: [magicResistance EX]
  , phantasm: { name:   "Tarasque"
              , desc:   "O' Tragic Drake Rage"
              , rank:   A
              , card:   Buster
              , kind:   "Anti-Unit/Anti-Dragon"
              , hits:   10
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ Debuff Enemy 1 DefenseDown 10.0 ]
              }
  , gen:      { starWeight: 102, starRate: 10.0, npPerHit: 0.76, npPerDefend: 3 }
  , hits:     { a: 3, b: 1, q: 4, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    21.0
  , align:    Lawful:Good
  , limited:  true
  }
, { name:     "Mash Kyrielight"
  , rarity:   3
  , class:    Shielder
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1261,  hp: 1854 }
              , max:   { atk: 6791, hp: 10302 }
              , grail: { atk: 10575, hp: 15619 }
              }
  , ratings:  { damage:2, np:3, critical:1, utility:5, support:4, durability:4 }
  , actives:  [ { name:   "Honorable Wall of Snowflakes"
                , icon:   IconShieldUp
                , cd:     5
                , effect: [ Grant Party 3 DefenseUp 20.0 
                          , Grant Party 0 DamageCut 2000.0
                          ]
                }
              , { name:   "Obscurant Wall of Chalk"
                , icon:   IconShield
                , cd:     7
                , effect: [ Grant Ally 1 Invincibility 0.0 
                          , To Ally GaugeUp 20.0
                          ]
                }
              , { name:   "Shield of Rousing Resolution"
                , icon:   IconCrosshairUp
                , cd:     6
                , effect: [ Grant Self 1 Taunt 0.0
                          , Grant Self 1 NPGen 400.0
                          ]
                }
              ]
  , passives: [magicResistance A, riding C]
  , phantasm: { name:   "Lord Camelot"
              , desc:   "Castle of the Distant Utopia"
              , rank:   BPlusPlus
              , card:   Arts
              , kind:   "Anti-Evil"
              , hits:   0
              , effect: [ Grant Party 3 ReduceDamage 1000.0 
                        , Grant Others 3 AttackUp 30.0
                        ]
              , over:   [ Grant Party 3 DefenseUp 30.0 ]
              }
  , gen:      { starWeight: 99, starRate: 9.9, npPerHit: 0.84, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, EnumaElish]
  , death:    24.5
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "Angra Mainyu"
  , rarity:   0
  , class:    Avenger
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1008,  hp: 1502 }
              , max:   { atk: 5683, hp: 7981 }
              , grail: { atk: 8235, hp: 11518 }
              }
  , ratings:  { damage:2, np:2, critical:2, utility:2, support:1, durability:1 }
  , actives:  [ { name:   "Zarich C"
                , icon:   IconExclamationDown
                , cd:     6
                , effect: [ Debuff Enemy 3 CritChance 50.0 ]
                }
              , { name:   "Tawrich C"
                , icon:   IconDarkMagic
                , cd:     6
                , effect: [ To Enemy GaugeDown 1.0 
                          , Debuff Enemy 3 AttackDown 30.0
                          ]
                }
              , { name:   "Annihilation Wish A"
                , icon:   IconQuickUp
                , cd:     8
                -- TODO in sequence
                , effect: [ When "turn 1" $ Grant Self 1 QuickUp 40.0 
                          , When "turn 2" $ Grant Self 1 QuickUp 80.0
                          , When "turn 3" $ Grant Self 1 QuickUp 120.0
                          , When "turn 4" $ Grant Self 1 QuickUp 160.0
                          , When "turn 5" $ Grant Self 1 QuickUp 200.0
                          , When "turn 6" $ To Self DemeritKill 0.0
                          ] 
                }
              ]
  , passives: [avenger A, oblivionCorrection A, selfRestoreMagic E]
  , phantasm: { name:   "Verg Avesta"
              , desc:   "False Copy of Inscribed Creation"
              , rank:   CMinus
              , card:   Arts
              , kind:   "Anti-Unit"
              , hits:   1 
              , effect: [ -- Wait 1 turn, then deal damage equal to damage
                          -- received in the previous turn times 3
                          -- damage cannot be blocked
                          Debuff Self 1 Stun 0.0
                        , To Enemy DamageRevenge 300.0
                        ]
              , over:   [ To Self Heal 5000.0 ]
              }
  , gen:      { starWeight: 29, starRate: 7.0, npPerHit: 0.79, npPerDefend: 5 }
  , hits:     { a: 2, b: 1, q: 4, ex: 5 }
  , traits:   [Male, EnumaElish]
  , death:    9.0
  , align:    Chaotic:Evil
  , limited:  false
  }
]
