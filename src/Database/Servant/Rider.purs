module Database.Servant.Rider where

import Prelude
import Operators
import Database.Model

riders ∷ Array Servant
riders = Servant <$>
[ { name:     "Ozymandias"
  , rarity:   5
  , class:    Rider
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1850,  hp: 1881 }
              , max:   { atk: 11971, hp: 12830 }
              , grail: { atk: 13104, hp: 14056 }
              }
  , ratings:  { damage:4, np:4, critical:2, utility:4, support:4, durability:3 }
  , actives:  [ { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 18.0 ]
                }
              , { name:   "Imperial Privilege A"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Self Heal 3000.0
                          , Chance 60 $ Grant Self 3 AttackUp 40.0
                          , Chance 60 $ Grant Self 3 DefenseUp 40.0
                          ]
                }
              , { name:   "Protection of the Sun God A"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Party GaugeUp 20.0 
                          , Grant Party 1 BuffUp 40.0
                          ]
                }
              ]
  , passives: [magicResistance B, riding APlus, divinity B]
  , phantasm: { name:   "Ramesseum Tentyris"
              , desc:   "The Shining Great Temple Complex"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Fortress" 
              , hits:   5
              , effect: [ To Enemy Damage 1000.0 
                        , Debuff Enemy 1 SealNP 0.0
                        ]
              , over:   [ Debuff Enemy 3 DefenseDown 20.0 ]
              }
  , gen:      { starWeight: 205, starRate: 9.0, npPerHit: 0.59, npPerDefend: 3 }
  , hits:     { a: 3, b: 1, q: 5, ex: 5 }
  , traits:   [Male, Brynhildr, Divine, Riding, EnumaElish]
  , death:    30.0
  , align:    Chaotic:Balanced
  , limited:  false
  }
, { name:     "Francis Drake"
  , rarity:   5
  , class:    Rider
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1750,  hp: 1881 }
              , max:   { atk: 11326, hp: 12830 }
              , grail: { atk: 12398, hp: 14056 }
              }
  , ratings:  { damage:4, np:5, critical:5, utility:3, support:3, durability:2 }
  , actives:  [ { name:   "Voyager of the Storm A+"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Party 1 NPUp 17.0
                          , Grant Party 1 AttackUp 17.0
                          ]
                }
              , { name:   "Golden Rule B"
                , icon:   IconNobleUp
                , cd:     6
                , effect: [ Grant Self 3 NPGen 45.0 ]
                }
              , { name:   "Pioneer of the Stars EX"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0 
                          , Grant Self 3 IgnoreInvinc 0.0
                          , To Party GainStars 10.0
                          ]
                }
              ]
  , passives: [magicResistance D, riding D]
  , phantasm: { name:   "Golden Wild Hunt"
              , desc:   "Golden Stag and the Eventide Tempest"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage 600.0 ]
              , over:   [ To Party GainStars 20.0 ]
              }
  , gen:      { starWeight: 208, starRate: 9.0, npPerHit: 0.42, npPerDefend: 3 }
  , hits:     { a: 4, b: 2, q: 6, ex: 4 }
  , traits:   [Female, Riding]
  , death:    50.0
  , align:    Chaotic:Evil
  , limited:  false
  }
, { name:     "Iskandar"
  , rarity:   5
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1786,  hp: 1938 }
              , max:   { atk: 11560, hp: 13219 }
              , grail: { atk: 12654, hp: 14482 }
              }
  , ratings:  { damage:5, np:2, critical:3, utility:2, support:3, durability:2 }
  , actives:  [ { name:   "Charisma A"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 20.0 ]
                }
              , { name:   "Tactics B"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Party 1 NPUp 18.0 ]
                }
              , { name:   "Lightning Conqueror EX"
                , icon:   IconBusterUp
                , cd:     6
                , effect: [ Grant Self 1 BusterUp 50.0 
                          , Grant Self 1 StarUp 50.0
                          ]
                }
              ]
  , passives: [magicResistance D, riding APlus, divinity C]
  , phantasm: { name:   "Ionioi Hetairoi"
              , desc:   "Army of the King"
              , rank:   EX
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   8
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown 10.0 ]
              }
  , gen:      { starWeight: 205, starRate: 8.8, npPerHit: 0.66, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 6 }
  , traits:   [Male, Brynhildr, Divine, GreekMyth, King, Riding, EnumaElish]
  , death:    40.0
  , align:    Neutral:Good
  , limited:  true
  }
, { name:     "Queen Medb"
  , rarity:   5
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1591,  hp: 2048 }
              , max:   { atk: 1026, hp: 13968 }
              , grail: { atk: 11270, hp: 15303 }
              }
  , ratings:  { damage:2, np:4, critical:4, utility:2, support:4, durability:3 }
  , actives:  [ { name:   "Golden Rule (Body) A"
                , icon:   IconHealTurn
                , cd:     6
                , effect: [ Grant Self 3 DebuffImmunity 0.0 
                          , Grant Self 3 HealPerTurn 1000.0
                          , Grant Self 3 GaugePerTurn 10.0
                          ]
                }
              , { name:   "Charisma B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 18.0 ]
                }
              , { name:   "Siren Song C"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Chance 80 $ Debuff (EnemyType Male) 1 Charm 0.0 ]
                }
              ]
  , passives: [magicResistance B, riding A]
  , phantasm: { name:   "Chariot My Love"
              , desc:   "My Dear Iron Chariot"
              , rank:   B
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemy Damage 1000.0 ]
              , over:   [ To (EnemyType Male) Damage 150.0 
                        , Debuff Enemy 3 MentalVuln 10.0
                        ]
              }
  , gen:      { starWeight: 208, starRate: 9.0, npPerHit: 0.86, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 7 }
  , traits:   [Female, Riding, EnumaElish]
  , death:    40.0
  , align:    Chaotic:Evil
  , limited:  false
  }
, { name:     "Sakata Kintoki (Rider)"
  , rarity:   4
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Buster Buster
  , stats:    { base:  { atk: 1636,  hp: 1728 }
              , max:   { atk: 9819, hp: 10800 }
              , grail: { atk: 11889, hp: 13095 }
              }
  , ratings:  { damage:5, np:4, critical:4, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Long-Distance Dash A"
                , icon:   IconQuickUp
                , cd:     5
                , effect: [ Grant Self 3 QuickUp 30.0 
                          , Grant Self 3 StarUp 50.0
                          ]
                }
              , { name:   "Animal Communication C"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 50.0 ]
                }
              , { name:   "Natural Body A"
                , icon:   IconHoodUp
                , cd:     5
                , effect: [ Grant Self 3 DebuffResist 120.0 
                          , To Self Heal 3000.0
                          ]
                }
              ]
  , passives: [divinity C]
  , phantasm: { name:   "Golden Drive—Good Night"
              , desc:   "Night Wolf Deadly Nine—Golden Dash"
              , rank:   B
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   4
              , effect: [ To Enemy Damage 2000.0 ]
              , over:   [ Grant Self 1 QuickUp 10.0 ]
              }
  , gen:      { starWeight: 198, starRate: 9.0, npPerHit: 1.15, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 4, ex: 5 }
  , traits:   [Male, Brynhildr, Divine, EnumaElish]
  , death:    40.0
  , align:    Lawful:Good
  , limited:  true
  }
, { name:     "Altria Pendragon (Santa Alter)"
  , rarity:   4
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1543,  hp: 1805 }
              , max:   { atk: 9258, hp: 11286 }
              , grail: { atk: 11209, hp: 13684 }
              }
  , ratings:  { damage:5, np:3, critical:4, utility:2, support:3, durability:2 }
  , actives:  [ { name:   "Saint's Gift EX"
                , icon:   IconHeal
                , cd:     5
                , effect: [ To Ally Heal 3500.0 
                          , Grant Ally 3 StarUp 30.0
                          ]
                }
              , { name:   "Intuition A"
                , icon:   IconStar
                , cd:     5
                , effect: [ To Party GainStars 15.0 ]
                }
              , { name:   "Mana Burst A-"
                , icon:   IconBusterUp
                , cd:     5
                , effect: [ Grant Self 1 BusterUp 45.0 ]
                }
              ]
  , passives: [magicResistance A, riding A]
  , phantasm: { name:   "Excalibur Morgan"
              , desc:   "Sword of Promised Victory"
              , rank:   APlusPlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   3
              , effect: [ To Enemies Damage 650.0 ]
              , over:   [ To Self GaugeUp 10.0 ]
              }
  , gen:      { starWeight: 204, starRate: 8.9, npPerHit: 0.87, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Arthur, Dragon, Riding, Saberface, EnumaElish, King]
  , death:    35.0
  , align:    Lawful:Good
  , limited:  true
  }
, { name:     "Saint Martha"
  , rarity:   4
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Arts Buster
  , stats:    { base:  { atk: 1335,  hp: 2090 }
              , max:   { atk: 8014, hp: 13068 }
              , grail: { atk: 9703, hp: 15845 }
              }
  , ratings:  { damage:2, np:4, critical:3, utility:4, support:4, durability:3 }
  , actives:  [ { name:   "Protection of the Faith A"
                , icon:   IconHoodUp
                , cd:     5
                , effect: [ Grant Self 3 DebuffResist 100.0 
                          , To Self Heal 2500.0
                          ]
                }
              , { name:   "Miracle D+"
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Party Heal 2000.0 
                          , To Party RemoveDebuffs 0.0
                          ]
                }
              , { name:   "Oath of the Holy Maiden C"
                , icon:   IconShieldDown
                , cd:     5
                , effect: [ Debuff Enemy 3 DefenseDown 30.0 
                          , To Enemy RemoveBuffs 0.0
                          ]
                }
              ]
  , passives: [magicResistance A, riding APlusPlus, divinity C]
  , phantasm: { name:   "Tarasque"
              , desc:   "O Tragic Drake Who Knew Naught of Love"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage 500.0 ]
              , over:   [ Debuff Enemies 3 DefenseDown 40.0 ]
              }
  , gen:      { starWeight: 205, starRate: 9.0, npPerHit: 1.58, npPerDefend: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, Divine, EnumaElish]
  , death:    30.0
  , align:    Lawful:Good
  , limited:  true
  }
, { name:     "Mordred (Rider)"
  , rarity:   4
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1535,  hp: 1824 }
              , max:   { atk: 9212, hp: 11400 }
              , grail: { atk: 11154, hp: 13822 }
              }
  , ratings:  { damage:3, np:5, critical:3, utility:2, support:1, durability:4 }
  , actives:  [ { name:   "Cerulean Ride A"
                , icon:   IconArtsUp
                , cd:     5
                , effect: [ Grant Self 3 ArtsUp 30.0 ]
                }
              , { name:   "Rodeo Flip A+"
                , icon:   IconDodge
                , cd:     7
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 1 StarUp 50.0
                          ]
                }
              , { name:   "Endless Summer B"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 3 Guts 1.0 
                          , To Self GaugeUp 30.0
                          ]
                }
              ]
  , passives: [magicResistance B, surfing A]
  , phantasm: { name:   "Prydwen Tube Riding"
              , desc:   "Kingly Mood of Reining in Surging Waves!"
              , rank:   A
              , card:   Arts
              , kind:   "Anti-Wave"
              , hits:   5
              , effect: [ To Enemies Damage 750.0 ]
              , over:   [ Chance 50 $ To Enemies GaugeDown 1.0 ]
              }
  , gen:      { starWeight: 204, starRate: 9.2, npPerHit: 0.71, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 3, ex: 4 }
  , traits:   [Female, Dragon, Saberface, EnumaElish]
  , death:    35.0
  , align:    Chaotic:Good
  , limited:  true
  }
, { name:     "Marie Antoinette"
  , rarity:   4
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1382,  hp: 1975 }
              , max:   { atk: 8293, hp: 12348 }
              , grail: { atk: 10041, hp: 14972 }
              }
  , ratings:  { damage:2, np:2, critical:3, utility:3, support:2, durability:5 }
  , actives:  [ { name:   "Siren Song C"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Chance 80 $ Debuff (EnemyType Male) 1 Charm 0.0 ]
                }
              , { name:   "Beautiful Princess A"
                , icon:   IconShield
                , cd:     6
                , effect: [ Grant Self 0 Invincibility 3.0 
                          , Grant Self 5 HealPerTurn 600.0
                          ]
                }
              , { name:   "God's Grace B"
                , icon:   IconStaffUp
                , cd:     5
                , effect: [ Grant Self 3 MentalSuccess 30.0 
                          , To Self Heal 2500.0  
                          ]
                }
              ]
  , passives: [magicResistance C, riding APlus]
  , phantasm: { name:   "Guillotine Breaker"
              , desc:   "Glory to the Crown of Lillies"
              , rank:   A
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage 1000.0 
                        , To Party RemoveDebuffs 0.0
                        ]
              , over:   [ To Party Heal 500.0 ]
              }
  , gen:      { starWeight: 201, starRate: 9.0, npPerHit: 1.0, npPerDefend: 3 }
  , hits:     { a: 1, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, EnumaElish]
  , death:    35.0
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "Anne Bonny & Mary Read"
  , rarity:   4
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1504,  hp: 1805 }
              , max:   { atk: 9029, hp: 11286 }
              , grail: { atk: 10932, hp: 13684 }
              }
  , ratings:  { damage:5, np:3, critical:5, utility:1, support:1, durability:2 }
  , actives:  [ { name:   "Voyage A"
                , icon:   IconStarHaloUp
                , cd:     6
                , effect: [ Grant Self 3 StarUp 50.0 ]
                }
              , { name:   "Marksmanship B"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 1 CritUp 100.0 ]
                }
              , { name:   "Combination C"
                , icon:   IconStarUp
                , cd:     5
                , effect: [ Grant Self 1 StarAbsorb 600.0 
                          , Grant Self 1 AttackUp 30.0
                          ]
                }
              ]
  , passives: [magicResistance D]
  , phantasm: { name:   "Carribbean Free Bird"
              , desc:   "Bond of Lovebirds"
              , rank:   CPlusPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   6
              , effect: [ To Enemy Damage 2400.0 ]
              , over:   [ To Enemy DamageRevenge 1200.0 ]
              }
  , gen:      { starWeight: 200, starRate: 9.1, npPerHit: 0.84, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, EnumaElish]
  , death:    50.0
  , align:    Chaotic:Evil
  , limited:  false
  }
, { name:     "Astolfo"
  , rarity:   4
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1489,  hp: 1787 }
              , max:   { atk: 8937, hp: 11172 }
              , grail: { atk: 10821, hp: 13456 }
              }
  , ratings:  { damage:2, np:1, critical:3, utility:2, support:2, durability:2 }
  , actives:  [ { name:   "Monstrous Strength C-"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 1 AttackUp 28.0 ]
                }
              , { name:   "Trap of Argalia D"
                , icon:   IconStun
                , cd:     8
                , effect: [ Chance 90 $ Debuff Enemy 1 Stun 0.0 ]
                }
              , { name:   "Evaporation of Reason D"
                , icon:   IconStarTurn
                , cd:     8
                , effect: [ Chance 65 $ Grant Self 3 StarsPerTurn 10.0 
                          , Chance 65 $ Grant Self 3 StarUp 50.0
                          , Chance 65 $ Grant Self 3 CritUp 40.0
                          ]
                }
              ]
  , passives: [magicResistance A, riding APlus, independentAction B]
  , phantasm: { name:   "Hippogriff"
              , desc:   "Otherworldly Phantom Horse"
              , rank:   BPlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies DamageThruDef 1000.0 
                        , Grant Self 0 Evasion 3.0
                        ]
              , over:   [ To Party GainStars 25.0 ]
              }
  , gen:      { starWeight: 205, starRate: 9.0, npPerHit: 0.66, npPerDefend: 1 }
  , hits:     { a: 2, b: 1, q: 2, ex: 4 }
  , traits:   [Riding, EnumaElish]
  , death:    40.0
  , align:    Chaotic:Good
  , limited:  false
  }
, { name:     "Georgios"
  , rarity:   2
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 929,  hp: 1731 }
              , max:   { atk: 5236, hp: 9200 }
              , grail: { atk: 7587, hp: 13278 }
              }
  , ratings:  { damage:2, np:4, critical:3, utility:4, support:3, durability:5 }
  , actives:  [ { name:   "Guardian Knight A+"
                , icon:   IconCrosshairUp
                , cd:     5
                , effect: [ Grant Self 3 Taunt 0.0 
                          , Grant Self 3 DefenseUp 20.0
                          ]
                }
              , { name:   "Martyr's Soul B+"
                , icon:   IconHoodUp
                , cd:     5
                , effect: [ Grant Self 3 DebuffResist 100.0 
                          , To Self Heal 2000.0
                          ]
                }
              , { name:   "Battle Continuation A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              ]
  , passives: [magicResistance A, riding B]
  , phantasm: { name:   "Ascalon"
              , desc:   "Blessed Sword of Force Subversion"
              , rank:   B
              , card:   Arts
              , kind:   "Anti-Personnel"
              , hits:   4
              , effect: [ To Enemy Damage 1500.0 
                        , Debuff Enemy 3 (ApplyTrait Dragon) 0.0
                        ]
              , over:   [ Grant Self 1 DefenseUp 20.0 ]
              }
  , gen:      { starWeight: 205, starRate: 8.9, npPerHit: 0.85, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Riding, Brynhildr, EnumaElish]
  , death:    45.0
  , align:    Lawful:Good
  , limited:  false
  }
, { name:     "Ushiwakamaru"
  , rarity:   3
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1314,  hp: 1628 }
              , max:   { atk: 7076, hp: 9028 }
              , grail: { atk: 9576, hp: 12240 }
              }
  , ratings:  { damage:4, np:3, critical:3, utility:1, support:3, durability:3 }
  , actives:  [ { name:   "Tengu's Strategy A"
                , icon:   IconNobleUp
                , cd:     5
                , effect: [ Grant Party 3 NPGen 20.0 ]
                }
              , { name:   "Charisma C+"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 17.0 ]
                }
              , { name:   "Art of the Swallow B"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 1.0 
                          , Grant Self 1 StarUp 100.0
                          ]
                }
              ]
  , passives: [magicResistance C, riding APlus]
  , phantasm: { name:   "Dan-No-Ura Eight-Boat Leap"
              , desc:   "Dan-no-ura • Hassou Tobi"
              , rank:   C
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   1
              , effect: [ To Enemy Damage 2400.0 ]
              , over:   [ Grant Self 3 StarUp 50.0 ]
              }
  , gen:      { starWeight: 204, starRate: 9.1, npPerHit: 0.87, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, Brynhildr, EnumaElish]
  , death:    35.0
  , align:    Chaotic:Balanced
  , limited:  false
  }
, { name:     "Medusa"
  , rarity:   3
  , class:    Rider
  , attr:     Earth
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1337,  hp: 1608 }
              , max:   { atk: 7200, hp: 8937 }
              , grail: { atk: 9744, hp: 12117 }
              }
  , ratings:  { damage:3, np:4, critical:4, utility:3, support:2, durability:2 }
  , actives:  [ { name:   "Mystic Eyes A+"
                , icon:   IconStun
                , cd:     6
                , effect: [ Chance 100 $ Debuff Enemy 1 Stun 0.0 ]
                }
              , { name:   "Monstrous Strength B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 2 AttackUp 30.0 ]
                }
              , { name:   "Blood Fort Andromeda B"
                , icon:   IconNoble
                , cd:     6
                , effect: [ To Self GaugeUp 20.0 
                          , Grant Self 3 NPGen 30.0
                          ]
                }
              ]
  , passives: [ magicResistance B, independentAction C, divinity EMinus
              , riding APlus
              ]
  , phantasm: { name:   "Bellerophon"
              , desc:   "Bridle of Chivalry"
              , rank:   APlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage 1000.0 ]
              , over:   [ Grant Party 3 StarUp 20.0 ]
              }
  , gen:      { starWeight: 194, starRate: 9.0, npPerHit: 0.58, npPerDefend: 3 }
  , hits:     { a: 3, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, Divine]
  , death:    35.0
  , align:    Chaotic:Good
  , limited:  false
  }
, { name:     "Edward Teach"
  , rarity:   2
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1097,  hp: 1488 }
              , max:   { atk: 6188, hp: 7907 }
              , grail: { atk: 8967, hp: 11411 }
              }
  , ratings:  { damage:4, np:2, critical:2, utility:2, support:3, durability:3 }
  , actives:  [ { name:   "Voyager of the Storm A"
                , icon:   IconBeamUp
                , cd:     5
                , effect: [ Grant Party 1 NPUp 16.0 
                          , Grant Party 1 AttackUp 16.0
                          ]
                }
              , { name:   "Pirate's Glory B"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Self 3 AttackUp 27.0
                          , Grant Self 0 Guts 1.0
                          , Debuff Self 3 DebuffVuln 50.0
                          ]
                }
              , { name:   "Gentlemanly Love C"
                , icon:   IconHeal
                , cd:     6
                , effect: [ To Party Heal 2000.0 
                          , To (AlliesType Female) Heal 2000.0 
                            -- TODO also genderfluid
                          ]
                }
              ]
  , passives: [magicResistance E]
  , phantasm: { name:   "Queen Anne's Revenge"
              , desc:   "Revenge of the Queen Anne"
              , rank:   CPlusPlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   5
              , effect: [ To Enemies Damage 500.0 
                        , To Party GainStars 5.0
                        ]
              , over:   [ Chance 30 $ To Enemies GaugeDown 1.0 ]
              }
  , gen:      { starWeight: 198, starRate: 8.8, npPerHit: 0.56, npPerDefend: 3 }
  , hits:     { a: 3, b: 1, q: 2, ex: 3 }
  , traits:   [Male, EnumaElish]
  , death:    45.0
  , align:    Chaotic:Evil
  , limited:  false
  }
, { name:     "Alexander"
  , rarity:   3
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1555,  hp: 1366 }
              , max:   { atk: 7356, hp: 8640 }
              , grail: { atk: 9955, hp: 11714 }
              }
  , ratings:  { damage:3, np:3, critical:4, utility:3, support:2, durability:2 }
  , actives:  [ { name:   "Charisma C"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Party 3 AttackUp 16.0 ]
                }
              , { name:   "Fair Youth B"
                , icon:   IconHeart
                , cd:     7
                , effect: [ Chance 80 $ Debuff (EnemyType Humanoid) 1 Charm 0.0 ] 
                }
              , { name:   "Omen of the Conqueror A"
                , icon:   IconQuickUp
                , cd:     5
                , effect: [ Grant Party 3 QuickUp 20.0 ]
                }
              ]
  , passives: [magicResistance D, divinity E, riding APlus]
  , phantasm: { name:   "Bucephalus"
              , desc:   "The Beginning of Trampling Conquest"
              , rank:   BPlusPlus
              , card:   Quick
              , kind:   "Anti-Army"
              , hits:   1
              , effect: [ To Enemies Damage 1200.0 ]
              , over:   [ To Party GainStars 15.0 ]
              }
  , gen:      { starWeight: 205, starRate: 9.0, npPerHit: 0.86, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Male, Riding, Divine, EnumaElish]
  , death:    40.0
  , align:    Neutral:Good
  , limited:  false
  }
, { name:     "Boudica"
  , rarity:   3
  , class:    Rider
  , attr:     Mankind
  , deck:     Deck Quick Quick Arts Arts Buster
  , stats:    { base:  { atk: 1168,  hp: 1823 }
              , max:   { atk: 6289, hp: 10130 }
              , grail: { atk: 8511, hp: 13735 }
              }
  , ratings:  { damage:2, np:3, critical:3, utility:2, support:3, durability:4 }
  , actives:  [ { name:   "Vow to the Goddess B"
                , icon:   IconDamageUp
                , cd:     5
                , effect: [ Grant Self 3 (AttackUpVs Roman) 60.0 ]
                }
              , { name:   "Battle Continuation A"
                , icon:   IconKneel
                , cd:     7
                , effect: [ Grant Self 5 Guts 1.0 ]
                }
              , { name:   "Andraste's Protection A"
                , icon:   IconArtsUp
                , cd:     5
                , effect: [ Grant Party 3 ArtsUp 20.0 ]
                }
              ]
  , passives: [magicResistance D, riding A]
  , phantasm: { name:   "Chariot of Boudica"
              , desc:   "Chariot Without Promised Protection"
              , rank:   BPlus
              , card:   Arts
              , kind:   "Anti-Army"
              , hits:   0
              , effect: [ Grant Party 3 DefenseUp 20.0 ]
              , over:   [ Grant Party 1 DefenseUp 20.0 ]
              }
  , gen:      { starWeight: 196, starRate: 8.9, npPerHit: 0.85, npPerDefend: 3 }
  , hits:     { a: 2, b: 1, q: 2, ex: 3 }
  , traits:   [Female, Riding, EnumaElish]
  , death:    45.0
  , align:    Neutral:Good
  , limited:  false
  }
]
