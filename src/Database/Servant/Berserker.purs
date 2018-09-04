module Database.Servant.Berserker (berserkers) where

import Prelude
import Operators
import Database.Model

berserkers ∷ Array Servant
berserkers = Servant <$>
[ { name:     "Cu Chulainn (Alter)"
  , id:       98
  , rarity:   5
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1979,  hp: 1790 }
              , max:   { atk: 12805, hp: 12210 }
              , grail: { atk: 14017, hp: 13377 }
              }
  , actives:  [ { name:   "Madness of the Spirits A"
                , icon:   IconExclamationDown
                , cd:     8
                , effect: [ Debuff Enemies 3 AttackDown $ 10.0 ~ 20.0
                          , Debuff Enemies 3 CritChance $ 30.0 ~ 50.0
                          ]
                }
              , { name:   "Protection from Arrows C"
                , icon:   IconDodge
                , cd:     7
                , effect: [ Times 2 $ Grant Self 0 Evasion Full
                          , Grant Self 3 DefenseUp $ 7.0 ~ 14.0
                          ]
                }
              , { name:   "Battle Continuation A"
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ Flat 1.0 ]
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Minamoto-no-Raikou"
  , id:       114
  , rarity:   5
  , class:    Berserker
  , attr:     Heaven
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1786,  hp: 1980 }
              , max:   { atk: 11556, hp: 13500 }
              , grail: { atk: 12650, hp: 14790 }
              }
  , actives:  [ { name:   "Eternal Arms Mastery A+"
                , icon:   IconStarUp
                , cd:     8
                , effect: [ Grant Self 3 StarAbsorb $ 3000.0 ~ 6000.0 ]
                }
              , { name:   "Mana Burst (Lightning) A"
                , icon:   IconBusterUp
                , cd:     8
                , effect: [ Grant Self 1 (Performance Buster) $ 20.0 ~ 30.0
                          , Grant Self 1 Evasion Full
                          ]
                }
              , { name:   "Mystic Slayer A"
                , icon:   IconDamageUp
                , cd:     8
                , effect: [ Grant Self 3 (AttackVs Demonic)
                            $ 30.0 ~ 50.0
                          , Grant Self 3 (AttackVs HeavenOrEarth)
                            $ 30.0 ~ 50.0
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
  , align:    Chaotic:Good
  , limited:  true
  , free:     false
  }
, { name:     "Sakata Kintoki"
  , id:       51
  , rarity:   5
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1964,  hp: 1782 }
              , max:   { atk: 12712, hp: 12150 }
              , grail: { atk: 13915, hp: 13311 }
              }
  , actives:  [ { name:   "Monstrous Strength A+"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 1 AttackUp $ 30.0 ~ 50.0 ]
                }
              , { name:   "Animal Communication C"
                , icon:   IconNoble
                , cd:     8
                , effect: [ To Self GaugeUp $ 30.0 ~ 50.0 ]
                }
              , { name:   "Natural Body A"
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 DebuffResist $ 60.0 ~ 120.0
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
  , align:    Lawful:Good
  , limited:  true
  , free:     false
  }
, { name:     "Vlad III"
  , id:       52
  , rarity:   5
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1777,  hp: 2019 }
              , max:   { atk: 11499, hp: 13770 }
              , grail: { atk: 12587, hp: 15086 }
              }
  , actives:  [ { name:   "Vampirism A"
                , icon:   IconDarkMagic
                , cd:     8
                , effect: [ Chances 80 100 <<< To Enemy GaugeDown $ Flat 1.0 ]
                }
              , { name:   "Morph C"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 16.0 ~ 24.0 ]
                }
              , { name:   "Battle Continuation A"
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Nightingale"
  , id:       97
  , rarity:   5
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 1573,  hp: 2232 }
              , max:   { atk: 10184, hp: 15221 }
              , grail: { atk: 11148, hp: 16675 }
              }
  , actives:  [ { name:   "Nurse of Steel A"
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Ally Heal $ 2000.0 ~ 4000.0 ]
                }
              , { name:   "Understanding of the Human Body A"
                , icon:   IconDamageUp
                , cd:     8
                , effect: [ Grant Self 3 (AttackVs Humanoid) $ 30.0 ~ 50.0
                          , Grant Self 3 (DefenseVs Humanoid) $ 15.0 ~ 25.0
                          ]
                }
              , { name:   "Angel's Cry EX"
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
              , effect: [ To Party RemoveBuffs Full
                        , To Party Heal $ 3000.0 ~ 5000.0
                        ]
              , over:   [ Debuff Enemies 1 NPDown $ 50.0 ~ 100.0 ]
              , first:  false
              }
  , gen:      { starWeight: 10, starRate: 5.0, npAtk: 0.77, npDef: 5 }
  , hits:     { quick: 6, arts: 2, buster: 1, ex: 5 }
  , traits:   [Female, EnumaElish]
  , death:    56.8
  , align:    Lawful:Good
  , limited:  false
  , free:     false
  }
, { name:     "Heracles"
  , id:       47
  , rarity:   4
  , class:    Berserker
  , attr:     Heaven
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1775,  hp: 1652 }
              , max:   { atk: 10655, hp: 10327 }
              , grail: { atk: 12901, hp: 12521 }
              }
  , actives:  [ { name:   "Valor A+"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 10.5 ~ 31.0
                          , Grant Self 3 DebuffResist $ 21.0 ~ 42.0
                          ]
                }
              , { name:   "Mind's Eye (Fake) B"
                , icon:   IconDodge
                , cd:     8
                , effect: [ Grant Self 1 Evasion Full
                          , Grant Self 3 CritUp $ 18.0 ~ 36.0
                          ]
                }
              , { name:   "Battle Continuation A"
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
  , traits:   [Male, Divine, EnumaElish, GreekMyth]
  , death:    39.0
  , align:    Chaotic:Mad
  , limited:  false
  , free:     false
  }
, { name:     "Frankenstein"
  , id:       82
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1573,  hp: 1710 }
              , max:   { atk: 9441, hp: 10687 }
              , grail: { atk: 11431, hp: 12958 }
              }
  , actives:  [ { name:   "Galvanism B"
                , icon:   IconNobleUp
                , cd:     7
                , effect: [ Grant Self 3 NPGen $ 25.0 ~ 45.0 ]
                }
              , { name:   "Wail of the Living Dead C"
                , icon:   IconStun
                , cd:     8
                , effect: [ Chance 60 $ Debuff Enemy 1 Stun Full
                          , Debuff Enemy 1 DefenseDown $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Overload C"
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
  , align:    Chaotic:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Ibaraki-Douji"
  , id:       116
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1606,  hp: 1752 }
              , max:   { atk: 9636, hp: 10954 }
              , grail: { atk: 11667, hp: 13282 }
              }
  , actives:  [ { name:   "Demonic Nature of Oni A"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Party 3 AttackUp $ 10.0 ~ 20.0
                          , Grant Self 3 NPUp $ 20.0 ~ 30.0
                          ]
                }
              , { name:   "Disengage A"
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              , { name:   "Morph A"
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Lancelot"
  , id:       48
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1746,  hp: 1652 }
              , max:   { atk: 10477, hp: 10327 }
              , grail: { atk: 12685, hp: 12521 }
              }
  , actives:  [ { name:   "Eternal Arms Mastery A+"
                , icon:   IconStarUp
                , cd:     7
                , effect: [ Grant Self 3 StarAbsorb $ 3000.0 ~ 6000.0 ]
                }
              , { name:   "Protection of the Spirits A"
                , icon:   IconStarHaloUp
                , cd:     7
                , effect: [ Grant Self 3 StarUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Mana Reversal A"
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
  , align:    Lawful:Mad
  , limited:  false
  , free:     false
  }
, { name:     "Beowulf"
  , id:       89
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1707,  hp: 1652 }
              , max:   { atk: 10247, hp: 10327 }
              , grail: { atk: 12407, hp: 12521 }
              }
  , actives:  [ { name:   "Berserk A"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 1 AttackUp $ 20.0 ~ 30.0
                          , Grant Self 1 NPUp $ 10.0 ~ 20.0
                          ]
                }
              , { name:   "Intuition B"
                , icon:   IconStar
                , cd:     7
                , effect: [ To Party GainStars $ 4.0 ~ 14.0 ]
                }
              , { name:   "Battle Continuation B"
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
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Tamamo Cat"
  , id:       58
  , rarity:   4
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1504,  hp: 1833 }
              , max:   { atk: 9026, hp: 11458 }
              , grail: { atk: 10929, hp: 13893 }
              }
  , actives:  [ { name:   "Monstrous Strength B"
                , icon:   IconSwordUp
                , cd:     9
                , effect: [ Grant Self 2 AttackUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Curse E"
                , icon:   IconDarkMagic
                , cd:     7
                , effect: [ Chances 40 60 <<< To Enemy GaugeDown $ Flat 1.0 ]
                }
              , { name:   "Morph B"
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
  , align:    Chaotic:Good
  , limited:  false
  , free:     false
  }
, { name:     "Lu Bu Fengxian"
  , id:       49
  , rarity:   3
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1507,  hp: 1494 }
              , max:   { atk: 8119, hp: 8302 }
              , grail: { atk: 10988, hp: 11256 }
              }
  , actives:  [ { name:   "Valor B"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 9.0 ~ 27.0
                          , Grant Self 3 MentalResist $ 18.0 ~ 36.0
                          ]
                }
              , { name:   "Defiant B"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 15.0 ~ 25.0
                          , Debuff Self 3 BuffFail $ Flat 50.0
                          ]
                }
              , { name:   "Chaotic Villain A"
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Spartacus"
  , id:       50
  , rarity:   1
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 922,  hp: 1544 }
              , max:   { atk: 5073, hp: 7722 }
              , grail: { atk: 7883, hp: 11904 }
              }
  , actives:  [ { name:   "Honor of Suffering B+"
                , icon:   IconHealTurn
                , cd:     9
                , effect: [ Grant Self 5 HealPerTurn $ 500.0 ~ 1500.0 ]
                }
              , { name:   "Unyielding Will A"
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 5 Guts $ Flat 1.0
                          , To Self GaugeUp $ 10.0 ~ 30.0
                          ]
                }
              , { name:   "Triumphant Return of the Sword B"
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
  , align:    Neutral:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Asterios"
  , id:       53
  , rarity:   1
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1097,  hp: 1320 }
              , max:   { atk: 6037, hp: 6604 }
              , grail: { atk: 9381, hp: 10181 }
              }
  , actives:  [ { name:   "Monstrous Strength A"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 10.0 ~ 30.0 ]
                }
              , { name:   "Natural Demon A++"
                , icon:   IconHoodUp
                , cd:     7
                , effect: [ Grant Self 3 DebuffResist $ 50.0 ~ 100.0
                          , Grant Self 3 DefenseUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Labrys of the Abyss C"
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
  , traits:   [Male, EnumaElish, GreekMyth]
  , death:    58.5
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
, { name:     "Kiyohime"
  , id:       56
  , rarity:   3
  , class:    Berserker
  , attr:     Earth
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1233,  hp: 1649 }
              , max:   { atk: 6644, hp: 9166 }
              , grail: { atk: 8992, hp: 12428 }
              }
  , actives:  [ { name:   "Morph C"
                , icon:   IconShieldUp
                , cd:     7
                , effect: [ Grant Self 3 DefenseUp $ 16.0 ~ 24.0 ]
                }
              , { name:   "Stalking B"
                , icon:   IconShieldDown
                , cd:     7
                , effect: [ Debuff Enemy 4 DefenseDown $ 12.0 ~ 24.0
                          , Grant Enemy 3 AttackUp $ Flat 20.0
                          ]
                }
              , { name:   "Flame-Colored Kiss A"
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     true
  }
, { name:     "Eric Bloodaxe"
  , id:       57
  , rarity:   2
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1116,  hp: 1447 }
              , max:   { atk: 6290, hp: 7688 }
              , grail: { atk: 9115, hp: 11095 }
              }
  , actives:  [ { name:   "Supporting Curse C+"
                , icon:   IconSwordDown
                , cd:     7
                , effect: [ Debuff Enemy 2 AttackDown $ 5.0 ~ 15.0
                          , Debuff Enemy 2 DefenseDown $ 10.0 ~ 30.0
                          ]
                }
              , { name:   "Battle Continuation B"
                , icon:   IconKneel
                , cd:     9
                , effect: [ Times 1 <<< Grant Self 4 Guts $ 750.0 ~ 2000.0 ]
                }
              , { name:   "Half-Dead Bloodaxe A+"
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
  , align:    Chaotic:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Darius III"
  , id:       55
  , rarity:   3
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1412,  hp: 1577 }
              , max:   { atk: 7608, hp: 8763 }
              , grail: { atk: 10297, hp: 11881 }
              }
  , actives:  [ { name:   "Golden Rule B"
                , icon:   IconNobleUp
                , cd:     8
                , effect: [ Grant Self 3 NPGen $ 18.0 ~ 45.0 ]
                }
              , { name:   "Disengage A"
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Self RemoveDebuffs Full
                          , To Self Heal $ 1000.0 ~ 2500.0
                          ]
                }
              , { name:   "Battle Continuation A"
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
  , align:    Lawful:Balanced
  , limited:  false
  , free:     false
  }
, { name:     "Caligula"
  , id:       54
  , rarity:   2
  , class:    Berserker
  , attr:     Mankind
  , deck:     Deck Quick Arts Buster Buster Buster
  , stats:    { base:  { atk: 1374,  hp: 1211 }
              , max:   { atk: 6831, hp: 7303 }
              , grail: { atk: 9899, hp: 10540 }
              }
  , actives:  [ { name:   "Sadistic Streak A"
                , icon:   IconSwordUp
                , cd:     7
                , effect: [ Grant Self 3 AttackUp $ 10.0 ~ 30.0
                          , Debuff Self 3 DefenseDown $ Flat 10.0
                          ]
                }
              , { name:   "Imperial Privilege A"
                , icon:   IconHeal
                , cd:     7
                , effect: [ To Self Heal $ 1000.0 ~ 3000.0
                          , Chance 60 <<< Grant Self 3 AttackUp $ 20.0 ~ 40.0
                          , Chance 60 <<< Grant Self 3 DefenseUp $ 20.0 ~ 40.0
                          ]
                }
              , { name:   "Glory of Past Days B"
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
  , align:    Chaotic:Evil
  , limited:  false
  , free:     false
  }
]
