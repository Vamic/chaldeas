module Database.CraftEssence (CraftEssence(..), craftEssences) where

import Prelude
import Operators

import Database.Base
import Database.Skill

type CraftEssence = { name     ∷ String
                     , id       ∷ Int
                     , rarity   ∷ Int
                     , stats    ∷ { base ∷ Stat, max ∷ Stat }
                     , effect   ∷ Array ActiveEffect
                     , limited  ∷ Boolean
                     }

{-
TEMPLATE BELOW

, { name:     ?_
  , id:       ?_
  , rarity:   ?_
  , stats:    { base: { atk: ?_, hp: ?_ }
              , max:  { atk: ?_, hp: ?_ }
              }
  , effect:   ?_
  , limited:  false
  }
-}

craftEssences ∷ Array CraftEssence
craftEssences = [
  { name:     "Tenacity"
  , id:       1
  , rarity:   1
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 300 }
              }
  , effect:   [ Grant Self 0 DefenseUp $ 3.0 ~ 5.0 ]
  , limited:  false
  }
, { name:     "Meditation"
  , id:       2
  , rarity:   1
  , stats:    { base: { atk: 0, hp: 150 }
              , max:  { atk: 0, hp: 450 }
              }
  , effect:   [ Grant Self 0 DebuffResist $ 5.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Technique"
  , id:       3
  , rarity:   1
  , stats:    { base: { atk: 100, hp: 0 }
              , max:  { atk: 300, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Arts) $ 3.0 ~ 5.0 ]
  , limited:  false
  }
, { name:     "Preemption"
  , id:       4
  , rarity:   1
  , stats:    { base: { atk: 100, hp: 0 }
              , max:  { atk: 300, hp: 0 }
              }
  , effect:   [Grant Self 0 (Boost Quick) $ 3.0 ~ 5.0]
  , limited:  false
  }
, { name:     "Destruction"
  , id:       5
  , rarity:   1
  , stats:    { base: { atk: 100, hp: 0 }
              , max:  { atk: 300, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Buster) $ 3.0 ~ 5.0 ]
  , limited:  false
  }
, { name:     "Flash"
  , id:       6
  , rarity:   2
  , stats:    { base: { atk: 150, hp: 0 }
              , max:  { atk: 500, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 5.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Opportunity"
  , id:       7
  , rarity:   2
  , stats:    { base: { atk: 75,  hp: 112 }
              , max:  { atk: 250, hp: 375 }
              }
  , effect:   [ Grant Self 0 StarUp $ 5.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Fruitful"
  , id:       8
  , rarity:   2
  , stats:    { base: { atk: 75,  hp: 112 }
              , max:  { atk: 250, hp: 375 }
              }
  , effect:   [ To Self GaugeUp $ 10.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Concentration"
  , id:       9
  , rarity:   2
  , stats:    { base: { atk: 75,  hp: 112 }
              , max:  { atk: 250, hp: 375 }
              }
  , effect:   [ Grant Self 0 NPGen $ 5.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Divine Oracle"
  , id:       10
  , rarity:   2
  , stats:    { base: { atk: 150, hp: 0 }
              , max:  { atk: 500, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 5.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Azoth Blade"
  , id:       11
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 200 }
              , max:  { atk: 0, hp: 1000 }
              }
  , effect:   [ Grant Self 0 DefenseUp $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "False Attendant's Writings"
  , id:       12
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DebuffResist $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "The Azure Black Keys"
  , id:       13
  , rarity:   3
  , stats:    { base: { atk: 200, hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Arts) $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "The Verdant Black Keys"
  , id:       14
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Quick) $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "The Crimson Black Keys"
  , id:       15
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Buster) $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Rin's Pendant"
  , id:       16
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Spell Tome"
  , id:       17
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 StarUp $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Dragon's Meridian"
  , id:       18
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ To Self GaugeUp $ 30.0 ~ 50.0 ]
  , limited:  false
  }
, { name:     "Sorcery Ore"
  , id:       19
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 NPGen $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Dragonkin"
  , id:       20
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Iron-Willed Training"
  , id:       21
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 400 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DefenseUp $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Primeval Curse"
  , id:       22
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 DebuffResist $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Projection"
  , id:       23
  , rarity:   4
  , stats:    { base: { atk: 400, hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Arts) $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Gandr"
  , id:       24
  , rarity:   4
  , stats:    { base: { atk: 400, hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Quick) $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Verdant Sound of Destruction"
  , id:       25
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Buster) $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Gem Magecraft:  Antumbra"
  , id:       26
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 1500 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 CritUp $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Be Elegant"
  , id:       27
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Grant Self 0 StarUp $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "The Imaginary Element"
  , id:       28
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ To Self GaugeUp $ 60.0 ~ 75.0 ]
  , limited:  false
  }
, { name:     "Divine Banquet"
  , id:       29
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Grant Self 0 NPGen $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Angel's Song"
  , id:       30
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Formal Craft"
  , id:       31
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Arts) $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Imaginary Around"
  , id:       12
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Quick) $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Limited/Zero Over"
  , id:       33
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Buster) $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Kaleidoscope"
  , id:       34
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ To Self GaugeUp $ 80.0 ~ 100.0 ]
  , limited:  false
  }
, { name:     "Heaven's Feel"
  , id:       35
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 40.0 ~ 50.0 ]
  , limited:  false
  }
, { name:     "Beginning of the Journey"
  , id:       36
  , rarity:   4
  , stats:    { base: { atk: 50,  hp: 50 }
              , max:  { atk: 50, hp: 50 }
              }
  , effect:   [ Bonus FriendPoints $ Flat 75.0 ]
  , limited:  false
  }
, { name:     "Parted Sea"
  , id:       37
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 Evasion $ Flat 1.0
              , Grant Self 0 DebuffResist $ 5.0 ~ 10.0
             ]
  , limited:  false
  }
, { name:     "Seal Designation Enforcer"
  , id:       38
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 StarUp $ 600.0 ~ 800.0 ]
  , limited:  false
  }
, { name:     "Holy Shroud of Magdalene"
  , id:       39
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 400 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (DefenseUpVs Male) $ 25.0 ~ 35.0 ]
  , limited:  false
  }
, { name:     "Prisma Cosmos"
  , id:       40
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 375 }
              , max:  { atk: 1000, hp: 1500 }
              }
  , effect:   [ Grant Self 0 GaugePerTurn $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Nightless Rose"
  , id:       41
  , rarity:   5
  , stats:    { base: { atk: 0,   hp: 0 }
              , max:  { atk: 500, hp: 2000 }
              }
  , effect:   [ Grant Self 0 Guts $ 500.0 ~ 1000.0 ]
  , limited:  false
  }
, { name:     "Mooncell Automaton"
  , id:       42
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 Guts $ 500.0 ~ 1000.0 ]
  , limited:  false
  }
, { name:     "Moony Jewel"
  , id:       43
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 (Resist Charm) $ 80.0 ~ 100.0 ]
  , limited:  false
  }
, { name:     "Moon Goddess' Bath"
  , id:       44
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 500 }
              , max:  { atk: 0, hp: 2000 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 500.0 ~ 750.0 ]
  , limited:  false
  }
, { name:     "Moonlight Fest"
  , id:       45
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 375 }
              , max:  { atk: 1000, hp: 1500 }
              }
  , effect:   [ Grant Self 0 StarUp $ 15.0 ~ 20.0
              , Grant Self 0 CritUp $ 15.0 ~ 20.0
              ]
  , limited:  false
  }
, { name:     "Runestones"
  , id:       46
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 DebuffResist $ 5.0 ~ 10.0
              , Grant Self 0 StarUp $ 100.0 ~ 200.0
              ]
  , limited:  false
  }
, { name:     "With One Strike"
  , id:       47
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 SureHit Full
              , Grant Self 0 (Boost Quick) $ 8.0 ~ 10.0
              ]
  , limited:  false
  }
, { name:     "The Black Grail"
  , id:       48
  , rarity:   5
  , stats:    { base: { atk: 600,  hp: 0 }
              , max:  { atk: 2400, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 60.0 ~ 80.0
              , Debuff Self 0 HealthLoss $ Flat 500.0
              ]
  , limited:  false
  }
, { name:     "Jack-o'-lantern"
  , id:       49
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 DamageUp $ 100.0 ~ 200.0 ]
  , limited:  false
  }
, { name:     "Trick or Treat"
  , id:       50
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 DebuffSuccess $ 10.0 ~ 12.0 ]
  , limited:  false
  }
, { name:     "Halloween Arrangement"
  , id:       51
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 1 Taunt Full
              , Grant Self 1 DefenseUp $ 60.0 ~ 80.0
              ]
  , limited:  false
  }
, { name:     "Halloween Princess"
  , id:       52
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 375 }
              , max:  { atk: 1000, hp: 1500 }
              }
  , effect:   [ Grant Self 0 NPUp $ 15.0 ~ 20.0
              , To Self GaugeUp $ 30.0 ~ 50.0
              ]
  , limited:  false
  }
, { name:     "Little Halloween Devil"
  , id:       53
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 20.0 ~ 25.0
              , To Self GaugeUp $ 50.0 ~ 60.0
              ]
  , limited:  false
  }
, { name:     "Maid in Halloween"
  , id:       54
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  -- TODO "Increase HP Recovery"
  , effect:   [ Grant Self 0 HealingReceived $ 60.0 ~ 75.0 ]
  , limited:  false
  }
, { name:     "Anchors Aweigh"
  , id:       55
  , rarity:   3
  , stats:    { base: { atk: 300,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 100.0 ~ 200.0 ]
  , limited:  false
  }
, { name:     "Code Cast"
  , id:       56
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Grant Self 3 AttackUp $ 25.0 ~ 30.0
              , Grant Self 3 DefenseUp $ 25.0 ~ 30.0
              ]
  , limited:  false
  }
, { name:     "Victor of the Moon"
  , id:       57
  , rarity:   5
  , stats:    { base: { atk: 600,  hp: 0 }
              , max:  { atk: 2400, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Buster) $ 10.0 ~ 15.0
              , Grant Self 0 CritUp $ 20.0 ~ 25.0
              ]
  , limited:  false
  }
, { name:     "Another Ending"
  , id:       58
  , rarity:   5
  , stats:    { base: { atk: 600,  hp: 0 }
              , max:  { atk: 2400, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Arts) $ 10.0 ~ 15.0
              , Grant Self 0 CritUp $ 20.0 ~ 25.0
              ]
  , limited:  false
  }
, { name:     "Fate GUDAGUDA Order"
  , id:       59
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 (Boost Quick) $ 1.0 ~ 2.0
              , Grant Self 0 (Boost Arts) $ 1.0 ~ 2.0
              , Grant Self 0 (Boost Buster) $ 1.0 ~ 2.0
              , Grant Self 0 StarUp $ 1.0 ~ 2.0
              , Grant Self 0 StarWeight $ 1.0 ~ 2.0
              , Grant Self 0 NPGen $ 1.0 ~ 2.0
              , Grant Self 0 NPUp $ 1.0 ~ 2.0
              , Grant Self 0 DebuffSuccess $ 1.0 ~ 2.0
              , Grant Self 0 DebuffResist $ 1.0 ~ 2.0
              , Grant Self 0 HealingReceived $ 1.0 ~ 2.0
              ]
  , limited:  false
  }
, { name:     "After-Party Order!"
  , id:       60
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Quick) $ 10.0 ~ 15.0
              , Grant Self 0 (Boost Buster) $ 10.0 ~ 15.0
              ]
  , limited:  false
  }
, { name:     "Guda-O"
  , id:       61
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 15.0 ~ 20.0
              , Grant Self 0 NPUp $ 15.0 ~ 20.0
              ]
  , limited:  false
  }
, { name:     "GUDAGUDA Poster Girl"
  , id:       62
  , rarity:   5
  , stats:    { base: { atk: 0,   hp: 0 }
              , max:  { atk: 750, hp: 3000 }
              }
  , effect:   [ Grant Self 3 Taunt Full
              , Grant Self 3 AttackUp $ 60.0 ~ 80.0
              ]
  , limited:  false
  }
, { name:     "Demonic Boar"
  , id:       65
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Boost Quick) $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Knight's Dignity"
  , id:       66
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Grant Self 0 CritUp $ 40.0 ~ 50.0
              , Debuff Self 0 DefenseDown $ Flat 20.0
              ]
  , limited:  false
  }
, { name:     "A Fragment of 2030"
  , id:       67
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Self 0 StarsPerTurn $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Lightning Reindeer"
  , id:       68
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 3 (Boost Buster) $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "March of the Saint"
  , id:       69
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 200.0 ~ 300.0
              , Grant Self 0 GaugePerTurn $ 3.0 ~ 4.0
              ]
  , limited:  false
  }
, { name:     "Present for My Master"
  , id:       70
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Self 0 StarUp $ 100.0 ~ 200.0
              , Grant Self 0 HealingReceived $ 40.0 ~ 50.0
              ]
  , limited:  false
  }
, { name:     "Holy Night Sign"
  , id:       71
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 375 }
              , max:  { atk: 1000, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (Boost Quick) $ 8.0 ~ 10.0
              , Grant Self 0 CritUp $ 15.0 ~ 20.0
              ]
  , limited:  false
  }
, { name:     "Clock Tower"
  , id:       72
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 GaugePerTurn $ 2.0 ~ 3.0 ]
  , limited:  false
  }
, { name:     "Necromancy"
  , id:       73
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Chance 50 ∘ Grant Self 0 Guts $ 500.0 ~ 1000.0 ]
  , limited:  false
  }
, { name:     "Awakened Will"
  , id:       74
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Chance 60 ∘ Grant Self 0 GaugePerTurn $ 12.0 ~ 15.0
              , Debuff Self 0 HealthLoss $ Flat 500.0
              ]
  , limited:  false
  }
, { name:     "500-Year Obsession"
  , id:       75
  , rarity:   5
  , stats:    { base: { atk: 600,  hp: 0 }
              , max:  { atk: 2400, hp: 0 }
              }
  , effect:   [ When "defeated by an enemy" $ Debuff Enemy 2 SealNP Full
              , When "defeated by an enemy"
                ∘ Debuff Enemy 10 Curse $ 1000.0 ~ 2000.0
              ]
  , limited:  false
  }
, { name:     "Peacefulness of 2018"
  , id:       76
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 200.0 ~ 300.0
              , Debuff Self 0 AttackDown $ Flat 10.0
              ]
  , limited:  false
  }
, { name:     "Heroic New Year"
  , id:       77
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 DebuffImmunity $ Flat 1.0
              , Grant Self 0 DefenseUp $ 8.0 ~ 10.0
              ]
  , limited:  false
  }
, { name:     "Law of the Jungle"
  , id:       78
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Bonus QuestQuartz $ 2017.0 ~ 2018.0 ]
  , limited:  false
  }
, { name:     "Grand New Year"
  , id:       79
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Self 1 Taunt Full
              , Grant Self 1 Invincibility Full
              , Grant Self 0 DebuffResist $ 10.0 ~ 20.0
              ]
  , limited:  false
  }
, { name:     "Mona Lisa"
  , id:       80
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Bonus QPGain $ 2.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Happy x3 Order"
  , id:       81
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 2016 }
              , max:  { atk: 0, hp: 2016 }
              }
  , effect:   [ Grant Self 0 StarsPerTurn $ 0.0 ~ 1.0 ]
  , limited:  false
  }
, { name:     "Purely Bloom"
  , id:       82
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ -- TODO  +5% NP damage per turn (40~50% cap)
              ]
  , limited:  false
  }
, { name:     "Star of Altria"
  , id:       83
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 Guts $ Flat 1.0
              , Grant Self 0 DebuffResist $ 5.0 ~ 10.0
              ]
  , limited:  false
  }
, { name:     "Trueshot"
  , id:       84
  , rarity:   3
  , stats:    { base: { atk: 200, hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 SureHit Full
              , Grant Self 0 CritUp $ 3.0 ~ 5.0
              ]
  , limited:  false
  }
, { name:     "Mikotto! Bride Training"
  , id:       85
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Chance 65 ∘ Grant Self 0 HealPerTurn $ 750.0 ~ 1000.0 ]
  , limited:  false
  }
, { name:     "The Crimson Land of Shadows"
  , id:       86
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ -- TODO Add 100 Damage per turn (max 1000~1200)

              ]
  , limited:  false
  }
, { name:     "Ryudoji Temple"
  , id:       89
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 160 }
              , max:  { atk: 500, hp: 800 }
              }
  , effect:   [ Grant Self 0 NPUp $ 10.0 ~ 15.0
              , To Self GaugeUp $ 20.0 ~ 30.0
              ]
  , limited:  false
  }
, { name:     "Mana Gauge"
  , id:       90
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1200, hp: 0  }
              }
  , effect:   [ Grant Self 0 (ClassAffinity Caster) $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Elixir of Love"
  , id:       91
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 160 }
              , max:  { atk: 500, hp: 800 }
              }
  , effect:   [ Grant Self 0 CharmSuccess $ 12.0 ~ 15.0 ]
  , limited:  false
  }
, { name:     "Storch Ritter"
  , id:       92
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ When "equipped by a Berserker"
                ∘ Grant Self 0 NPUp $ 15.0 ~ 25.0 ]
  , limited:  false
  }
, { name:     "Hermitage"
  , id:       93
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 3 (Boost Arts) $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Motored Cuirassier"
  , id:       94
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (ClassAffinity Rider) $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Stuffed Lion"
  , id:       95
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ When "defeated" ∘ To Allies Heal $ 800.0 ~ 1000.0 ]
  , limited:  false
  }
, { name:     "Lugh's Halo"
  , id:       96
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (Resist Stun) $ 25.0 ~ 30.0 ]
  , limited:  false
  }
]
