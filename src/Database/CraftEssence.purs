module Database.CraftEssence (CraftEssence(..), craftEssences) where

import Prelude

import Database.Base (Card(..), Class(..), Stat, Trait(..))
import Database.Skill (ActiveEffect(..), Amount(..), BonusEffect(..), BuffEffect(..), DebuffEffect(..), InstantEffect(..), Target(..), (~))

type CraftEssence = { name     ∷ String
                     , id       ∷ Int
                     , rarity   ∷ Int
                     , stats    ∷ { base ∷ Stat, max ∷ Stat }
                     , effect   ∷ Array ActiveEffect
                     , limited  ∷ Boolean
                     }

equipped ∷ Class -> ActiveEffect -> ActiveEffect
equipped = When <<< ("equipped by a " <> _) <<< show

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
  , effect:   [ Grant Self 0 (Performance Arts) $ 3.0 ~ 5.0 ]
  , limited:  false
  }
, { name:     "Preemption"
  , id:       4
  , rarity:   1
  , stats:    { base: { atk: 100, hp: 0 }
              , max:  { atk: 300, hp: 0 }
              }
  , effect:   [Grant Self 0 (Performance Quick) $ 3.0 ~ 5.0]
  , limited:  false
  }
, { name:     "Destruction"
  , id:       5
  , rarity:   1
  , stats:    { base: { atk: 100, hp: 0 }
              , max:  { atk: 300, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 3.0 ~ 5.0 ]
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
  , effect:   [ Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "The Verdant Black Keys"
  , id:       14
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "The Crimson Black Keys"
  , id:       15
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 8.0 ~ 10.0 ]
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
  , effect:   [ Grant Self 0 (Performance Arts) $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Gandr"
  , id:       24
  , rarity:   4
  , stats:    { base: { atk: 400, hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Verdant Sound of Destruction"
  , id:       25
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 15.0 ~ 20.0 ]
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
  , effect:   [ Grant Self 0 (Performance Arts) $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Imaginary Around"
  , id:       12
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Limited/Zero Over"
  , id:       33
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 25.0 ~ 30.0 ]
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
  , limited:  true
  }
, { name:     "Parted Sea"
  , id:       37
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Times 1 $ Grant Self 0 Evasion Full
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
  , effect:   [ Grant Self 0 (DefenseVs Male) $ 25.0 ~ 35.0 ]
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
  , effect:   [ Times 1 <<< Grant Self 0 Guts $ 500.0 ~ 1000.0 ]
  , limited:  true
  }
, { name:     "Mooncell Automaton"
  , id:       42
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 } 
              }
  , effect:   [ Times 1 <<< Grant Self 0 Guts $ 500.0 ~ 1000.0 ]
  , limited:  false
  }
, { name:     "Moony Jewel"
  , id:       43
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 (Resist Charm) $ 80.0 ~ 100.0 ]
  , limited:  true
  }
, { name:     "Moon Goddess' Bath"
  , id:       44
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 500 }
              , max:  { atk: 0, hp: 2000 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 500.0 ~ 750.0 ]
  , limited:  true
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
  , limited:  true
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
              , Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0
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
  , limited:  true
  }
, { name:     "Trick or Treat"
  , id:       50
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 DebuffSuccess $ 10.0 ~ 12.0 ]
  , limited:  true
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
  , limited:  true
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
  , limited:  true
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
  , limited:  true
  }
, { name:     "Maid in Halloween"
  , id:       54
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  -- TODO "Increase HP Recovery"
  , effect:   [ Grant Self 0 HealingReceived $ 60.0 ~ 75.0 ]
  , limited:  true
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
  , effect:   [ Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
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
  , effect:   [ Grant Self 0 (Performance Arts) $ 10.0 ~ 15.0
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
  , effect:   [ Grant Self 0 (Performance Quick) $ 1.0 ~ 2.0
              , Grant Self 0 (Performance Arts) $ 1.0 ~ 2.0
              , Grant Self 0 (Performance Buster) $ 1.0 ~ 2.0
              , Grant Self 0 StarUp $ 1.0 ~ 2.0
              , Grant Self 0 StarWeight $ 1.0 ~ 2.0
              , Grant Self 0 NPGen $ 1.0 ~ 2.0
              , Grant Self 0 NPUp $ 1.0 ~ 2.0
              , Grant Self 0 DebuffSuccess $ 1.0 ~ 2.0
              , Grant Self 0 DebuffResist $ 1.0 ~ 2.0
              , Grant Self 0 HealingReceived $ 1.0 ~ 2.0
              ]
  , limited:  true
  }
, { name:     "After-Party Order!"
  , id:       60
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0
              , Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              ]
  , limited:  true
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
  , limited:  true
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
  , limited:  true
  }
, { name:     "Demonic Boar"
  , id:       65
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 15.0 ~ 20.0 ]
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
  , effect:   [ Grant Party 0 StarsPerTurn $ 8.0 ~ 10.0 ]
  , limited:  false
  }
, { name:     "Lightning Reindeer"
  , id:       68
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 3 (Performance Buster) $ 15.0 ~ 20.0 ]
  , limited:  true
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
  , limited:  true
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
  , limited:  true
  }
, { name:     "Holy Night Sign"
  , id:       71
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 375 }
              , max:  { atk: 1000, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0
              , Grant Self 0 CritUp $ 15.0 ~ 20.0
              ]
  , limited:  true
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
  , effect:   [ Chance 50 <<< Times 1 <<< Grant Self 0 Guts $ 500.0 ~ 1000.0 ]
  , limited:  false
  }
, { name:     "Awakened Will"
  , id:       74
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Chance 60 <<< Grant Self 0 GaugePerTurn $ 12.0 ~ 15.0
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
  , effect:   [ When "defeated by an enemy" $ Debuff Killer 2 SealNP Full
              , When "defeated by an enemy"
                <<< Debuff Killer 10 Curse $ 1000.0 ~ 2000.0
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
  , limited:  true
  }
, { name:     "Heroic New Year"
  , id:       77
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Times 1 $ Grant Self 0 DebuffResist Full
              , Grant Self 0 DefenseUp $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Law of the Jungle"
  , id:       78
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Bonus QuestQuartz $ 2017.0 ~ 2018.0 ]
  , limited:  true
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
  , limited:  true
  }
, { name:     "Mona Lisa"
  , id:       80
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Bonus QPGain $ 2.0 ~ 10.0 ]
  , limited:  true
  }
, { name:     "Happy x3 Order"
  , id:       81
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 2016 }
              , max:  { atk: 0, hp: 2016 }
              }
  , effect:   [ Grant Party 0 StarsPerTurn $ 0.0 ~ 1.0 ]
  , limited:  true
  }
, { name:     "Purely Bloom"
  , id:       82
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ -- TODO  +5% NP damage per turn (40~50% cap)
              ]
  , limited:  true
  }
, { name:     "Star of Altria"
  , id:       83
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Times 1 <<< Grant Self 0 Guts $ Flat 1.0
              , Grant Self 0 DebuffResist $ 5.0 ~ 10.0
              ]
  , limited:  true
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
  , limited:  true
  }
, { name:     "Mikotto! Bride Training"
  , id:       85
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Chance 65 <<< Grant Self 0 HealPerTurn $ 750.0 ~ 1000.0 ]
  , limited:  true
  }
, { name:     "The Crimson Land of Shadows"
  , id:       86
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ -- TODO Add 100 Damage per turn (max 1000~1200)

              ]
  , limited:  true
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
  , effect:   [ Grant Self 0 (Success Charm) $ 12.0 ~ 15.0 ]
  , limited:  false
  }
, { name:     "Storch Ritter"
  , id:       92
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ equipped Berserker <<< Grant Self 0 NPUp $ 15.0 ~ 25.0 ]
  , limited:  false
  }
, { name:     "Hermitage"
  , id:       93
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 3 (Performance Arts) $ 15.0 ~ 20.0 ]
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
  , effect:   [ When "defeated" <<< To Party Heal $ 800.0 ~ 1000.0 ]
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
, { name:     "Vessel of the Saint"
  , id:       97
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Times 3 $ Grant Self 0 DebuffResist Full
              , Grant Self 0 NPGen $ 15.0 ~ 20.0
              ]
  , limited:  false
  }
, { name:     "Golden Millennium Tree"
  , id:       98
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ -- Increase max HP by 200~300 per turn (Max: 3000)
              ]
  , limited:  false
  }
, { name:     "Valentine Dojo of Tears"
  , id:       109
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 SureHit Full 
              , Grant Self 0 GaugePerTurn $ 3.0 ~ 5.0
              , Debuff Self 0 CharmVuln $ Flat 10.0
              ]
  , limited:  true
  }
, { name:     "Kitchen☆Patissiere"
  , id:       110
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 StarUp $ 15.0 ~ 20.0 
              , Grant Self 0 NPGen $ 15.0 ~ 20.0
              ]
  , limited:  true
  }
, { name:     "Street Choco-Maid"
  , id:       111
  , rarity:   5
  , stats:    { base: { atk: 250, hp: 1000 }
              , max:  { atk: 400, hp: 1600 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 10.0 ~ 15.0 
              , Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0 
              , Grant Self 0 HealingReceived $ 20.0 ~ 30.0
              ]
  , limited:  true
  }
, { name:     "Melty Sweetheart"
  , id:       112
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Times 3 <<< Grant Self 0 (DefenseVs Male) $ Flat 100.0
              , Grant Self 0 StarUp $ 10.0 ~ 20.0 
              ]
  , limited:  true
  }
, { name:     "Decapitating Bunny 2018"
  , id:       154
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 IgnoreInvinc Full 
              , Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Mature Gentleman"
  , id:       155
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Self 0 KillResist $ 60.0 ~ 80.0 ]
  , limited:  true
  }
, { name:     "Grand Puppeteer"
  , id:       156
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ To Self GaugeUp $ 50.0 ~ 60.0 
              , Grant Self 3 (Performance Arts) $ 15.0 ~ 20.0
              ]
  , limited:  true
  }
, { name:     "Threefold Barrier"
  , id:       157
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Times 3 <<< Grant Self 0 DamageCut $ 1000.0 ~ 1200.0 ]
  , limited:  true
  }
, { name:     "Vivid Dance of Fists"
  , id:       158
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 DamageUp $ 800.0 ~ 1000.0 ]
  , limited:  true
  }
, { name:     "Mystic Eyes of Distortion"
  , id:       159
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 20.0 ~ 25.0 
              , Debuff Self 0 DefenseDown $ Flat 15.0
              ]
  , limited :  true
  }
, { name:     "Summer's Futuresight"
  , id:       160
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Times 1 $ Grant Self 0 Evasion Full 
              , Grant Self 0 StarUp $ 15.0 ~ 20.0 
              ]
  , limited:  true
  }
, { name:     "Refrain"
  , id:       161
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 StarWeight $ 300.0 ~ 400.0 
              , Grant Self 3 DebuffResist $ 15.0 ~ 20.0
              ]
  , limited:  true
  }
, { name:     "Sprinter"
  , id:       162
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 5.0 ~ 8.0 
              , Grant Self 0 DebuffResist $ 10.0 ~ 15.0 
              ]
  , limited:  true
  }
, { name:     "Repeat Magic"
  , id:       163
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ To Self GaugeUp $ 20.0 ~ 30.0 
              , Grant Self 0 NPGen $ 10.0 ~ 15.0
              ]
  , limited:  true
  }
, { name:     "Kiss Your Hand"
  , id:       165
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 10.0 ~ 12.0 
              , Grant Self 0 (Performance Buster) $ 10.0 ~ 12.0 
              , Grant Self 0 (Performance Quick) $ 10.0 ~ 12.0 
              ]
  , limited:  true
  }
, { name:     "Teacher and I"
  , id:       166
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ To Self GaugeUp $ 5.0 ~ 60.0 
              , Grant Self 0 StarWeight $ 300.0 ~ 400.0
              ]
  , limited:  true
  }
, { name:     "Versus"
  , id:       167
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 3 (AttackVs Divine) $ 80.0 ~ 100.0 
              , Grant Self 3 (DefenseVs Divine) $ 40.0 ~ 50.0
              ]
  , limited:  true
  }
, { name:     "Beasts Under the Moon"
  , id:       168
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPGen $ 12.0 ~ 15.0 
              , Grant Self 0 StarUp $ 12.0 ~ 15.0
              , Grant Self 0 HealPerTurn $ 200.0 ~ 300.0
              ]
  , limited:  true
  }
, { name:     "Glass Full Sweet Time"
  , id:       169
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 SureHit Full 
              , Grant Self 0 DamageUp $ 400.0 ~ 600.0
              , Grant Self 0 DamageCut $ 200.0 ~ 300.0
              ]
  , limited:  true
  }
, { name:     "Salon de Marie"
  , id:       170
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Times 1 $ Grant Self 0 Evasion Full 
              , Grant Self 0 HealingReceived $ 5.0 ~ 10.0
              , Grant Self 0 DebuffSuccess $ 3.0 ~ 5.0
              ] 
  , limited:  true
  }
, { name:     "Prince of Slayer"
  , id:       171
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 160 }
              , max:  { atk: 500, hp: 800 }
              }
  , effect:   [ Grant Party 0 StarsPerTurn $ 1.0 ~ 2.0 
              , Grant Self 0 (AttackVs Dragon) $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Noisy Obsession"
  , id:       172
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 15.0 ~ 20.0 
              , Grant Self 0 NPUp $ 15.0 ~ 20.0
              , Grant Self 0 (Success Charm) $ 12.0 ~ 15.0
              ]
  , limited:  true
  }
, { name:     "Ideal Holy King"
  , id:       175
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Party 0 MaxHP $ 1000.0 ~ 1200.0 ]
  , limited:  false
  }
, { name:     "Record Holder"
  , id:       176
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 DebuffSuccess $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Beast of Billows"
  , id:       177
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ equipped Lancer <<< Grant Self 0 NPUp $ 15.0 ~ 25.0  ]
  , limited:  false
  }
, { name:     "Personal Training"
  , id:       178
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Bonus EXP $ 2.0 ~ 10.0 ]
  , limited:  true
  }
, { name:     "The Scholars of Chaldea"
  , id:       15
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ To Self GaugeUp $ 30.0 ~ 50.0 
              , Grant Self 0 HealingReceived $ 20.0 ~ 30.0
              ]
  , limited:  true
  }
, { name:     "Maiden Leading Chaldea"
  , id:       180
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0 
              , Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              ]
  , limited:  true
  }
, { name:     "The Merciless One"
  , id:       181
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ When "defeated" <<< To Party GaugeUp $ 15.0 ~ 20.0 
              , Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              ]
  , limited:  true
  }
, { name:     "Art of the Poisonous Snake"
  , id:       182
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 3 (Performance Arts) $ 30.0 ~ 40.0 ]
  , limited:  false
  }
, { name:     "Art of Death"
  , id:       183
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (AttackVs Humanoid) $ 25.0 ~ 30.0 ]
  , limited:  false
  }
, { name:     "Gentle Affection"
  , id:       184
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 HealUp $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Volumen Hydrargyrum"
  , id:       185
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Times 3 $ Grant Self 0 Invincibility Full ]
  , limited:  false
  }
, { name:     "Innocent Maiden"
  , id:       186
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 GaugePerTurn $ 4.0 ~ 5.0 
              , Grant Self 0 (Performance Quick) $ 10.0 ~ 12.0
              ]
  , limited:  false
  }
, { name:     "Self Geas Scroll"
  , id:       187
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (Success Stun) $ 12.0 ~ 15.0 ]
  , limited:  false
  }
, { name:     "Before Awakening"
  , id:       188
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0 
              , Grant Self 0 (Performance Buster) $ 8.0 ~ 10.0 
              , Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0 
              ]
  , limited:  false
  }
, { name:     "His Rightful Place"
  , id:       189
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0 
              , To Self GaugeUp $ 30.0 ~ 50.0
              ]
  , limited:  true
  }
, { name:     "The Wandering Tales of Shana-oh"
  , id:       211
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0 
              , When "defeated" 
                <<< Grant Party 1 (Performance Quick) $ 20.0 ~ 30.0
              ]
  , limited:  true
  }
, { name:     "Golden Captures the Carp"
  , id:       212
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ To Self GaugeUp $ 30.0 ~ 50.0 
              , To Party GainStars $ 15.0 ~ 20.0
              ]
  , limited:  true
  }
, { name:     "A Fox Night's Dream"
  , id:       213
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 NPGen $ 20.0 ~ 25.0 
              , Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0
              ]
  , limited:  true
  }
, { name:     "Burning Tale of Love"
  , id:       214
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (AttackVs Male) $ 25.0 ~ 30.0 
              , Grant Self 0 DebuffSuccess $ 12.0 ~ 15.0
              ]
  , limited:  true
  }
, { name:     "Reciting the Subscription List"
  , id:       215
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Times 1 $ Grant Self 0 DebuffResist Full ]
  , limited:  true
  }
, { name:     "GO WEST!!"
  , id:       226
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 NPUp $ 20.0 ~ 25.0 
              , Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0
              ]
  , limited:  true
  }
, { name:     "The Classic Three Great Heroes"
  , id:       227
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 NPUp $ 15.0 ~ 20.0 
              , Grant Self 0 StarUp $ 15.0 ~ 20.0
              , To Self GaugeUp $ 25.0 ~ 40.0  
              ]
  , limited:  true
  }
, { name:     "True Samadhi Fire"
  , id:       228
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 15.0 ~ 20.0 
              , Grant Self 0 (Performance Buster) $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "All Three Together"
  , id:       229
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 StarWeight $ 100.0 ~ 200.0 
              , Grant Self 0 CritUp $ 5.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Divine Princess of the Storm"
  , id:       240
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ When "defeated" <<< Grant Party 3 DefenseUp $ 20.0 ~ 25.0 ]
  , limited:  true
  }
, { name:     "Ox-Demon King"
  , id:       241
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Party 3 (Performance Buster) $ 10.0 ~ 15.0 ]
  , limited:  true
  }
, { name:     "Personal Lesson"
  , id:       242
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Bonus MysticCode $ Flat 2.0 ]
  , limited:  true
  }
, { name:     "Bronze-Link Manipulator"
  , id:       243
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 3 AttackUp $ 15.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Ath nGabla"
  , id:       244
  , rarity:   3
  , stats:    { base: { atk: 100, hp: 160 }
              , max:  { atk: 500, hp: 800 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0 
              , Debuff Self 0 DefenseDown $ Flat 10.0
              ]
  , limited:  false
  }
, { name:     "Bygone Dreams"
  , id:       245
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ equipped Assassin <<< Grant Self 0 NPUp $ 15.0 ~ 25.0 ]
  , limited:  false
  }
, { name:     "Extremely Spicy Mapo Tofu"
  , id:       246
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 HealingReceived $ 10.0 ~ 20.0 ]
  , limited:  false
  }
, { name:     "Dumplings Over Flowers"
  , id:       258
  , rarity:   5
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 15.0 ~ 20.0 ]
  , limited:  true
  }
, { name:     "Faithful Companions"
  , id:       259
  , rarity:   4
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0 
              , Grant Self 0 NPGen $ 15.0 ~ 20.0
              ]
  , limited:  true
  }
, { name:     "Hidden Sword: Pheasant Reversal"
  , id:       260
  , rarity:   3
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 3.0 ~ 5.0 
              , Grant Self 0 CritUp $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Golden Sumo: Boulder Tournament"
  , id:       261
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 AttackUp $ 10.0 ~ 15.0 
              , To Self GaugeUp $ 30.0 ~ 50.0
              ]
  , limited:  true
  }
, { name:     "Hot Spring Under the Moon"
  , id:       262
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 20.0 ~ 25.0 
              , Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0
              ]
  , limited:  true
  }
, { name:     "Origin Bullet"
  , id:       263
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 IgnoreInvinc Full 
              , Grant Self 0 (ClassAffinity Caster) $ 35.0 ~ 40.0
              ]
  , limited:  false
  }
, { name:     "Covering Fire"
  , id:       264
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 DamageUp $ 400.0 ~ 600.0 
              , Grant Self 0 CritUp $ 15.0 ~ 20.0
              ]
  , limited:  false
  }
, { name:     "Battle of Camlann"
  , id:       265
  , rarity:   3
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ When "defeated" <<< To Party GaugeUp $ 10.0 ~ 15.0 ]
  , limited:  false
  }
, { name:     "Anniversary Heroines"
  , id:       276
  , rarity:   4
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Grant Self 0 AttackUp $ Flat 10.0 
              , Grant Self 0 StarsPerTurn $ Flat 3.0
              ]
  , limited:  true
  }
, { name:     "Leisure Stroll"
  , id:       277
  , rarity:   5
  , stats:    { base: { atk: 400,  hp: 250 }
              , max:  { atk: 1600, hp: 1000 }
              }
  , effect:   [ Grant Self 0 StarUp $ 400.0 ~ 600.0 
              , Grant Self 0 (Performance Arts) $ 10.0 ~ 15.0
              ]
  , limited:  true
  }
, { name:     "Partake with the King"
  , id:       278
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0  
              , To Self GaugeUp $ 50.0 ~ 60.0
              ]
  , limited:  true
  }
, { name:     "Goldfish Scooping"
  , id:       279
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 SureHit Full 
              , Grant Self 0 (Performance Buster) $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Fire Flower"
  , id:       280
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 StarUp $ 5.0 ~ 10.0 
              , Grant Self 0 CritUp $ 5.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Pirates Party!"
  , id:       291
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 IgnoreInvinc Full 
              , Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0
              ]
  , limited:  true
  }
, { name:     "Summertime Mistress"
  , id:       292
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 15.0 ~ 20.0 
              , To Self GaugeUp $ 30.0 ~ 50.0
              ]
  , limited:  true
  }
, { name:     "Twilight Memory"
  , id:       293
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Times 1 $ Grant Self 0 Evasion Full 
              , Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Shiny Goddess"
  , id:       294
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DefenseUp $ 3.0 ~ 5.0 
              , Grant Self 0 (Performance Arts) $ 3.0 ~ 5.0
              ]
  , limited:  true
  }
, { name:     "Knights of Marines"
  , id:       295
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0 
              , To Self GaugeUp $ 50.0 ~ 60.0
              ]
  , limited:  true
  }
, { name:     "Chaldea Lifesavers"
  , id:       296
  , rarity:   5
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Times 1 <<< Grant Self 0 Guts $ Flat 1.0 
              , Grant Self 0 NPGen $ 15.0 ~ 20.0
              ]
  , limited:  true
  }
, { name:     "Meat Wars"
  , id:       297
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 200.0 ~ 300.0 
              , Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Shaved Ice (Void's Dust Flavor)"
  , id:       298
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DamageCut $ 100.0 ~ 200.0 
              , Grant Self 0 DebuffResist $ 5.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Magical Girl of Sapphire"
  , id:       309
  , rarity:   5
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPGen $ 25.0 ~ 30.0 
              , To Self GaugeUp $ 40.0 ~ 50.0
              ]
  , limited:  true
  }
, { name:     "Kill on Sight"
  , id:       310
  , rarity:   4
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0 
              , Grant Self 0 NPUp $ 15.0 ~ 20.0
              ]
  , limited:  true
  }
, { name:     "Zunga Zunga!"
  , id:       311
  , rarity:   3
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DamageCut $ 100.0 ~ 200.0 
              , Grant Self 0 HealingReceived $ 5.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Kaleid Ruby"
  , id:       312
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0 
              , Grant Self 0 NPUp $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Kaleid Sapphire"
  , id:       313
  , rarity:   4
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 10.0 ~ 15.0 
              , Grant Self 0 NPUp $ 8.0 ~ 10.0
              ]
  , limited:  true
  }
, { name:     "Fate/Extella"
  , id:       360
  , rarity:   4
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Grant Self 0 CritUp $ Flat 15.0
              , Grant Party 0 StarsPerTurn $ Flat 3.0
              ]
  , limited:  true
  }
, { name:     "Spiritron Portrait: Nero Claudius"
  , id:       361
  , rarity:   4
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , limited:  true
  }
, { name:     "Spiritron Portrait: Nameless"
  , id:       362
  , rarity:   4
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , limited:  true
  }
, { name:     "Spiritron Portrait: Tamamo no Mae"
  , id:       363
  , rarity:   4
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , limited:  true
  }
, { name:     "Spiritron Portrait: Karna"
  , id:       364
  , rarity:   4
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , limited:  true
  }
, { name:     "Spiritron Portrait: Altera"
  , id:       365
  , rarity:   4
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , limited:  true
  }
, { name:     "Spiritron Portrait: Gilgamesh"
  , id:       366
  , rarity:   4
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , limited:  true
  }
, { name:     "First Order"
  , id:       399
  , rarity:   4
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , limited:  true
  }
]
