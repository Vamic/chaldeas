-- | All Craft Essences.
-- Unlike Servants, which are divided into multiple subfiles,
-- Craft Essences are all included in this one file
-- along with their data model definition.
module Database.CraftEssence
  ( class MatchCraftEssence, ceHas
  , CraftEssence(..)
  , craftEssences
  , getBond
  ) where

import StandardLibrary
import Generic  as G
import Data.Int as Int

import Database.Base
import Database.Servant
import Database.Skill


-- | All Craft Essences available in EN.
-- Note: Names _must_ be true to their EN localization.
-- GrandOrder.Wiki is only trustworthy for CEs that have been in the game
-- for a while. Craft Essences introduced during events and the like should be
-- checked against the official announcement.
craftEssences :: Array CraftEssence
craftEssences = CraftEssence <$>
[ { name:     "Tenacity"
  , id:       1
  , rarity:   1
  , icon:     IconShieldUp
  , stats:    { base: { atk: 0, hp: 100 }
              , max:  { atk: 0, hp: 300 }
              }
  , effect:   [ Grant Self 0 DefenseUp $ 3.0 ~ 5.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Meditation"
  , id:       2
  , rarity:   1
  , icon:     IconHoodUp
  , stats:    { base: { atk: 0, hp: 150 }
              , max:  { atk: 0, hp: 450 }
              }
  , effect:   [ Grant Self 0 DebuffResist $ 5.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Technique"
  , id:       3
  , rarity:   1
  , icon:     IconArtsUp
  , stats:    { base: { atk: 100, hp: 0 }
              , max:  { atk: 300, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 3.0 ~ 5.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Preemption"
  , id:       4
  , rarity:   1
  , icon:     IconQuickUp
  , stats:    { base: { atk: 100, hp: 0 }
              , max:  { atk: 300, hp: 0 }
              }
  , effect:   [Grant Self 0 (Performance Quick) $ 3.0 ~ 5.0]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Destruction"
  , id:       5
  , rarity:   1
  , icon:     IconBusterUp
  , stats:    { base: { atk: 100, hp: 0 }
              , max:  { atk: 300, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 3.0 ~ 5.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Flash"
  , id:       6
  , rarity:   2
  , icon:     IconExclamationUp
  , stats:    { base: { atk: 150, hp: 0 }
              , max:  { atk: 500, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 5.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Opportunity"
  , id:       7
  , rarity:   2
  , icon:     IconStarHaloUp
  , stats:    { base: { atk: 75,  hp: 112 }
              , max:  { atk: 250, hp: 375 }
              }
  , effect:   [ Grant Self 0 StarUp $ 5.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Fruitful"
  , id:       8
  , rarity:   2
  , icon:     IconNoble
  , stats:    { base: { atk: 75,  hp: 112 }
              , max:  { atk: 250, hp: 375 }
              }
  , effect:   [ To Self GaugeUp $ 10.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Concentration"
  , id:       9
  , rarity:   2
  , icon:     IconNobleUp
  , stats:    { base: { atk: 75,  hp: 112 }
              , max:  { atk: 250, hp: 375 }
              }
  , effect:   [ Grant Self 0 NPGen $ 5.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Divine Oracle"
  , id:       10
  , rarity:   2
  , icon:     IconBeamUp
  , stats:    { base: { atk: 150, hp: 0 }
              , max:  { atk: 500, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 5.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Azoth Blade"
  , id:       11
  , rarity:   3
  , icon:     IconShieldUp
  , stats:    { base: { atk: 0, hp: 200 }
              , max:  { atk: 0, hp: 1000 }
              }
  , effect:   [ Grant Self 0 DefenseUp $ 8.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "False Attendant's Writings"
  , id:       12
  , rarity:   3
  , icon:     IconHoodUp
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DebuffResist $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "The Azure Black Keys"
  , id:       13
  , rarity:   3
  , icon:     IconArtsUp
  , stats:    { base: { atk: 200, hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "The Verdant Black Keys"
  , id:       14
  , rarity:   3
  , icon:     IconQuickUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "The Crimson Black Keys"
  , id:       15
  , rarity:   3
  , icon:     IconBusterUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 8.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Rin's Pendant"
  , id:       16
  , rarity:   3
  , icon:     IconExclamationUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Spell Tome"
  , id:       17
  , rarity:   3
  , icon:     IconStarHaloUp
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 StarUp $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Dragon's Meridian"
  , id:       18
  , rarity:   3
  , icon:     IconNoble
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ To Self GaugeUp $ 30.0 ~ 50.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Sorcery Ore"
  , id:       19
  , rarity:   3
  , icon:     IconNobleUp
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 NPGen $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Dragonkin"
  , id:       20
  , rarity:   3
  , icon:     IconBeamUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Iron-Willed Training"
  , id:       21
  , rarity:   4
  , icon:     IconShieldUp
  , stats:    { base: { atk: 0, hp: 400 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DefenseUp $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Primeval Curse"
  , id:       22
  , rarity:   4
  , icon:     IconHoodUp
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 DebuffResist $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Projection"
  , id:       23
  , rarity:   4
  , icon:     IconArtsUp
  , stats:    { base: { atk: 400, hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Gandr"
  , id:       24
  , rarity:   4
  , icon:     IconQuickUp
  , stats:    { base: { atk: 400, hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Verdant Sound of Destruction"
  , id:       25
  , rarity:   4
  , icon:     IconBusterUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Gem Magecraft: Antumbra"
  , id:       26
  , rarity:   4
  , icon:     IconExclamationUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Be Elegant"
  , id:       27
  , rarity:   4
  , icon:     IconStarHaloUp
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Grant Self 0 StarUp $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "The Imaginary Element"
  , id:       28
  , rarity:   4
  , icon:     IconNoble
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ To Self GaugeUp $ 60.0 ~ 75.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Divine Banquet"
  , id:       29
  , rarity:   4
  , icon:     IconNobleUp
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Grant Self 0 NPGen $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Angel's Song"
  , id:       30
  , rarity:   4
  , icon:     IconBeamUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Formal Craft"
  , id:       31
  , rarity:   5
  , icon:     IconArtsUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Imaginary Around"
  , id:       32
  , rarity:   5
  , icon:     IconQuickUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Limited/Zero Over"
  , id:       33
  , rarity:   5
  , icon:     IconBusterUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Kaleidoscope"
  , id:       34
  , rarity:   5
  , icon:     IconNoble
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ To Self GaugeUp $ 80.0 ~ 100.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Heaven's Feel"
  , id:       35
  , rarity:   5
  , icon:     IconBeamUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 40.0 ~ 50.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Beginning of the Journey"
  , id:       36
  , rarity:   4
  , icon:     IconRoad
  , stats:    { base: { atk: 50,  hp: 50 }
              , max:  { atk: 50, hp: 50 }
              }
  , effect:   [ Bonus FriendPoints $ Flat 75.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Parted Sea"
  , id:       37
  , rarity:   3
  , icon:     IconDodge
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Times 1 $ Grant Self 0 Evasion Full
              , Grant Self 0 DebuffResist $ 5.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Seal Designation Enforcer"
  , id:       38
  , rarity:   4
  , icon:     IconStarUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 StarUp $ 600.0 ~ 800.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Holy Shroud of Magdalene"
  , id:       39
  , rarity:   4
  , icon:     IconShieldUp
  , stats:    { base: { atk: 0, hp: 400 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (DefenseVs Male) $ 25.0 ~ 35.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Prisma Cosmos"
  , id:       40
  , rarity:   5
  , icon:     IconNobleTurn
  , stats:    { base: { atk: 250,  hp: 375 }
              , max:  { atk: 1000, hp: 1500 }
              }
  , effect:   [ Grant Self 0 GaugePerTurn $ 8.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Nightless Rose"
  , id:       41
  , rarity:   5
  , icon:     IconKneel
  , stats:    { base: { atk: 0, hp: 500 }
              , max:  { atk: 0, hp: 2000 }
              }
  , effect:   [ Times 1 <<< Grant Self 0 Guts $ 500.0 ~ 1000.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Mooncell Automaton"
  , id:       42
  , rarity:   3
  , icon:     IconAllUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Times 1 <<< Grant Self 0 Guts $ 500.0 ~ 1000.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Moony Jewel"
  , id:       43
  , rarity:   4
  , icon:     IconHoodUp
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 (Resist Charm) $ 80.0 ~ 100.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Moon Goddess' Bath"
  , id:       44
  , rarity:   5
  , icon:     IconHealTurn
  , stats:    { base: { atk: 0, hp: 500 }
              , max:  { atk: 0, hp: 2000 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 500.0 ~ 750.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Moonlight Fest"
  , id:       45
  , rarity:   5
  , icon:     IconStarHaloUp
  , stats:    { base: { atk: 250,  hp: 375 }
              , max:  { atk: 1000, hp: 1500 }
              }
  , effect:   [ Grant Self 0 StarUp $ 15.0 ~ 20.0
              , Grant Self 0 CritUp $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Runestone"
  , id:       46
  , rarity:   3
  , icon:     IconHoodUp
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 DebuffResist $ 5.0 ~ 10.0
              , Grant Self 0 StarUp $ 100.0 ~ 200.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "With One Strike"
  , id:       47
  , rarity:   4
  , icon:     IconBullseye
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 SureHit Full
              , Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "The Black Grail"
  , id:       48
  , rarity:   5
  , icon:     IconBeamUp
  , stats:    { base: { atk: 600,  hp: 0 }
              , max:  { atk: 2400, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 60.0 ~ 80.0
              , Debuff Self 0 HealthLoss $ Flat 500.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Jack-o'-Lantern"
  , id:       49
  , rarity:   3
  , icon:     IconDamageUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 DamageUp $ 100.0 ~ 200.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Trick or Treat"
  , id:       50
  , rarity:   3
  , icon:     IconStaffUp
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 DebuffSuccess $ 10.0 ~ 12.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Halloween Arrangement"
  , id:       51
  , rarity:   4
  , icon:     IconCrosshairUp
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 1 Taunt Full
              , Grant Self 1 DefenseUp $ 60.0 ~ 80.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Halloween Princess"
  , id:       52
  , rarity:   5
  , icon:     IconBeamUp
  , stats:    { base: { atk: 250,  hp: 375 }
              , max:  { atk: 1000, hp: 1500 }
              }
  , effect:   [ Grant Self 0 NPUp $ 15.0 ~ 20.0
              , To Self GaugeUp $ 30.0 ~ 50.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Little Halloween Devil"
  , id:       53
  , rarity:   5
  , icon:     IconNobleUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPGen $ 20.0 ~ 25.0
              , To Self GaugeUp $ 50.0 ~ 60.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Maid in Halloween"
  , id:       54
  , rarity:   5
  , icon:     IconHealUp
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Self 0 HealingReceived $ 60.0 ~ 75.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Anchors Aweigh"
  , id:       55
  , rarity:   3
  , icon:     IconHealTurn
  , stats:    { base: { atk: 300,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 100.0 ~ 200.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Code Cast"
  , id:       56
  , rarity:   4
  , icon:     IconSwordShieldUp
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Grant Self 3 AttackUp $ 25.0 ~ 30.0
              , Grant Self 3 DefenseUp $ 25.0 ~ 30.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Victor of the Moon"
  , id:       57
  , rarity:   5
  , icon:     IconBusterUp
  , stats:    { base: { atk: 600,  hp: 0 }
              , max:  { atk: 2400, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              , Grant Self 0 CritUp $ 20.0 ~ 25.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Another Ending"
  , id:       58
  , rarity:   5
  , icon:     IconArtsUp
  , stats:    { base: { atk: 600,  hp: 0 }
              , max:  { atk: 2400, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 10.0 ~ 15.0
              , Grant Self 0 CritUp $ 20.0 ~ 25.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Fate GUDAGUDA Order"
  , id:       59
  , rarity:   3
  , icon:     IconAllUp
  , stats:    { base: { atk: 100, hp: 150 }
              , max:  { atk: 500, hp: 750 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 1.0 ~ 2.0
              , Grant Self 0 (Performance Arts) $ 1.0 ~ 2.0
              , Grant Self 0 (Performance Buster) $ 1.0 ~ 2.0
              , Grant Self 0 StarUp $ 1.0 ~ 2.0
              , Grant Self 0 StarAbsorb $ 1.0 ~ 2.0
              , Grant Self 0 NPGen $ 1.0 ~ 2.0
              , Grant Self 0 NPUp $ 1.0 ~ 2.0
              , Grant Self 0 DebuffSuccess $ 1.0 ~ 2.0
              , Grant Self 0 DebuffResist $ 1.0 ~ 2.0
              , Grant Self 0 HealingReceived $ 1.0 ~ 2.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "After-Party Order!"
  , id:       60
  , rarity:   4
  , icon:     IconQuickBusterUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0
              , Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Guda-o"
  , id:       61
  , rarity:   5
  , icon:     IconDamageUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 15.0 ~ 20.0
              , Grant Self 0 NPUp $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "GUDAGUDA Poster Girl"
  , id:       62
  , rarity:   5
  , icon:     IconCrosshairUp
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Self 3 Taunt Full
              , Grant Self 3 AttackUp $ 60.0 ~ 80.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Demon Boar"
  , id:       65
  , rarity:   3
  , icon:     IconQuickUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Knight's Dignity"
  , id:       66
  , rarity:   4
  , icon:     IconExclamationUp
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Grant Self 0 CritUp $ 40.0 ~ 50.0
              , Debuff Self 0 DefenseDown $ Flat 20.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "A Fragment of 2030"
  , id:       67
  , rarity:   5
  , icon:     IconStarTurn
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Party 0 StarsPerTurn $ 8.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Lightning Reindeer"
  , id:       68
  , rarity:   3
  , icon:     IconBusterUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 3 (Performance Buster) $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "March of the Saint"
  , id:       69
  , rarity:   4
  , icon:     IconHealTurn
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 200.0 ~ 300.0
              , Grant Self 0 GaugePerTurn $ 3.0 ~ 4.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Present For My Master"
  , id:       70
  , rarity:   5
  , icon:     IconStarUp
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Self 0 StarUp $ 100.0 ~ 200.0
              , Grant Self 0 HealingReceived $ 40.0 ~ 50.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Holy Night Sign"
  , id:       71
  , rarity:   5
  , icon:     IconQuickUp
  , stats:    { base: { atk: 250,  hp: 375 }
              , max:  { atk: 1000, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0
              , Grant Self 0 CritUp $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Clock Tower"
  , id:       72
  , rarity:   3
  , icon:     IconNobleTurn
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 GaugePerTurn $ 2.0 ~ 3.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Necromancy"
  , id:       73
  , rarity:   4
  , icon:     IconKneel
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2400 }
              }
  , effect:   [ Chance 50 <<< Times 1 <<< Grant Self 0 Guts $ 500.0 ~ 1000.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Awakened Will"
  , id:       74
  , rarity:   4
  , icon:     IconNobleTurn
  , stats:    { base: { atk: 200, hp: 300 }
              , max:  { atk: 750, hp: 1125 }
              }
  , effect:   [ Chance 60 <<< Grant Self 0 GaugePerTurn $ 12.0 ~ 15.0
              , Debuff Self 0 HealthLoss $ Flat 500.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "500-Year Obsession"
  , id:       75
  , rarity:   5
  , icon:     IconCircuits
  , stats:    { base: { atk: 600,  hp: 0 }
              , max:  { atk: 2400, hp: 0 }
              }
  , effect:   [ When "defeated by an enemy" $ Debuff Target 2 SealNP Full
              , When "defeated by an enemy" <<<
                Debuff Target 10 Curse $ 1000.0 ~ 2000.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Peacefulness of 2018"
  , id:       76
  , rarity:   3
  , icon:     IconHealTurn
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 200.0 ~ 300.0
              , Debuff Self 0 AttackDown $ Flat 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic New Year"
  , id:       77
  , rarity:   4
  , icon:     IconHoodUp
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Times 1 $ Grant Self 0 DebuffResist Full
              , Grant Self 0 DefenseUp $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Law of the Jungle"
  , id:       78
  , rarity:   3
  , icon:     IconQuartz
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Bonus QuestQuartz $ 2017.0 ~ 2018.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Grand New Year"
  , id:       79
  , rarity:   5
  , icon:     IconShield
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Self 1 Taunt Full
              , Grant Self 1 Invincibility Full
              , Grant Self 0 DebuffResist $ 10.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Mona Lisa"
  , id:       80
  , rarity:   5
  , icon:     IconQuartz
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Bonus QPGain $ 2.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Happy x3 Order"
  , id:       81
  , rarity:   4
  , icon:     IconStarTurn
  , stats:    { base: { atk: 0, hp: 2018 }
              , max:  { atk: 0, hp: 2018 }
              }
  , effect:   [ Grant Party 0 StarsPerTurn $ 0.0 ~ 1.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Purely Bloom"
  , id:       82
  , rarity:   5
  , icon:     IconBeamUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ ToMax (40.0 ~ 50.0) <<< Grant Self 0 NPUp $ Flat 5.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Star of Altria"
  , id:       83
  , rarity:   5
  , icon:     IconKneel
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Times 1 <<< Grant Self 0 Guts $ Flat 1.0
              , Grant Self 0 DebuffResist $ 5.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Trueshot"
  , id:       84
  , rarity:   3
  , icon:     IconBullseye
  , stats:    { base: { atk: 200, hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 SureHit Full
              , Grant Self 0 CritUp $ 3.0 ~ 5.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Mikotto! Bride Training"
  , id:       85
  , rarity:   4
  , icon:     IconHealTurn
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Chance 65 <<< Grant Self 0 HealPerTurn $ 750.0 ~ 1000.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "The Crimson Land of Shadows"
  , id:       86
  , rarity:   5
  , icon:     IconDamageUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ ToMax (1000.0 ~ 1200.0) <<< Grant Self 0 DamageUp $ Flat 100.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Ryudoji Temple"
  , id:       89
  , rarity:   3
  , icon:     IconBeamUp
  , stats:    { base: { atk: 100, hp: 160 }
              , max:  { atk: 500, hp: 800 }
              }
  , effect:   [ Grant Self 0 NPUp $ 10.0 ~ 15.0
              , To Self GaugeUp $ 20.0 ~ 30.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Mana Gauge"
  , id:       90
  , rarity:   3
  , icon:     IconDamageUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0  }
              }
  , effect:   [ Grant Self 0 (ClassAffinity Caster) $ 8.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Elixir of Love"
  , id:       91
  , rarity:   3
  , icon:     IconHeart
  , stats:    { base: { atk: 100, hp: 160 }
              , max:  { atk: 500, hp: 800 }
              }
  , effect:   [ Grant Self 0 (Success Charm) $ 12.0 ~ 15.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Storch Ritter"
  , id:       92
  , rarity:   3
  , icon:     IconDamageUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ equipped Berserker <<< Grant Self 0 NPUp $ 15.0 ~ 25.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Hermitage"
  , id:       93
  , rarity:   3
  , icon:     IconArtsUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 3 (Performance Arts) $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Motored Cuirassier"
  , id:       94
  , rarity:   3
  , icon:     IconDamageUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (ClassAffinity Rider) $ 8.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Stuffed Lion"
  , id:       95
  , rarity:   3
  , icon:     IconHeal
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ When "defeated" <<< To Party Heal $ 800.0 ~ 1000.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Lugh's Halo"
  , id:       96
  , rarity:   3
  , icon:     IconHoodUp
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (Resist Stun) $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Vessel of the Saint"
  , id:       97
  , rarity:   5
  , icon:     IconHoodUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Times 3 $ Grant Self 0 DebuffResist Full
              , Grant Self 0 NPGen $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Golden Millennium Tree"
  , id:       98
  , rarity:   4
  , icon:     IconHPUp
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ ToMax (Flat 3000.0) <<< Grant Self 0 MaxHP $ 200.0 ~ 300.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Heroic Portrait: Mash Kyrielight"
  , id:       99
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic Portrait: Altria Pendragon"
  , id:       100
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic Portrait: Jeanne d'Arc"
  , id:       101
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic Portrait: Altera"
  , id:       102
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic Portrait: Arjuna"
  , id:       103
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic Portrait: Scathach"
  , id:       104
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic Portrait: Ushiwakamaru"
  , id:       105
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic Portrait: Henry Jekyll & Hyde"
  , id:       106
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic Portrait: Mephistopheles"
  , id:       107
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Heroic Portrait: Darius III"
  , id:       108
  , rarity:   4
  , icon:     IconRainbow
  , stats:    { base: { atk: 500, hp: 500 }
              , max:  { atk: 500, hp: 500 }
              }
  , effect:   [ Bonus Bond $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Valentine Dojo of Tears"
  , id:       109
  , rarity:   3
  , icon:     IconBullseye
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 SureHit Full
              , Grant Self 0 GaugePerTurn $ 3.0 ~ 5.0
              , Debuff Self 0 CharmVuln $ Flat 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Kitchen ☆ Patissiere"
  , id:       110
  , rarity:   4
  , icon:     IconStarHaloUp
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 StarUp $ 15.0 ~ 20.0
              , Grant Self 0 NPGen $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Street Choco-Maid"
  , id:       111
  , rarity:   5
  , icon:     IconArtsQuickUp
  , stats:    { base: { atk: 250, hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 10.0 ~ 15.0
              , Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0
              , Grant Self 0 HealingReceived $ 20.0 ~ 30.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Melty Sweetheart"
  , id:       112
  , rarity:   5
  , icon:     IconShieldUp
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Times 3 <<< Grant Self 0 (DefenseVs Male) $ Flat 100.0
              , Grant Self 0 StarUp $ 10.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Decapitating Bunny 2018"
  , id:       154
  , rarity:   5
  , icon:     IconShieldBreak
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 IgnoreInvinc Full
              , Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Mature Gentleman"
  , id:       155
  , rarity:   5
  , icon:     IconFire
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Grant Self 0 KillResist $ 60.0 ~ 80.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Grand Puppeteer"
  , id:       156
  , rarity:   5
  , icon:     IconNoble
  , stats:    { base: { atk: 400,  hp: 250 }
              , max:  { atk: 1600, hp: 1000 }
              }
  , effect:   [ To Self GaugeUp $ 50.0 ~ 60.0
              , Grant Self 3 (Performance Arts) $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Threefold Barrier"
  , id:       157
  , rarity:   5
  , icon:     IconShieldUp
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Times 3 <<< Grant Self 0 DamageDown $ 1000.0 ~ 1200.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Vivid Dance of Fists"
  , id:       158
  , rarity:   4
  , icon:     IconDamageUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 DamageUp $ 800.0 ~ 1000.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Mystic Eyes of Distortion"
  , id:       159
  , rarity:   4
  , icon:     IconBusterUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 20.0 ~ 25.0
              , Debuff Self 0 DefenseDown $ Flat 15.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Summer's Precognition"
  , id:       160
  , rarity:   4
  , icon:     IconDodge
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Times 1 $ Grant Self 0 Evasion Full
              , Grant Self 0 StarUp $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Chorus"
  , id:       161
  , rarity:   4
  , icon:     IconStarUp
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 StarAbsorb $ 300.0 ~ 400.0
              , Grant Self 3 DebuffResist $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Sprinter"
  , id:       162
  , rarity:   3
  , icon:     IconQuickUp
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 5.0 ~ 8.0
              , Grant Self 0 DebuffResist $ 10.0 ~ 15.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Repeat Magic"
  , id:       163
  , rarity:   3
  , icon:     IconNoble
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ To Self GaugeUp $ 20.0 ~ 30.0
              , Grant Self 0 NPGen $ 10.0 ~ 15.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Kiss Your Hand"
  , id:       165
  , rarity:   5
  , icon:     IconAllUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 10.0 ~ 12.0
              , Grant Self 0 (Performance Buster) $ 10.0 ~ 12.0
              , Grant Self 0 (Performance Quick) $ 10.0 ~ 12.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Teacher and I"
  , id:       166
  , rarity:   5
  , icon:     IconNoble
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ To Self GaugeUp $ 5.0 ~ 60.0
              , Grant Self 0 StarAbsorb $ 300.0 ~ 400.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Versus"
  , id:       167
  , rarity:   5
  , icon:     IconDamageUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 3 (AttackVs Divine) $ 80.0 ~ 100.0
              , Grant Self 3 (DefenseVs Divine) $ 40.0 ~ 50.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Beasts Under the Moon"
  , id:       168
  , rarity:   4
  , icon:     IconNobleUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPGen $ 12.0 ~ 15.0
              , Grant Self 0 StarUp $ 12.0 ~ 15.0
              , Grant Self 0 HealPerTurn $ 200.0 ~ 300.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Glass Full Sweet Time"
  , id:       169
  , rarity:   4
  , icon:     IconBullseye
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 SureHit Full
              , Grant Self 0 DamageUp $ 400.0 ~ 600.0
              , Grant Self 0 DamageDown $ 200.0 ~ 300.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Salon de Marie"
  , id:       170
  , rarity:   3
  , icon:     IconDodge
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Times 1 $ Grant Self 0 Evasion Full
              , Grant Self 0 HealingReceived $ 5.0 ~ 10.0
              , Grant Self 0 DebuffSuccess $ 3.0 ~ 5.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Prince of Slayer"
  , id:       171
  , rarity:   3
  , icon:     IconStarTurn
  , stats:    { base: { atk: 100, hp: 160 }
              , max:  { atk: 500, hp: 800 }
              }
  , effect:   [ Grant Party 0 StarsPerTurn $ 1.0 ~ 2.0
              , Grant Self 0 (AttackVs Dragon) $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Noisy Obsession"
  , id:       172
  , rarity:   4
  , icon:     IconExclamationUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 15.0 ~ 20.0
              , Grant Self 0 NPUp $ 15.0 ~ 20.0
              , Grant Self 0 (Success Charm) $ 12.0 ~ 15.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Ideal Holy King"
  , id:       175
  , rarity:   5
  , icon:     IconHPUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Party 0 MaxHP $ 1000.0 ~ 1200.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Record Holder"
  , id:       176
  , rarity:   4
  , icon:     IconStaffUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 DebuffSuccess $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Beast of Billows"
  , id:       177
  , rarity:   3
  , icon:     IconBeamUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ equipped Lancer <<< Grant Self 0 NPUp $ 15.0 ~ 25.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Personal Training"
  , id:       178
  , rarity:   5
  , icon:     IconRoad
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Bonus EXPPerc $ 2.0 ~ 10.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "The Scholars of Chaldea"
  , id:       179
  , rarity:   5
  , icon:     IconNoble
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ To Self GaugeUp $ 30.0 ~ 50.0
              , Grant Self 0 HealingReceived $ 20.0 ~ 30.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Maiden Leading Chaldea"
  , id:       180
  , rarity:   5
  , icon:     IconStarTurn
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0
              , Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "The Merciless One"
  , id:       181
  , rarity:   5
  , icon:     IconNoble
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ When "defeated" <<< To Party GaugeUp $ 15.0 ~ 20.0
              , Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Art of the Poisonous Snake"
  , id:       182
  , rarity:   4
  , icon:     IconArtsUp
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 3 (Performance Arts) $ 30.0 ~ 40.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Art of Death"
  , id:       183
  , rarity:   4
  , icon:     IconDamageUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (AttackVs Humanoid) $ 25.0 ~ 30.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Gentle Affection"
  , id:       184
  , rarity:   4
  , icon:     IconHealUp
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 HealUp $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Volumen Hydrargyrum"
  , id:       185
  , rarity:   5
  , icon:     IconShield
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Times 3 $ Grant Self 0 Invincibility Full
              , Grant Self 0 DamageUp $ 200.0 ~ 500.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Innocent Maiden"
  , id:       186
  , rarity:   4
  , icon:     IconNobleTurn
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 GaugePerTurn $ 4.0 ~ 5.0
              , Grant Self 0 (Performance Quick) $ 10.0 ~ 12.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Self Geas Scroll"
  , id:       187
  , rarity:   3
  , icon:     IconStaffUp
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 (Success Stun) $ 12.0 ~ 15.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Before Awakening"
  , id:       188
  , rarity:   5
  , icon:     IconAllUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0
              , Grant Self 0 (Performance Buster) $ 8.0 ~ 10.0
              , Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0
              , Grant Self 0 DefenseUp $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "His Rightful Place"
  , id:       189
  , rarity:   5
  , icon:     IconStarTurn
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0
              , To Self GaugeUp $ 30.0 ~ 50.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, bond 191 "Crown of the Star" "Altria Pendragon" IconDamageUp
  [ party AttackUp 15 ]
, bond 192 "Relic of the King" "Zhuge Liang (Lord El-Melloi II)" IconBusterUp
  [ party' Buster 15 ]
, bond 193 "Triumph of the Lord Impaler" "Vlad III" IconBeamUp
  [ np30, atkChance 30 <<< To Self GaugeUp $ Flat 5.0 ]
, bond 194 "Revelation from Heaven" "Jeanne d'Arc" IconBusterUp
  [ party' Buster 15 ]
, bond 195 "Memories of the Dragon" "Altria Pendragon (Alter)" IconBeamUp
  [ np30, atkChance 30 <<< Debuff Target 3 DefenseDown $ Flat 5.0 ]
, bond 196 "Hunter of the Red Plains" "EMIYA" IconBeamUp
  [ np30, atkChance 30 <<< To Party GainStars $ Flat 5.0 ]
, bond 197 "Castle of Snow" "Heracles" IconKneel
  [ Times 3 <<< Grant Self 0 Guts $ Flat 500.0 ]
, bond 198 "Yggdrasil Tree" "Cu Chulainn (Caster)" IconBeamUp
  [ np30, atkChance 30 <<< To Self Heal $ Flat 500.0 ]
, bond 199 "Scorching Embrace" "Kiyohime" IconBeamUp
  [ np30, atkChance 30 <<< Debuff Target 5 Burn $ Flat 500.0 ]
, bond 200 "Worthless Jewel" "Mata Hari" IconNobleUp
  [ party NPGen 15 ]
, bond 201 "Eternal Solitude" "Altera" IconSwordUp
  [ party AttackUp 15 ]
, bond 202 "Queen's Present" "Chevalier d'Eon" IconArtsUp
  [ party' Arts 15 ]
, bond 203 "Elixir" "Elisabeth Bathory" IconHealTurn
  [ party HealPerTurn 500 ]
, bond 204 "My Necklace" "Marie Antoinette" IconStarHaloUp
  [ party StarUp 20 ]
, bond 205 "Staff He Gave Me" "Martha" IconHealUp
  [ party HealingReceived 30 ]
, bond 206 "Iron Maiden" "Carmilla" IconBeamUp
  [ np30, atkChance 10 $ Debuff Target 1 SealNP Full ]
, bond 207 "Cat Apron" "Tamamo Cat" IconHeal
  [ party MaxHP 2000 ]
, bond 208 "Thirst for Victory" "Boudica" IconStarHaloUp
  [ party StarUp 20 ]
, bond 209 "To My Dear Friend" "Hans Christian Andersen" IconHoodUp
  [ party DebuffResist 30 ]
, bond 210 "Sacred Devotion" "Arash" IconHeal
  [ When "defeated" $ To Party RemoveDebuffs Full
  , When "defeated" $ To Party Heal $ Flat 5000.0
  ]
, { name:     "The Wandering Tales of Shana-oh"
  , id:       211
  , rarity:   5
  , icon:     IconQuickUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0
              , When "defeated" <<<
                Grant Party 1 (Performance Quick) $ 20.0 ~ 30.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Golden Captures the Carp"
  , id:       212
  , rarity:   5
  , icon:     IconNoble
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ To Self GaugeUp $ 30.0 ~ 50.0
              , To Party GainStars $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "A Fox Night's Dream"
  , id:       213
  , rarity:   5
  , icon:     IconNobleUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 NPGen $ 20.0 ~ 25.0
              , Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Burning Tale of Love"
  , id:       214
  , rarity:   4
  , icon:     IconDamageUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (AttackVs Male) $ 25.0 ~ 30.0
              , Grant Self 0 DebuffSuccess $ 12.0 ~ 15.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Reciting the Subscription List"
  , id:       215
  , rarity:   3
  , icon:     IconHoodUp
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Times 1 $ Grant Self 0 DebuffResist Full ]
  , bond:     Nothing
  , limited:  true
  }
, bond 216 "Key of the King's Law" "Gilgamesh" IconBeamUp
  [ np30, atkChance 30 <<< Grant Self 3 CritUp $ Flat 10.0 ]
, bond 217 "Golden Glass" "Sakata Kintoki" IconBeamUp
  [ np30, atkChance 30 <<< To Self GaugeUp $ Flat 5.0 ]
, bond 218 "Thunderous Applause" "Nero Claudius" IconArtsUp
  [ party' Arts 15 ]
, bond 219 "Das Rheingold" "Siegfried" IconNobleUp
  [ party NPGen 15 ]
, bond 220 "Radiance of the Goddess" "Stheno" IconQuickUp
  [ party' Quick 15 ]
, bond 221 "Voyage of the Flowers" "Altria Pendragon (Lily)" IconSwordUp
  [ party AttackUp 10, party StarUp 10 ]
, bond 222 "Ark of the Covenant" "David" IconBeamUp
  [ np30, atkChance 10 $ To Target Kill Full ]
, bond 223 "Door to Babylon" "Darius III" IconBusterUp
  [ party' Buster 15 ]
, bond 224 "Blood-Thirsting Axe" "Eric Bloodaxe" IconExclamationUp
  [ party CritUp 25 ]
, bond 225 "Insurrection" "Spartacus" IconKneel
  [ gutsPercent 50 ]
, { name:     "Go West!!"
  , id:       226
  , rarity:   5
  , icon:     IconBeamUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 NPUp $ 20.0 ~ 25.0
              , Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "The Classic Three Great Heroes"
  , id:       227
  , rarity:   5
  , icon:     IconBeamUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 NPUp $ 15.0 ~ 20.0
              , Grant Self 0 StarUp $ 15.0 ~ 20.0
              , To Self GaugeUp $ 25.0 ~ 40.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "True Samadhi Fire"
  , id:       228
  , rarity:   4
  , icon:     IconBeamUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 15.0 ~ 20.0
              , Grant Self 0 (Performance Buster) $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "All Three Together"
  , id:       229
  , rarity:   3
  , icon:     IconStarUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 StarAbsorb $ 100.0 ~ 200.0
              , Grant Self 0 CritUp $ 5.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, bond 230 "Tristar Belt" "Orion" IconExclamationUp
  [ party CritUp 25 ]
, bond 231 "Golden Helm" "Francis Drake" IconBeamUp
  [ party NPUp 20 ]
, bond 232 "Black Knight's Helmet" "Lancelot" IconBeamUp
  [ np30, atkChance 30 <<< Debuff Target 3 CritChance $ Flat 30.0 ]
, bond 233 "Golden Apple" "Atalante" IconQuickUp
  [ party' Quick 15 ]
, bond 234 "Holy Pumpkin Grail" "Elisabeth Bathory (Halloween)" IconHoodUp
  [ party DebuffResist 30 ]
, bond 235 "Rotary Matchlock" "Oda Nobunaga" IconExclamationUp
  [ party CritUp 25 ]
, bond 236 "Llamrei Unit II" "Altria Pendragon (Santa Alter)" IconStarHaloUp
  [ party StarUp 20 ]
, bond 237 "Things to Calm the Heart" "Henry Jekyll & Hyde" IconBusterUp
  [ party' Buster 15 ]
, bond 238 "Glory of the Past Days" "Edward Teach" IconBusterUp
  [ party' Buster 15 ]
, bond 239 "Heaven Among the Mountains" "Sasaki Kojirou" IconQuickUp
  [ party' Quick 15 ]
, { name:     "Divine Princess of the Storm"
  , id:       240
  , rarity:   5
  , icon:     IconShieldUp
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ When "defeated" <<< Grant Party 3 DefenseUp $ 20.0 ~ 25.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Ox-Demon King"
  , id:       241
  , rarity:   5
  , icon:     IconBusterUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Party 3 (Performance Buster) $ 10.0 ~ 15.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Personal Lesson"
  , id:       242
  , rarity:   5
  , icon:     IconRoad
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Bonus MysticCode $ Flat 2.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Bronze-Link Manipulator"
  , id:       243
  , rarity:   3
  , icon:     IconSwordUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 3 AttackUp $ 15.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Ath nGabla"
  , id:       244
  , rarity:   3
  , icon:     IconQuickUp
  , stats:    { base: { atk: 100, hp: 160 }
              , max:  { atk: 500, hp: 800 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0
              , Debuff Self 0 DefenseDown $ Flat 10.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Bygone Dream"
  , id:       245
  , rarity:   3
  , icon:     IconBeamUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ equipped Assassin <<< Grant Self 0 NPUp $ 15.0 ~ 25.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Extremely Spicy Mapo Tofu"
  , id:       246
  , rarity:   3
  , icon:     IconHealUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 HealingReceived $ 10.0 ~ 20.0 ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Jeweled Sword Zelretch"
  , id:       247
  , rarity:   3
  , icon:     IconNobleUp
  , stats:    { base: { atk: 100, hp: 160 }
              , max:  { atk: 500, hp: 800 }
              }
  , effect:   [ Grant Self 0 NPGen $ 5.0 ~ 10.0
              , To Self GaugeUp $ 25.0 ~ 40.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, bond 248 "Tamamo's Club" "Tamamo-no-Mae" IconArtsUp
  [ party' Arts 15 ]
, bond 249 "Headband of Resolve" "Okita Souji" IconExclamationUp
  [ party CritUp 25 ]
, bond 250 "Calico Jack" "Anne Bonny & Mary Read" IconExclamationUp
  [ party CritUp 25 ]
, bond 251 "Gazing Upon Dun Scaith" "Scathach" IconQuickUp
  [ party' Quick 15 ]
, bond 252 "Star of Prophecy" "Cu Chulainn" IconBeamUp
  [ np30, atkChance 30 <<< Grant Self 3 CritUp $ Flat 10.0 ]
, bond 253 "Hekate's Staff" "Medea" IconArtsUp
  [ party' Arts 15 ]
, bond 254 "Formless Island" "Medusa" IconNobleUp
  [ party NPGen 15 ]
, bond 255 "Cask of the Wise" "Alexander" IconQuickUp
  [ party' Quick 15 ]
, bond 256 "Shaytan's Arm" "Hassan of the Cursed Arm" IconReaperUp
  [ party KillUp 20 ]
, bond 257 "Ariadne's Thread" "Asterios" IconQuickUp
  [ party' Quick 15 ]
, { name:     "Dumplings Over Flowers"
  , id:       258
  , rarity:   5
  , icon:     IconQuickUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 15.0 ~ 20.0
              , Grant Self 0 NPUp $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Faithful Companions"
  , id:       259
  , rarity:   4
  , icon:     IconArtsUp
  , stats:    { base: { atk: 0, hp: 600 }
              , max:  { atk: 0, hp: 2250 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0
              , Grant Self 0 NPGen $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Hidden Sword: Pheasant Reversal"
  , id:       260
  , rarity:   3
  , icon:     IconQuickUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 3.0 ~ 5.0
              , Grant Self 0 CritUp $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Golden Sumo: Boulder Tournament"
  , id:       261
  , rarity:   5
  , icon:     IconSwordUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 AttackUp $ 10.0 ~ 15.0
              , To Self GaugeUp $ 30.0 ~ 50.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Hot Spring Under the Moon"
  , id:       262
  , rarity:   5
  , icon:     IconExclamationUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 20.0 ~ 25.0
              , Grant Self 0 StarsPerTurn $ 3.0 ~ 4.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Origin Bullet"
  , id:       263
  , rarity:   5
  , icon:     IconShieldBreak
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 IgnoreInvinc Full
              , Grant Self 0 (ClassAffinity Caster) $ 35.0 ~ 40.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Covering Fire"
  , id:       264
  , rarity:   4
  , icon:     IconDamageUp
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 DamageUp $ 400.0 ~ 600.0
              , Grant Self 0 CritUp $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  false
  }
, { name:     "Battle of Camlann"
  , id:       265
  , rarity:   3
  , icon:     IconNoble
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ When "defeated" <<< To Party GaugeUp $ 10.0 ~ 15.0 ]
  , bond:     Nothing
  , limited:  false
  }
, bond 266 "Who Am I?" "Mordred" IconBeamUp
  [ party NPUp 20 ]
, bond 267 "The Misty Night of London" "Jack the Ripper" IconExclamationUp
  [ party CritUp 25 ]
, bond 268 "Wonderland" "Nursery Rhyme" IconExclamationUp
  [ party CritUp 15, party HealingReceived 10 ]
, bond 269 "Faceless King" "Robin Hood" IconArtsUp
  [ party' Arts 15 ]
, bond 270 "Usumidori" "Ushiwakamaru" IconQuickUp
  [ party' Quick 15 ]
, bond 271 "Etiquette of Nine Guests" "Jing Ke" IconBeamUp
  [ np30, atkChance 30 <<< Grant Self 3 KillUp $ Flat 30.0 ]
, bond 272 "Heaven Scorcher Halberd" "Lu Bu Fengxian" IconBusterUp
  [ party' Buster 15 ]
, bond 273 "What can be Left Behind" "Georgios" IconShield
  [ When "defeated" <<< Times 1 $ Grant Party 0 Invincibility Full
  , When "defeated" <<< Grant Party 3 DamageDown $ Flat 1000.0
  ]
, bond 274 "Thermopylae" "Leonidas I" IconBusterUp
  [ party' Buster 15 ]
, bond 275 "Haydn Quartets" "Wolfgang Amadeus Mozart" IconBeamUp
  [ party NPUp 20 ]
, { name:     "Anniversary Heroines"
  , id:       276
  , rarity:   4
  , icon:     IconSwordUp
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Grant Self 0 AttackUp $ Flat 10.0
              , Grant Self 0 StarsPerTurn $ Flat 3.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Leisure Stroll"
  , id:       277
  , rarity:   5
  , icon:     IconStarUp
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Grant Self 0 StarUp $ 400.0 ~ 600.0
              , Grant Self 0 (Performance Arts) $ 10.0 ~ 15.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Partake with the King"
  , id:       278
  , rarity:   5
  , icon:     IconBusterUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              , To Self GaugeUp $ 50.0 ~ 60.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Goldfish Scooping"
  , id:       279
  , rarity:   4
  , icon:     IconBullseye
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 SureHit Full
              , Grant Self 0 (Performance Buster) $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Fire Flower"
  , id:       280
  , rarity:   3
  , icon:     IconStarHaloUp
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 StarUp $ 5.0 ~ 10.0
              , Grant Self 0 CritUp $ 5.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, bond 281 "Arm of Raiden" "Nikola Tesla" IconBeamUp
  [ party NPUp 20 ]
, bond 282 "Endowed Hero" "Arjuna" IconBeamUp
  [ np30, Grant Self 0 StarAbsorb $ Flat 1000.0 ]
, bond 283 "Lamp of the Unfortunate" "Karna" IconAllUp
  [ party' Quick 8, party' Arts 8, party' Buster 8 ]
, bond 284 "Procedure to Humanity" "Frankenstein" IconQuickUp
  [ party' Quick 15 ]
, bond 285 "Black Helmet" "Altria Pendragon (Lancer Alter)" IconNobleUp
  [ party NPGen 15 ]
, bond 286 "Legend of the Gallic War" "Gaius Julius Caesar" IconQuickUp
  [ party' Quick 15 ]
, bond 287 "Rome" "Romulus" IconBeamUp
  [ party NPUp 20 ]
, bond 288 "Encounter at Gojou Bridge" "Musashibou Benkei" IconNobleRedUp
  [ party NPFromDamage 20 ]
, bond 289 "Impure Death Mask" "Phantom of the Opera" IconQuickUp
  [ party' Quick 15 ]
, bond 290 "Really Convenient" "William Shakespeare" IconNobleUp
  [ party NPGen 15 ]
, { name:     "Pirates Party!"
  , id:       291
  , rarity:   5
  , icon:     IconShieldBreak
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 IgnoreInvinc Full
              , Grant Party 0 StarsPerTurn $ 3.0 ~ 4.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Summertime Mistress"
  , id:       292
  , rarity:   5
  , icon:     IconExclamationUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 CritUp $ 15.0 ~ 20.0
              , To Self GaugeUp $ 30.0 ~ 50.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Twilight Memory"
  , id:       293
  , rarity:   4
  , icon:     IconQuickUp
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Times 1 $ Grant Self 0 Evasion Full
              , Grant Self 0 (Performance Quick) $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Shiny Goddess"
  , id:       294
  , rarity:   3
  , icon:     IconArtsUp
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DefenseUp $ 3.0 ~ 5.0
              , Grant Self 0 (Performance Arts) $ 3.0 ~ 5.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Knights of Marines"
  , id:       295
  , rarity:   5
  , icon:     IconQuickUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Quick) $ 10.0 ~ 15.0
              , To Self GaugeUp $ 50.0 ~ 60.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Chaldea Lifesavers"
  , id:       296
  , rarity:   5
  , icon:     IconKneel
  , stats:    { base: { atk: 0, hp: 750 }
              , max:  { atk: 0, hp: 3000 }
              }
  , effect:   [ Times 1 <<< Grant Self 0 Guts $ Flat 1.0
              , Grant Self 0 NPGen $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Meat Wars"
  , id:       297
  , rarity:   4
  , icon:     IconHealTurn
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 HealPerTurn $ 200.0 ~ 300.0
              , Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Shaved Ice (Void's Dust Flavor)"
  , id:       298
  , rarity:   3
  , icon:     IconShieldUp
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DamageDown $ 100.0 ~ 200.0
              , Grant Self 0 DebuffResist $ 5.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, bond 299 "Annihilation List" "Mysterious Heroine X" IconDamageUp
  [ party (ClassAffinity Saber) 20 ]
, bond 300 "Imperishable Flames" "Brynhild" IconBusterUp
  [ party' Buster 10, party NPGen 10 ]
, bond 301 "Ring of Bay Laurel" "Nero Claudius (Bride)" IconArtsUp
  [ party' Arts 15 ]
, bond 302 "Final Battle" "Beowulf" IconDamageUp
  [ party (AttackVs Dragon) 20 ]
, bond 303 "Bratan of Wisdom" "Fionn mac Cumhaill" IconArtsUp
  [ party' Arts 10, party NPUp 10 ]
, bond 304 "Prelati's Spellbook" "Gilles de Rais" IconBusterUp
  [ party' Buster 20, Debuff Party 0 DebuffVuln $ Flat 20.0 ]
, bond 305 "Parasitic Bomb" "Mephistopheles" IconBeamUp
  [ party NPUp 20 ]
, bond 306 "Seethe of a Warrior" "Fergus mac Roich" IconBusterUp
  [ party' Buster 10, party NPUp 10 ]
, bond 307 "My Loathsome Life" "Charles-Henri Sanson" IconReaperUp
  [ party KillUp 10, party NPGen 10 ]
, bond 308 "There is No Love Here" "Caligula" IconBusterUp
  [ party' Buster 20, Debuff Party 0 DefenseDown $ Flat 10.0 ]
, { name:     "Magical Girl of Sapphire"
  , id:       309
  , rarity:   5
  , icon:     IconNobleUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPGen $ 25.0 ~ 30.0
              , To Self GaugeUp $ 40.0 ~ 50.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Kill on Sight"
  , id:       310
  , rarity:   4
  , icon:     IconArtsUp
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 8.0 ~ 10.0
              , Grant Self 0 NPUp $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Zunga Zunga!"
  , id:       311
  , rarity:   3
  , icon:     IconShieldUp
  , stats:    { base: { atk: 0, hp: 300 }
              , max:  { atk: 0, hp: 1500 }
              }
  , effect:   [ Grant Self 0 DamageDown $ 100.0 ~ 200.0
              , Grant Self 0 HealingReceived $ 5.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Kaleid Ruby"
  , id:       312
  , rarity:   4
  , icon:     IconBusterUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              , Grant Self 0 NPUp $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Kaleid Sapphire"
  , id:       313
  , rarity:   4
  , icon:     IconArtsUp
  , stats:    { base: { atk: 400,  hp: 0 }
              , max:  { atk: 1500, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Arts) $ 10.0 ~ 15.0
              , Grant Self 0 NPUp $ 8.0 ~ 10.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, bond 315 "Mugashiki—Shinkuu Myou" "Ryougi Shiki (Saber)" IconArtsUp
  [ party' Arts 15 ]
, bond 317 "Château d'If" "Edmond Dantes" IconQuickUp
  [ party' Quick 15 ]
, bond 318 "Unlimited Pancakes" "Medea (Lily)" IconHealUp
  [ party HealingReceived 30 ]
, bond 319 "Red Leather Jacket" "Ryougi Shiki (Assassin)" IconReaperUp
  [ party KillUp 30 ]
, bond 321 "Letter From a Friend" "Gilles de Rais (Caster)" IconBusterUp
  [ party' Buster 20, Debuff Party 0 StarDown $ Flat 20.0 ]
, bond 322 "Hound of Culann" "Cu Chulainn (Prototype)" IconDamageUp
  [ party (AttackVs Beast) 20 ]
, bond 323 "Radiance of the Goddess (Euryale)" "Euryale" IconArtsUp
  [ party' Arts 15 ]
, bond 324 "Hero's Armament" "Hektor" IconBeamUp
  [ party NPUp 20 ]
, { name:     "Glory Is With Me"
  , id:       325
  , rarity:   5
  , icon:     IconBeamUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 NPUp $ 15.0 ~ 20.0
              , Grant Self 0 CritUp $ 15.0 ~ 20.0
              , Grant Self 0 StarsPerTurn $ 3.0 ~ 4.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Original Legion"
  , id:       326
  , rarity:   4
  , icon:     IconShieldUp
  , stats:    { base: { atk: 200, hp: 320 }
              , max:  { atk: 750, hp: 1200 }
              }
  , effect:   [ Grant Self 0 DefenseUp $ 8.0 ~ 20.0
              , Grant Self 0 NPUp $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Howl at the Moon"
  , id:       327
  , rarity:   3
  , icon:     IconBusterUp
  , stats:    { base: { atk: 200,  hp: 0 }
              , max:  { atk: 1000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 10.0 ~ 15.0
              , Debuff Self 0 DebuffVuln $ Flat 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Princess of the White Rose"
  , id:       328
  , rarity:   5
  , icon:     IconKneel
  , stats:    { base: { atk: 250,  hp: 400 }
              , max:  { atk: 1000, hp: 1600 }
              }
  , effect:   [ Times 1 <<< Grant Self 0 Guts $ Flat 1.0
              , To Self GaugeUp $ 10.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Joint Recital"
  , id:       329
  , rarity:   5
  , icon:     IconBusterUp
  , stats:    { base: { atk: 500,  hp: 0 }
              , max:  { atk: 2000, hp: 0 }
              }
  , effect:   [ Grant Self 0 (Performance Buster) $ 15.0 ~ 20.0
              , Grant Self 0 CritUp $ 15.0 ~ 20.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, bond 334 "Indomitableness" "Florence Nightingale" IconBusterUp
  [ party' Buster 10, party HealingReceived 20 ]
, bond 335 "One-Man War" "Cu Chulainn (Alter)" IconKneel
  [ np30, gutsPercent 20 ]
, bond 336 "Sacred Spring" "Queen Medb" IconNobleUp
  [ party NPGen 15 ]
, bond 337 "Indestructible Blade" "Rama" IconExclamationUp
  [ party CritUp 25 ]
, bond 338 "Concealed Goddess" "Helena Blavatsky" IconDamageUp
  [ party (ClassAffinity Assassin) 20 ]
, bond 339 "Lights of Civilization" "Thomas Edison" IconNobleUp
  [ party NPGen 15 ]
, bond 340 "Reaching the Zenith of My Skill" "Li Shuwen" IconArtsUp
  [ party' Arts 15 ]
, bond 341 "Knight's Oath" "Diarmuid Ua Duibhne" IconArtsUp
  [ party' Quick 10, party' Arts 10 ]
, bond 342 "Elemental" "Paracelsus von Hohenheim" IconArtsUp
  [ party' Arts 10, party NPUp 10 ]
, bond 343 "NEO Difference Engine" "Charles Babbage" IconBusterUp
  [ party' Buster 20, Debuff Party 0 DefenseDown $ Flat 10.0 ]
, bond 350 "Hell of Blazing Punishment" "Jeanne d'Arc (Alter)" IconBusterUp
  [ party' Buster 15 ]
, bond 351 "Gordian Knot" "Iskandar" IconSwordUp
  [ party AttackUp 15 ]
, bond 352 "Bai Long" "Xuanzang Sanzang" IconBusterUp
  [ party' Buster 20, Debuff Party 0 DefenseDown $ Flat 10.0 ]
, bond 353 "The Sun Shines Here" "Emiya (Assassin)" IconArtsQuickUp
  [ party' Quick 10, party' Arts 10 ]
, bond 354 "Dress of Heaven" "Irisviel (Dress of Heaven)" IconHealUp
  [ party HealingReceived 30 ]
, bond 355 "Manifestation of The Golden Rule" "Gilgamesh (Child)" IconNobleUp
  [ party NPGen 15 ]
, bond 356 "Spirit of The Vast Land" "Geronimo" IconNobleUp
  [ party NPGen 15 ]
, bond 357 "Extolling The Revolver" "Billy the Kid" IconExclamationUp
  [ party CritUp 25 ]
, bond 358 "Library of Hundred Men" "Hassan of the Hundred Personas" IconAllUp
  [ party' Buster 8, party' Quick 8, party' Arts 8 ]
, bond 359 "Last Splinter" "Angra Mainyu" IconDamageUp
  [ Grant Self 0 (AttackVs Beast) $ Flat 200.0, gutsPercent 20 ]
, { name:     "Fate/EXTELLA"
  , id:       360
  , rarity:   4
  , icon:     IconExclamationUp
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Grant Self 0 CritUp $ Flat 15.0
              , Grant Party 0 StarsPerTurn $ Flat 3.0
              ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Spiritron Portrait: Nero Claudius"
  , id:       361
  , rarity:   4
  , icon:     IconRoad
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Spiritron Portrait: Nameless"
  , id:       362
  , rarity:   4
  , icon:     IconRoad
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Spiritron Portrait: Tamamo-no-Mae"
  , id:       363
  , rarity:   4
  , icon:     IconRoad
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Spiritron Portrait: Karna"
  , id:       364
  , rarity:   4
  , icon:     IconRoad
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Spiritron Portrait: Altera"
  , id:       365
  , rarity:   4
  , icon:     IconRoad
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, { name:     "Spiritron Portrait: Gilgamesh"
  , id:       366
  , rarity:   4
  , icon:     IconRoad
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
, bond 367 "Divine Oni-Poison Sake" "Shuten-Douji" IconArtsQuickUp
  [ party' Quick 10, party' Arts 10 ]
, bond 368 "Doujigiri Yasutsuna" "Minamoto-no-Raikou" IconBusterUp
  [ party' Buster 10, party CritUp 15 ]
, bond 369 "Ramesseum" "Ozymandias" IconBusterArtsUp
  [ party' Arts 10, party' Buster 10 ]
, bond 370 "Bone Sword (Nameless)" "Ibaraki-Douji" IconBusterUp
  [ party' Buster 20, Debuff Party 0 DefenseDown $ Flat 10.0 ]
, bond 371 "Unit Golden Bear" "Sakata Kintoki (Rider)" IconStarHaloUp
  [ party StarUp 20 ]
, bond 372 "Gringolet" "Gawain" IconBusterUp
  [ party' Buster 15 ]
, bond 373 "But I Lied Once" "Tristan" IconExclamationUp
  [ party CritUp 25 ]
, bond 374 "Exercising the Royal Authority" "Nitocris" IconNobleUp
  [ party NPGen 10, party NPUp 10 ]
, bond 375 "Mask of A Demon" "Fuuma \"Evil-wind\" Kotarou" IconQuickUp
  [ party' Quick 15 ]
, bond 376 "Cook Despite of Exhaustion" "Tawara Touta" IconHealTurn
  [ party HealPerTurn 500 ]
, bond 377 "Dun Stallion" "Altria Pendragon (Lancer)" IconSwordUp
  [ party AttackUp 10, party NPUp 10 ]
, bond 378 "All-Encompassing Wisdom" "Leonardo da Vinci" IconBeamUp
  [ party NPUp 20 ]
, bond 379 "Sunset Beach" "Tamamo-no-Mae (Lancer)" IconQuickBusterUp
  [ party' Quick 10, party' Buster 10 ]
, bond 380 "Lady of the Lake" "Lancelot (Saber)" IconNobleUp
  [ party NPGen 10, party CritUp 10 ]
, bond 381 "Reminiscence of the Summer"
           "Marie Antoinette (Caster)" IconExclamationUp
  [ party CritUp 25 ]
, bond 382 "Currently In The Middle Of A Shower"
           "Anne Bonny & Mary Read (Archer)" IconBusterArtsUp
  [ party' Buster 10, party' Arts 10 ]
, bond 383 "Prydwen" "Mordred (Rider)" IconBeamUp
  [ party NPUp 20 ]
, bond 384 "Beach Love Letter (Terror)" "Kiyohime (Lancer)" IconBusterUp
  [ party' Buster 20, Debuff Party 0 DefenseDown $ Flat 10.0 ]
, bond 385 "My Long Lost Right Arm" "Bedivere" IconBusterUp
  [ party' Buster 10, party NPGen 10 ]
, bond 386 "Proof of Existence" "Hassan of the Serenity" IconQuickUp
  [ party' Quick 15 ]
, bond 391 "Champion Cup" "Altria Pendragon (Archer)" IconSwordUp
  [ party AttackUp 15 ]
, bond 392 "Phantasmal Summoning (Install)" "Illyasviel von Einzbern" IconAllUp
  [ party' Buster 8, party' Quick 8, party' Arts 8 ]
, bond 394 "Holy Knuckle" "Martha (Ruler)" IconBusterUp
  [ party' Buster 15 ]
, bond 395 "Minimal Prudence" "Scathach (Assassin)" IconQuickUp
  [ party' Quick 15 ]
, bond 396 "Sharing of Pain" "Chloe von Einzbern" IconExclamationUp
  [ party CritUp 30, Debuff Party 0 HealthLoss $ Flat 200.0 ]
, { name:     "First Order"
  , id:       399
  , rarity:   4
  , icon:     IconRoad
  , stats:    { base: { atk: 100, hp: 100 }
              , max:  { atk: 100, hp: 100 }
              }
  , effect:   [ Bonus EXP $ Flat 50.0 ]
  , bond:     Nothing
  , limited:  true
  }
]
  where
    np30        = Grant Self 0 NPUp $ Flat 30.0
    gutsPercent = Times 1 <<< Grant Self 0 GutsPercent <<< Flat <<< Int.toNumber
    party buff  = Grant Party 0 buff <<< Flat <<< Int.toNumber
    party' card = party (Performance card)
    atkChance chance = When "attacking" <<< Chance chance
    bond id name servant icon effect =
        { name
        , id
        , rarity:   4
        , icon
        , stats:    { base: { atk: 100, hp: 100 }
                    , max:  { atk: 100, hp: 100 }
                    }
        , effect:   When ("equipped by " <> servant) <$> effect
        , bond:     Just servant
        , limited:  false
        }


getBond :: Servant -> Maybe CraftEssence
getBond (Servant s) = go s.name
  where
    go = memoize \name ->
         let match (CraftEssence ce) = ce.bond == Just name
         in find match craftEssences

newtype CraftEssence = CraftEssence { name     :: String
                                    , id       :: Int
                                    , rarity   :: Int
                                    , icon     :: Icon
                                    , stats    :: { base :: Stat, max :: Stat }
                                    , effect   :: Array SkillEffect
                                    , bond     :: Maybe String
                                    , limited  :: Boolean
                                    }

instance _0_ :: Show CraftEssence where
    show (CraftEssence ce) = ce.name

getEffects :: CraftEssence -> Array SkillEffect
getEffects (CraftEssence ce) = filter (not <<< demerit) $
                               simplify <$> ce.effect

class (G.BoundedEnum a, Show a) <= MatchCraftEssence a where
    ceHas :: a -> Boolean -> CraftEssence -> Boolean
instance _b_ :: MatchCraftEssence BuffEffect where
    ceHas x noSelf = any match <<< getEffects where
        match (Grant t _ y _) = x == y && allied t && (not noSelf || t /= Self)
        match _ = false
instance _c_ :: MatchCraftEssence DebuffEffect where
    ceHas x _ = any match <<< getEffects where
        match (Debuff t _ y _) = x == y && not (allied t)
        match _ = false
instance _d_ :: MatchCraftEssence InstantEffect where
    ceHas x noSelf = any match <<< getEffects where
        match (To t y _) = x == y && (not noSelf || t /= Self)
        match _ = false
instance _e_ :: MatchCraftEssence BonusEffect where
    ceHas x _ = any match <<< getEffects where
        match (Bonus y _) = x == y
        match _ = false

equipped :: Class -> SkillEffect -> SkillEffect
equipped = When <<< append "equipped by a " <<< show
