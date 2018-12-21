module Database.CraftEssence exposing
  ( CraftEssence
  , craftEssences
  , getBond
  )

{-| All Craft Essences. -}
-- Unlike Servants, which are divided into multiple subfiles,
-- Craft Essences are all included in this one file
-- along with their data model definition.

import Dict exposing (Dict)
import Maybe.Extra as Maybe

import Database.Base    exposing (..)
import Database.Skill   exposing (..)
import Database.Servant exposing (..)

import Class.Show as Show

{-| All Craft Essences available in EN. -}
-- Note: Names _must_ be true to their EN localization.
-- GrandOrder.Wiki is only trustworthy for CEs that have been in the game
-- for a while. Craft Essences introduced during events and the like should be
-- checked against the official announcement.
craftEssences : List CraftEssence
craftEssences =
  let
    gutsPercent = Times 1 << Grant Self 0 GutsPercent << Flat
    self buff   = Grant Self 0 buff << Flat
    party buff  = Grant Party 0 buff << Flat
    party_ card = party (Performance card)
    demeritAll debuff = Debuff Party 0 debuff << Flat
    atkChance chance  = When "attacking" << Chance chance
    bond id name servant icon effect =
        { name     = name
        , id       = id
        , rarity   = 4
        , icon     = icon
        , stats    = { base = { atk = 100, hp = 100 }
                     , max  = { atk = 100, hp = 100 }
                     }
        , effect  = effect
        , bond    = Just servant
        , limited = False
        }
  in
    [ { name    = "Tenacity"
      , id      = 1
      , rarity  = 1
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 100 }
                  , max  = { atk = 0, hp = 300 }
                  }
      , effect  = [ Grant Self 0 DefenseUp <| Range 3 5 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Meditation"
      , id      = 2
      , rarity  = 1
      , icon    = IconHoodUp
      , stats   = { base = { atk = 0, hp = 150 }
                  , max  = { atk = 0, hp = 450 }
                  }
      , effect  = [ Grant Self 0 DebuffResist <| Range 5 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Technique"
      , id      = 3
      , rarity  = 1
      , icon    = IconArtsUp
      , stats   = { base = { atk = 100, hp = 0 }
                  , max  = { atk = 300, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 3 5 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Preemption"
      , id      = 4
      , rarity  = 1
      , icon    = IconQuickUp
      , stats   = { base = { atk = 100, hp = 0 }
                  , max  = { atk = 300, hp = 0 }
                  }
      , effect  = [Grant Self 0 (Performance Quick) <| Range 3 5]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Destruction"
      , id      = 5
      , rarity  = 1
      , icon    = IconBusterUp
      , stats   = { base = { atk = 100, hp = 0 }
                  , max  = { atk = 300, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 3 5 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Flash"
      , id      = 6
      , rarity  = 2
      , icon    = IconExclamationUp
      , stats   = { base = { atk = 150, hp = 0 }
                  , max  = { atk = 500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 CritUp <| Range 5 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Opportunity"
      , id      = 7
      , rarity  = 2
      , icon    = IconStarHaloUp
      , stats   = { base = { atk = 75,  hp = 112 }
                  , max  = { atk = 250, hp = 375 }
                  }
      , effect  = [ Grant Self 0 StarUp <| Range 5 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Fruitful"
      , id      = 8
      , rarity  = 2
      , icon    = IconNoble
      , stats   = { base = { atk = 75,  hp = 112 }
                  , max  = { atk = 250, hp = 375 }
                  }
      , effect  = [ To Self GaugeUp <| Range 10 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Concentration"
      , id      = 9
      , rarity  = 2
      , icon    = IconNobleUp
      , stats   = { base = { atk = 75,  hp = 112 }
                  , max  = { atk = 250, hp = 375 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 5 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Divine Oracle"
      , id      = 10
      , rarity  = 2
      , icon    = IconBeamUp
      , stats   = { base = { atk = 150, hp = 0 }
                  , max  = { atk = 500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 5 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Azoth Blade"
      , id      = 11
      , rarity  = 3
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 200 }
                  , max  = { atk = 0, hp = 1000 }
                  }
      , effect  = [ Grant Self 0 DefenseUp <| Range 8 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "False Attendant's Writings"
      , id      = 12
      , rarity  = 3
      , icon    = IconHoodUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 DebuffResist <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "The Azure Black Keys"
      , id      = 13
      , rarity  = 3
      , icon    = IconArtsUp
      , stats   = { base = { atk = 200, hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 8 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "The Verdant Black Keys"
      , id      = 14
      , rarity  = 3
      , icon    = IconQuickUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 8 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "The Crimson Black Keys"
      , id      = 15
      , rarity  = 3
      , icon    = IconBusterUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 8 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Rin's Pendant"
      , id      = 16
      , rarity  = 3
      , icon    = IconExclamationUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 CritUp <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Spell Tome"
      , id      = 17
      , rarity  = 3
      , icon    = IconStarHaloUp
      , stats   = { base = { atk = 100, hp = 150 }
                  , max  = { atk = 500, hp = 750 }
                  }
      , effect  = [ Grant Self 0 StarUp <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Dragon's Meridian"
      , id      = 18
      , rarity  = 3
      , icon    = IconNoble
      , stats   = { base = { atk = 100, hp = 150 }
                  , max  = { atk = 500, hp = 750 }
                  }
      , effect  = [ To Self GaugeUp <| Range 30 50 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Sorcery Ore"
      , id      = 19
      , rarity  = 3
      , icon    = IconNobleUp
      , stats   = { base = { atk = 100, hp = 150 }
                  , max  = { atk = 500, hp = 750 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Dragonkin"
      , id      = 20
      , rarity  = 3
      , icon    = IconBeamUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Iron-Willed Training"
      , id      = 21
      , rarity  = 4
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 400 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 DefenseUp <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Primeval Curse"
      , id      = 22
      , rarity  = 4
      , icon    = IconHoodUp
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ Grant Self 0 DebuffResist <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Projection"
      , id      = 23
      , rarity  = 4
      , icon    = IconArtsUp
      , stats   = { base = { atk = 400, hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Gandr"
      , id      = 24
      , rarity  = 4
      , icon    = IconQuickUp
      , stats   = { base = { atk = 400, hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Verdant Sound of Destruction"
      , id      = 25
      , rarity  = 4
      , icon    = IconBusterUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Gem Magecraft: Antumbra"
      , id      = 26
      , rarity  = 4
      , icon    = IconExclamationUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 CritUp <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Be Elegant"
      , id      = 27
      , rarity  = 4
      , icon    = IconStarHaloUp
      , stats   = { base = { atk = 200, hp = 300 }
                  , max  = { atk = 750, hp = 1125 }
                  }
      , effect  = [ Grant Self 0 StarUp <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "The Imaginary Element"
      , id      = 28
      , rarity  = 4
      , icon    = IconNoble
      , stats   = { base = { atk = 200, hp = 300 }
                  , max  = { atk = 750, hp = 1125 }
                  }
      , effect  = [ To Self GaugeUp <| Range 60 75 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Divine Banquet"
      , id      = 29
      , rarity  = 4
      , icon    = IconNobleUp
      , stats   = { base = { atk = 200, hp = 300 }
                  , max  = { atk = 750, hp = 1125 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Angel's Song"
      , id      = 30
      , rarity  = 4
      , icon    = IconBeamUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Formal Craft"
      , id      = 31
      , rarity  = 5
      , icon    = IconArtsUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Imaginary Around"
      , id      = 32
      , rarity  = 5
      , icon    = IconQuickUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Limited/Zero Over"
      , id      = 33
      , rarity  = 5
      , icon    = IconBusterUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Kaleidoscope"
      , id      = 34
      , rarity  = 5
      , icon    = IconNoble
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ To Self GaugeUp <| Range 80 100 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Heaven's Feel"
      , id      = 35
      , rarity  = 5
      , icon    = IconBeamUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 40 50 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Beginning of the Journey"
      , id      = 36
      , rarity  = 4
      , icon    = IconRoad
      , stats   = { base = { atk = 50,  hp = 50 }
                  , max  = { atk = 50, hp = 50 }
                  }
      , effect  = [ Bonus FriendPoints False <| Flat 75 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Parted Sea"
      , id      = 37
      , rarity  = 3
      , icon    = IconDodge
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Times 1 <| Grant Self 0 Evasion Full
                  , Grant Self 0 DebuffResist <| Range 5 10
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Seal Designation Enforcer"
      , id      = 38
      , rarity  = 4
      , icon    = IconStarUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 StarAbsorb <| Range 600 800 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Holy Shroud of Magdalene"
      , id      = 39
      , rarity  = 4
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 400 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 (Special DefenseUp <| VsTrait Male) <| Range 25 35 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Prisma Cosmos"
      , id      = 40
      , rarity  = 5
      , icon    = IconNobleTurn
      , stats   = { base = { atk = 250,  hp = 375 }
                  , max  = { atk = 1000, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 GaugePerTurn <| Range 8 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Nightless Rose"
      , id      = 41
      , rarity  = 5
      , icon    = IconKneel
      , stats   = { base = { atk = 0, hp = 500 }
                  , max  = { atk = 0, hp = 2000 }
                  }
      , effect  = [ Times 1 << Grant Self 0 Guts <| Range 500 1000 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Mooncell Automaton"
      , id      = 42
      , rarity  = 3
      , icon    = IconAllUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 3 5
                  , Grant Self 0 (Performance Buster) <| Range 3 5
                  , Grant Self 0 (Performance Quick) <| Range 3 5
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Moony Jewel"
      , id      = 43
      , rarity  = 4
      , icon    = IconHoodUp
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ Grant Self 0 (Resist Charm) <| Range 80 100 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Moon Goddess' Bath"
      , id      = 44
      , rarity  = 5
      , icon    = IconHealTurn
      , stats   = { base = { atk = 0, hp = 500 }
                  , max  = { atk = 0, hp = 2000 }
                  }
      , effect  = [ Grant Self 0 HealPerTurn <| Range 500 750 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Moonlight Fest"
      , id      = 45
      , rarity  = 5
      , icon    = IconStarHaloUp
      , stats   = { base = { atk = 250,  hp = 375 }
                  , max  = { atk = 1000, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 StarUp <| Range 15 20
                  , Grant Self 0 CritUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Runestone"
      , id      = 46
      , rarity  = 3
      , icon    = IconHoodUp
      , stats   = { base = { atk = 100, hp = 150 }
                  , max  = { atk = 500, hp = 750 }
                  }
      , effect  = [ Grant Self 0 DebuffResist <| Range 5 10
                  , Grant Self 0 StarAbsorb <| Range 100 200
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "With One Strike"
      , id      = 47
      , rarity  = 4
      , icon    = IconBullseye
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 SureHit Full
                  , Grant Self 0 (Performance Quick) <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "The Black Grail"
      , id      = 48
      , rarity  = 5
      , icon    = IconBeamUp
      , stats   = { base = { atk = 600,  hp = 0 }
                  , max  = { atk = 2400, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 60 80
                  , Debuff Self 0 HealthLoss <| Flat 500
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Jack-o'-lantern"
      , id      = 49
      , rarity  = 3
      , icon    = IconDamageUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 DamageUp <| Range 100 200 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Trick or Treat"
      , id      = 50
      , rarity  = 3
      , icon    = IconStaffUp
      , stats   = { base = { atk = 100, hp = 150 }
                  , max  = { atk = 500, hp = 750 }
                  }
      , effect  = [ Grant Self 0 DebuffSuccess <| Range 10 12 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Halloween Arrangement"
      , id      = 51
      , rarity  = 4
      , icon    = IconCrosshairUp
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ Grant Self 1 Taunt Full
                  , Grant Self 1 DefenseUp <| Range 60 80
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Halloween Princess"
      , id      = 52
      , rarity  = 5
      , icon    = IconBeamUp
      , stats   = { base = { atk = 250,  hp = 375 }
                  , max  = { atk = 1000, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 15 20
                  , To Self GaugeUp <| Range 30 50
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Little Halloween Devil"
      , id      = 53
      , rarity  = 5
      , icon    = IconNobleUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 20 25
                  , To Self GaugeUp <| Range 50 60
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Maid in Halloween"
      , id      = 54
      , rarity  = 5
      , icon    = IconHealUp
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Grant Self 0 HealingReceived <| Range 60 75 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Anchors Aweigh"
      , id      = 55
      , rarity  = 3
      , icon    = IconHealTurn
      , stats   = { base = { atk = 300,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 HealPerTurn <| Range 100 200 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Code Cast"
      , id      = 56
      , rarity  = 4
      , icon    = IconSwordShieldUp
      , stats   = { base = { atk = 200, hp = 300 }
                  , max  = { atk = 750, hp = 1125 }
                  }
      , effect  = [ Grant Self 3 AttackUp <| Range 25 30
                  , Grant Self 3 DefenseUp <| Range 25 30
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Victor of the Moon"
      , id      = 57
      , rarity  = 5
      , icon    = IconBusterUp
      , stats   = { base = { atk = 600,  hp = 0 }
                  , max  = { atk = 2400, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 10 15
                  , Grant Self 0 CritUp <| Range 20 25
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Another Ending"
      , id      = 58
      , rarity  = 5
      , icon    = IconArtsUp
      , stats   = { base = { atk = 600,  hp = 0 }
                  , max  = { atk = 2400, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 10 15
                  , Grant Self 0 CritUp <| Range 20 25
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Fate GUDAGUDA Order"
      , id      = 59
      , rarity  = 3
      , icon    = IconAllUp
      , stats   = { base = { atk = 100, hp = 150 }
                  , max  = { atk = 500, hp = 750 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 1 2
                  , Grant Self 0 (Performance Arts) <| Range 1 2
                  , Grant Self 0 (Performance Buster) <| Range 1 2
                  , Grant Self 0 StarUp <| Range 1 2
                  , Grant Self 0 StarAbsorb <| Range 1 2
                  , Grant Self 0 NPGen <| Range 1 2
                  , Grant Self 0 NPUp <| Range 1 2
                  , Grant Self 0 DebuffSuccess <| Range 1 2
                  , Grant Self 0 DebuffResist <| Range 1 2
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "After-Party Order!"
      , id      = 60
      , rarity  = 4
      , icon    = IconQuickBusterUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 10 15
                  , Grant Self 0 (Performance Buster) <| Range 10 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Guda-o"
      , id      = 61
      , rarity  = 5
      , icon    = IconDamageUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 CritUp <| Range 15 20
                  , Grant Self 0 NPUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "GUDAGUDA Poster Girl"
      , id      = 62
      , rarity  = 5
      , icon    = IconCrosshairUp
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Grant Self 3 Taunt Full
                  , Grant Self 3 AttackUp <| Range 60 80
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Demon Boar"
      , id      = 65
      , rarity  = 3
      , icon    = IconQuickUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Knight's Dignity"
      , id      = 66
      , rarity  = 4
      , icon    = IconExclamationUp
      , stats   = { base = { atk = 200, hp = 300 }
                  , max  = { atk = 750, hp = 1125 }
                  }
      , effect  = [ Grant Self 0 CritUp <| Range 40 50
                  , Debuff Self 0 DefenseDown <| Flat 20
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "A Fragment of 2030"
      , id      = 67
      , rarity  = 5
      , icon    = IconStarTurn
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Grant Party 0 StarsPerTurn <| Range 8 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Lightning Reindeer"
      , id      = 68
      , rarity  = 3
      , icon    = IconBusterUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 3 (Performance Buster) <| Range 15 20 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "March of the Saint"
      , id      = 69
      , rarity  = 4
      , icon    = IconHealTurn
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ Grant Self 0 HealPerTurn <| Range 200 300
                  , Grant Self 0 GaugePerTurn <| Range 3 4
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Present For My Master"
      , id      = 70
      , rarity  = 5
      , icon    = IconStarUp
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Grant Self 0 StarAbsorb <| Range 100 200
                  , Grant Self 0 HealingReceived <| Range 40 50
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Holy Night Sign"
      , id      = 71
      , rarity  = 5
      , icon    = IconQuickUp
      , stats   = { base = { atk = 250,  hp = 375 }
                  , max  = { atk = 1000, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 8 10
                  , Grant Self 0 CritUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Clock Tower"
      , id      = 72
      , rarity  = 3
      , icon    = IconNobleTurn
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 GaugePerTurn <| Range 2 3 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Necromancy"
      , id      = 73
      , rarity  = 4
      , icon    = IconKneel
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2400 }
                  }
      , effect  = [ Chance 50 << Times 1 << Grant Self 0 Guts <| Range 500 1000 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Awakened Will"
      , id      = 74
      , rarity  = 4
      , icon    = IconNobleTurn
      , stats   = { base = { atk = 200, hp = 300 }
                  , max  = { atk = 750, hp = 1125 }
                  }
      , effect  = [ Chance 60 << Grant Self 0 GaugePerTurn <| Range 12 15
                  , Debuff Self 0 HealthLoss <| Flat 500
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "500-Year Obsession"
      , id      = 75
      , rarity  = 5
      , icon    = IconCircuits
      , stats   = { base = { atk = 600,  hp = 0 }
                  , max  = { atk = 2400, hp = 0 }
                  }
      , effect  = [ When "defeated by an enemy" <| Debuff Target 2 SealNP Full
                  , When "defeated by an enemy" <<
                    Debuff Target 10 Curse <| Range 1000 2000
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Peacefulness of 2018"
      , id      = 76
      , rarity  = 3
      , icon    = IconHealTurn
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 HealPerTurn <| Range 200 300
                  , Debuff Self 0 AttackDown <| Flat 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic New Year"
      , id      = 77
      , rarity  = 4
      , icon    = IconHoodUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Times 1 <| Grant Self 0 DebuffResist Full
                  , Grant Self 0 DefenseUp <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Law of the Jungle"
      , id      = 78
      , rarity  = 3
      , icon    = IconQuartz
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Bonus QPQuest False <| Range 2017 2018 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Grand New Year"
      , id      = 79
      , rarity  = 5
      , icon    = IconShield
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Grant Self 1 Taunt Full
                  , Grant Self 1 Invincibility Full
                  , Grant Self 0 DebuffResist <| Range 10 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Mona Lisa"
      , id      = 80
      , rarity  = 5
      , icon    = IconQuartz
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Bonus QPDrop True <| Range 2 10 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Happy x3 Order"
      , id      = 81
      , rarity  = 4
      , icon    = IconStarTurn
      , stats   = { base = { atk = 0, hp = 2018 }
                  , max  = { atk = 0, hp = 2018 }
                  }
      , effect  = [ Grant Party 0 StarsPerTurn <| Range 0 1 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Purely Bloom"
      , id      = 82
      , rarity  = 5
      , icon    = IconBeamUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ ToMax (Range 40 50) << Grant Self 0 NPUp <| Flat 5 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Star of Altria"
      , id      = 83
      , rarity  = 5
      , icon    = IconKneel
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Times 1 << Grant Self 0 Guts <| Flat 1
                  , Grant Self 0 DebuffResist <| Range 5 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Trueshot"
      , id      = 84
      , rarity  = 3
      , icon    = IconBullseye
      , stats   = { base = { atk = 200, hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 SureHit Full
                  , Grant Self 0 CritUp <| Range 3 5
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Mikotto! Bride Training"
      , id      = 85
      , rarity  = 4
      , icon    = IconHealTurn
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ Chance 65 << Grant Self 0 HealPerTurn <| Range 750 1000 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "The Crimson Land of Shadows"
      , id      = 86
      , rarity  = 5
      , icon    = IconDamageUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ ToMax (Range 1000 1200) << Grant Self 0 DamageUp <| Flat 100 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Ryudoji Temple"
      , id      = 89
      , rarity  = 3
      , icon    = IconBeamUp
      , stats   = { base = { atk = 100, hp = 160 }
                  , max  = { atk = 500, hp = 800 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 10 15
                  , To Self GaugeUp <| Range 20 30
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Mana Gauge"
      , id      = 90
      , rarity  = 3
      , icon    = IconDamageUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0  }
                  }
      , effect  = [ Grant Self 0 (Special AttackUp <| VsClass Caster) <| Range 8 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Elixir of Love"
      , id      = 91
      , rarity  = 3
      , icon    = IconHeart
      , stats   = { base = { atk = 100, hp = 160 }
                  , max  = { atk = 500, hp = 800 }
                  }
      , effect  = [ Grant Self 0 (Success Charm) <| Range 12 15 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Storch Ritter"
      , id      = 92
      , rarity  = 3
      , icon    = IconBeamUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ equipped Berserker << Grant Self 0 NPUp <| Range 15 25 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Hermitage"
      , id      = 93
      , rarity  = 3
      , icon    = IconArtsUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 3 (Performance Arts) <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Motored Cuirassier"
      , id      = 94
      , rarity  = 3
      , icon    = IconDamageUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Special AttackUp <| VsClass Rider) <| Range 8 10 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Stuffed Lion"
      , id      = 95
      , rarity  = 3
      , icon    = IconHeal
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ When "defeated" << To Party Heal <| Range 800 1000 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Lugh's Halo"
      , id      = 96
      , rarity  = 3
      , icon    = IconHoodUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 (Resist Stun) <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Vessel of the Saint"
      , id      = 97
      , rarity  = 5
      , icon    = IconHoodUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Times 3 <| Grant Self 0 DebuffResist Full
                  , Grant Self 0 NPGen <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Golden Millennium Tree"
      , id      = 98
      , rarity  = 4
      , icon    = IconHPUp
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ ToMax (Flat 3000) << Grant Self 0 MaxHP <| Range 200 300 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Heroic Portrait: Mash Kyrielight"
      , id      = 99
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic Portrait: Altria Pendragon"
      , id      = 100
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic Portrait: Jeanne d'Arc"
      , id      = 101
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic Portrait: Altera"
      , id      = 102
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic Portrait: Arjuna"
      , id      = 103
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic Portrait: Scathach"
      , id      = 104
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic Portrait: Ushiwakamaru"
      , id      = 105
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic Portrait: Henry Jekyll & Hyde"
      , id      = 106
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic Portrait: Mephistopheles"
      , id      = 107
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Heroic Portrait: Darius III"
      , id      = 108
      , rarity  = 4
      , icon    = IconRainbow
      , stats   = { base = { atk = 500, hp = 500 }
                  , max  = { atk = 500, hp = 500 }
                  }
      , effect  = [ Bonus Bond False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Valentine Dojo of Tears"
      , id      = 109
      , rarity  = 3
      , icon    = IconBullseye
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 SureHit Full
                  , Grant Self 0 GaugePerTurn <| Range 3 5
                  , Debuff Self 0 CharmVuln <| Flat 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Kitchen ☆ Patissiere"
      , id      = 110
      , rarity  = 4
      , icon    = IconStarHaloUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 StarUp <| Range 15 20
                  , Grant Self 0 NPGen <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Street Choco-Maid"
      , id      = 111
      , rarity  = 5
      , icon    = IconArtsQuickUp
      , stats   = { base = { atk = 250, hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 10 15
                  , Grant Self 0 (Performance Quick) <| Range 10 15
                  , Grant Self 0 HealingReceived <| Range 20 30
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Melty Sweetheart"
      , id      = 112
      , rarity  = 5
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Times 3 << Grant Self 0 (Special DefenseUp <| VsTrait Male) <| Flat 100
                  , Grant Self 0 StarUp <| Range 10 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Decapitating Bunny 2018"
      , id      = 154
      , rarity  = 5
      , icon    = IconShieldBreak
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 IgnoreInvinc Full
                  , Grant Self 0 (Performance Quick) <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Mature Gentleman"
      , id      = 155
      , rarity  = 5
      , icon    = IconFire
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Grant Self 0 KillResist <| Range 60 80 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Grand Puppeteer"
      , id      = 156
      , rarity  = 5
      , icon    = IconNoble
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ To Self GaugeUp <| Range 50 60
                  , Grant Self 3 (Performance Arts) <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Threefold Barrier"
      , id      = 157
      , rarity  = 5
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Times 3 << Grant Self 0 DamageDown <| Range 1000 1200 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Vivid Dance of Fists"
      , id      = 158
      , rarity  = 4
      , icon    = IconDamageUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 DamageUp <| Range 800 1000 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Mystic Eyes of Distortion"
      , id      = 159
      , rarity  = 4
      , icon    = IconBusterUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 20 25
                  , Debuff Self 0 DefenseDown <| Flat 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Summer's Precognition"
      , id      = 160
      , rarity  = 4
      , icon    = IconDodge
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ Times 1 <| Grant Self 0 Evasion Full
                  , Grant Self 0 StarUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Chorus"
      , id      = 161
      , rarity  = 4
      , icon    = IconStarUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 StarAbsorb <| Range 300 400
                  , Grant Self 3 DebuffResist <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Sprinter"
      , id      = 162
      , rarity  = 3
      , icon    = IconQuickUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 5 8
                  , Grant Self 0 DebuffResist <| Range 10 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Repeat Magic"
      , id      = 163
      , rarity  = 3
      , icon    = IconNoble
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ To Self GaugeUp <| Range 20 30
                  , Grant Self 0 NPGen <| Range 10 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Kiss Your Hand"
      , id      = 165
      , rarity  = 5
      , icon    = IconAllUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 10 12
                  , Grant Self 0 (Performance Buster) <| Range 10 12
                  , Grant Self 0 (Performance Quick) <| Range 10 12
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Teacher and I"
      , id      = 166
      , rarity  = 5
      , icon    = IconNoble
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ To Self GaugeUp <| Range 5 60
                  , Grant Self 0 StarAbsorb <| Range 300 400
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Versus"
      , id      = 167
      , rarity  = 5
      , icon    = IconDamageUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Self 3 (Special AttackUp <| VsTrait Divine) <| Range 80 100
                  , Grant Self 3 (Special DefenseUp <| VsTrait Divine) <| Range 40 50
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Beasts Under the Moon"
      , id      = 168
      , rarity  = 4
      , icon    = IconNobleUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 12 15
                  , Grant Self 0 StarUp <| Range 12 15
                  , Grant Self 0 HealPerTurn <| Range 200 300
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Glass Full Sweet Time"
      , id      = 169
      , rarity  = 4
      , icon    = IconBullseye
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 SureHit Full
                  , Grant Self 0 DamageUp <| Range 400 600
                  , Grant Self 0 DamageDown <| Range 200 300
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Salon de Marie"
      , id      = 170
      , rarity  = 3
      , icon    = IconDodge
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Times 1 <| Grant Self 0 Evasion Full
                  , Grant Self 0 HealingReceived <| Range 5 10
                  , Grant Self 0 DebuffSuccess <| Range 3 5
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Prince of Slayer"
      , id      = 171
      , rarity  = 3
      , icon    = IconStarTurn
      , stats   = { base = { atk = 100, hp = 160 }
                  , max  = { atk = 500, hp = 800 }
                  }
      , effect  = [ Grant Party 0 StarsPerTurn <| Range 1 2
                  , Grant Self 0 (Special AttackUp <| VsTrait Dragon) <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Noisy Obsession"
      , id      = 172
      , rarity  = 4
      , icon    = IconExclamationUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 CritUp <| Range 15 20
                  , Grant Self 0 NPUp <| Range 15 20
                  , Grant Self 0 (Success Charm) <| Range 12 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "[Heaven's Feel]"
      , id      = 174
      , rarity  = 4
      , icon    = IconQuickUp
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Flat 10
                  , To Self GaugeUp <| Flat 40
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Ideal Holy King"
      , id      = 175
      , rarity  = 5
      , icon    = IconHPUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Party 0 MaxHP <| Range 1000 1200 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Record Holder"
      , id      = 176
      , rarity  = 4
      , icon    = IconStaffUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 DebuffSuccess <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Beast of Billows"
      , id      = 177
      , rarity  = 3
      , icon    = IconBeamUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ equipped Lancer << Grant Self 0 NPUp <| Range 15 25 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Personal Training"
      , id      = 178
      , rarity  = 5
      , icon    = IconRoad
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Bonus EXP True <| Range 2 10 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "The Scholars of Chaldea"
      , id      = 179
      , rarity  = 5
      , icon    = IconNoble
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ To Self GaugeUp <| Range 30 50
                  , Grant Self 0 HealingReceived <| Range 20 30
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Maiden Leading Chaldea"
      , id      = 180
      , rarity  = 5
      , icon    = IconStarTurn
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Party 0 StarsPerTurn <| Range 3 4
                  , Grant Self 0 (Performance Buster) <| Range 10 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "The Merciless One"
      , id      = 181
      , rarity  = 5
      , icon    = IconNoble
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ When "defeated" << To Party GaugeUp <| Range 15 20
                  , Grant Self 0 (Performance Buster) <| Range 10 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Art of the Poisonous Snake"
      , id      = 182
      , rarity  = 4
      , icon    = IconArtsUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 3 (Performance Arts) <| Range 30 40 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Art of Death"
      , id      = 183
      , rarity  = 4
      , icon    = IconDamageUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Special AttackUp <| VsTrait Humanoid) <| Range 25 30 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Gentle Affection"
      , id      = 184
      , rarity  = 4
      , icon    = IconHealUp
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ Grant Self 0 HealUp <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Volumen Hydrargyrum"
      , id      = 185
      , rarity  = 5
      , icon    = IconShield
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Times 3 <| Grant Self 0 Invincibility Full
                  , Grant Self 0 DamageUp <| Range 200 500
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Innocent Maiden"
      , id      = 186
      , rarity  = 4
      , icon    = IconNobleTurn
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 GaugePerTurn <| Range 4 5
                  , Grant Self 0 (Performance Quick) <| Range 10 12
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Self Geas Scroll"
      , id      = 187
      , rarity  = 3
      , icon    = IconStaffUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 (Success Stun) <| Range 12 15 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Before Awakening"
      , id      = 188
      , rarity  = 5
      , icon    = IconAllUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 8 10
                  , Grant Self 0 (Performance Buster) <| Range 8 10
                  , Grant Self 0 (Performance Quick) <| Range 8 10
                  , Grant Self 0 DefenseUp <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "His Rightful Place"
      , id      = 189
      , rarity  = 5
      , icon    = IconStarTurn
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Party 0 StarsPerTurn <| Range 3 4
                  , To Self GaugeUp <| Range 30 50
                  ]
      , bond    = Nothing
      , limited = True
      }
    , bond 191 "Crown of the Star" "Altria Pendragon" IconDamageUp
      [ party AttackUp 15 ]
    , bond 192 "Relic of the King" "Zhuge Liang (Lord El-Melloi II)" IconBusterUp
      [ party_ Buster 15 ]
    , bond 193 "Triumph of the Lord Impaler" "Vlad III" IconBeamUp
      [ self NPUp 30, atkChance 30 << To Self GaugeUp <| Flat 5 ]
    , bond 194 "Revelation from Heaven" "Jeanne d'Arc" IconBusterUp
      [ party_ Buster 15 ]
    , bond 195 "Memories of the Dragon" "Altria Pendragon (Alter)" IconBeamUp
      [ self NPUp 30, atkChance 30 << Debuff Target 3 DefenseDown <| Flat 5 ]
    , bond 196 "Hunter of the Red Plains" "EMIYA" IconBeamUp
      [ self NPUp 30, atkChance 30 << To Party GainStars <| Flat 5 ]
    , bond 197 "Castle of Snow" "Heracles" IconKneel
      [ Times 3 <| self Guts 500 ]
    , bond 198 "Yggdrasil Tree" "Cu Chulainn (Caster)" IconBeamUp
      [ self NPUp 30, atkChance 30 << To Self Heal <| Flat 500 ]
    , bond 199 "Scorching Embrace" "Kiyohime" IconBeamUp
      [ self NPUp 30, atkChance 30 << Debuff Target 5 Burn <| Flat 500 ]
    , bond 200 "Worthless Jewel" "Mata Hari" IconNobleUp
      [ party NPGen 15 ]
    , bond 201 "Eternal Solitude" "Altera" IconSwordUp
      [ party AttackUp 15 ]
    , bond 202 "Queen's Present" "Chevalier d'Eon" IconArtsUp
      [ party_ Arts 15 ]
    , bond 203 "Elixir" "Elisabeth Bathory" IconHealTurn
      [ party HealPerTurn 500 ]
    , bond 204 "My Necklace" "Marie Antoinette" IconStarHaloUp
      [ party StarUp 20 ]
    , bond 205 "Staff He Gave Me" "Martha" IconHealUp
      [ party HealingReceived 30 ]
    , bond 206 "Iron Maiden" "Carmilla" IconBeamUp
      [ self NPUp 30, atkChance 10 <| Debuff Target 1 SealNP Full ]
    , bond 207 "Cat Apron" "Tamamo Cat" IconHeal
      [ party MaxHP 2000 ]
    , bond 208 "Thirst for Victory" "Boudica" IconStarHaloUp
      [ party StarUp 20 ]
    , bond 209 "To My Dear Friend" "Hans Christian Andersen" IconHoodUp
      [ party DebuffResist 30 ]
    , bond 210 "Sacred Devotion" "Arash" IconHeal
      [ When "defeated" <| To Party RemoveDebuffs Full
      , When "defeated" <| To Party Heal <| Flat 5000
      ]
    , { name    = "The Wandering Tales of Shana-oh"
      , id      = 211
      , rarity  = 5
      , icon    = IconQuickUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 10 15
                  , When "defeated" <<
                    Grant Party 1 (Performance Quick) <| Range 20 30
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Golden Captures the Carp"
      , id      = 212
      , rarity  = 5
      , icon    = IconNoble
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ To Self GaugeUp <| Range 30 50
                  , To Party GainStars <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "A Fox Night's Dream"
      , id      = 213
      , rarity  = 5
      , icon    = IconNobleUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 20 25
                  , Grant Party 0 StarsPerTurn <| Range 3 4
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Burning Tale of Love"
      , id      = 214
      , rarity  = 4
      , icon    = IconDamageUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Special AttackUp <| VsTrait Male) <| Range 25 30
                  , Grant Self 0 DebuffSuccess <| Range 12 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Reciting the Subscription List"
      , id      = 215
      , rarity  = 3
      , icon    = IconHoodUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Times 1 <| Grant Self 0 DebuffResist Full ]
      , bond    = Nothing
      , limited = True
      }
    , bond 216 "Key of the King's Law" "Gilgamesh" IconBeamUp
      [ self NPUp 30, atkChance 30 << Grant Self 3 CritUp <| Flat 10 ]
    , bond 217 "Golden Glass" "Sakata Kintoki" IconBeamUp
      [ self NPUp 30, atkChance 30 << To Self GaugeUp <| Flat 5 ]
    , bond 218 "Thunderous Applause" "Nero Claudius" IconArtsUp
      [ party_ Arts 15 ]
    , bond 219 "Das Rheingold" "Siegfried" IconNobleUp
      [ party NPGen 15 ]
    , bond 220 "Radiance of the Goddess" "Stheno" IconQuickUp
      [ party_ Quick 15 ]
    , bond 221 "Voyage of the Flowers" "Altria Pendragon (Lily)" IconSwordUp
      [ party AttackUp 10, party StarUp 10 ]
    , bond 222 "Ark of the Covenant" "David" IconBeamUp
      [ self NPUp 30, atkChance 10 <| To Target Kill Full ]
    , bond 223 "Door to Babylon" "Darius III" IconBusterUp
      [ party_ Buster 15 ]
    , bond 224 "Blood-Thirsting Axe" "Eric Bloodaxe" IconExclamationUp
      [ party CritUp 25 ]
    , bond 225 "Insurrection" "Spartacus" IconKneel
      [ gutsPercent 50 ]
    , { name    = "Go West!!"
      , id      = 226
      , rarity  = 5
      , icon    = IconBeamUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 20 25
                  , Grant Party 0 StarsPerTurn <| Range 3 4
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "The Classic Three Great Heroes"
      , id      = 227
      , rarity  = 5
      , icon    = IconBeamUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 15 20
                  , Grant Self 0 StarUp <| Range 15 20
                  , To Self GaugeUp <| Range 25 40
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "True Samadhi Fire"
      , id      = 228
      , rarity  = 4
      , icon    = IconBeamUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 15 20
                  , Grant Self 0 (Performance Buster) <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "All Three Together"
      , id      = 229
      , rarity  = 3
      , icon    = IconStarUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 StarAbsorb <| Range 100 200
                  , Grant Self 0 CritUp <| Range 5 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , bond 230 "Tristar Belt" "Orion" IconExclamationUp
      [ party CritUp 25 ]
    , bond 231 "Golden Helm" "Francis Drake" IconBeamUp
      [ party NPUp 20 ]
    , bond 232 "Black Knight's Helmet" "Lancelot" IconBeamUp
      [ self NPUp 30, atkChance 30 << Debuff Target 3 CritChance <| Flat 30 ]
    , bond 233 "Golden Apple" "Atalante" IconQuickUp
      [ party_ Quick 15 ]
    , bond 234 "Holy Pumpkin Grail" "Elisabeth Bathory (Halloween)" IconHoodUp
      [ party DebuffResist 30 ]
    , bond 235 "Rotary Matchlock" "Oda Nobunaga" IconExclamationUp
      [ party CritUp 25 ]
    , bond 236 "Llamrei Unit II" "Altria Pendragon (Santa Alter)" IconStarHaloUp
      [ party StarUp 20 ]
    , bond 237 "Things to Calm the Heart" "Henry Jekyll & Hyde" IconBusterUp
      [ party_ Buster 15 ]
    , bond 238 "Glory of the Past Days" "Edward Teach" IconBusterUp
      [ party_ Buster 15 ]
    , bond 239 "Heaven Among the Mountains" "Sasaki Kojirou" IconQuickUp
      [ party_ Quick 15 ]
    , { name    = "Divine Princess of the Storm"
      , id      = 240
      , rarity  = 5
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ When "defeated" << Grant Party 3 DefenseUp <| Range 20 25 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Ox-Demon King"
      , id      = 241
      , rarity  = 5
      , icon    = IconBusterUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Party 3 (Performance Buster) <| Range 10 15 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Personal Lesson"
      , id      = 242
      , rarity  = 5
      , icon    = IconRoad
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Bonus MysticCode True <| Flat 2 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Bronze-Link Manipulator"
      , id      = 243
      , rarity  = 3
      , icon    = IconSwordUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 3 AttackUp <| Range 15 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Ath nGabla"
      , id      = 244
      , rarity  = 3
      , icon    = IconQuickUp
      , stats   = { base = { atk = 100, hp = 160 }
                  , max  = { atk = 500, hp = 800 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 10 15
                  , Debuff Self 0 DefenseDown <| Flat 10
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Bygone Dream"
      , id      = 245
      , rarity  = 3
      , icon    = IconBeamUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ equipped Assassin << Grant Self 0 NPUp <| Range 15 25 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Extremely Spicy Mapo Tofu"
      , id      = 246
      , rarity  = 3
      , icon    = IconHealUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 HealingReceived <| Range 10 20 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Jeweled Sword Zelretch"
      , id      = 247
      , rarity  = 3
      , icon    = IconNobleUp
      , stats   = { base = { atk = 100, hp = 160 }
                  , max  = { atk = 500, hp = 800 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 5 10
                  , To Self GaugeUp <| Range 25 40
                  ]
      , bond    = Nothing
      , limited = False
      }
    , bond 248 "Tamamo's Club" "Tamamo-no-Mae" IconArtsUp
      [ party_ Arts 15 ]
    , bond 249 "Headband of Resolve" "Okita Souji" IconExclamationUp
      [ party CritUp 25 ]
    , bond 250 "Calico Jack" "Anne Bonny & Mary Read" IconExclamationUp
      [ party CritUp 25 ]
    , bond 251 "Gazing Upon Dun Scaith" "Scathach" IconQuickUp
      [ party_ Quick 15 ]
    , bond 252 "Star of Prophecy" "Cu Chulainn" IconBeamUp
      [ self NPUp 30, atkChance 30 << Grant Self 3 CritUp <| Flat 10 ]
    , bond 253 "Hekate's Staff" "Medea" IconArtsUp
      [ party_ Arts 15 ]
    , bond 254 "Formless Island" "Medusa" IconNobleUp
      [ party NPGen 15 ]
    , bond 255 "Cask of the Wise" "Alexander" IconQuickUp
      [ party_ Quick 15 ]
    , bond 256 "Shaytan's Arm" "Hassan of the Cursed Arm" IconReaperUp
      [ party KillUp 20 ]
    , bond 257 "Ariadne's Thread" "Asterios" IconQuickUp
      [ party_ Quick 15 ]
    , { name    = "Dumplings Over Flowers"
      , id      = 258
      , rarity  = 5
      , icon    = IconQuickUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 15 20
                  , Grant Self 0 NPUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Faithful Companions"
      , id      = 259
      , rarity  = 4
      , icon    = IconArtsUp
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 8 10
                  , Grant Self 0 NPGen <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Hidden Sword: Pheasant Reversal"
      , id      = 260
      , rarity  = 3
      , icon    = IconQuickUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 3 5
                  , Grant Self 0 CritUp <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Golden Sumo: Boulder Tournament"
      , id      = 261
      , rarity  = 5
      , icon    = IconSwordUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 AttackUp <| Range 10 15
                  , To Self GaugeUp <| Range 30 50
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Hot Spring Under the Moon"
      , id      = 262
      , rarity  = 5
      , icon    = IconExclamationUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 CritUp <| Range 20 25
                  , Grant Self 0 StarsPerTurn <| Range 3 4
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Origin Bullet"
      , id      = 263
      , rarity  = 5
      , icon    = IconShieldBreak
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 IgnoreInvinc Full
                  , Grant Self 0 (Special AttackUp <| VsClass Caster) <| Range 35 40
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Covering Fire"
      , id      = 264
      , rarity  = 4
      , icon    = IconDamageUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 DamageUp <| Range 400 600
                  , Grant Self 0 CritUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Battle of Camlann"
      , id      = 265
      , rarity  = 3
      , icon    = IconNoble
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ When "defeated" << To Party GaugeUp <| Range 10 15 ]
      , bond    = Nothing
      , limited = False
      }
    , bond 266 "Who Am I?" "Mordred" IconBeamUp
      [ party NPUp 20 ]
    , bond 267 "The Misty Night of London" "Jack the Ripper" IconExclamationUp
      [ party CritUp 25 ]
    , bond 268 "Wonderland" "Nursery Rhyme" IconExclamationUp
      [ party CritUp 15, party HealingReceived 10 ]
    , bond 269 "Faceless King" "Robin Hood" IconArtsUp
      [ party_ Arts 15 ]
    , bond 270 "Usumidori" "Ushiwakamaru" IconQuickUp
      [ party_ Quick 15 ]
    , bond 271 "Etiquette of Nine Guests" "Jing Ke" IconBeamUp
      [ self NPUp 30, atkChance 30 << Grant Self 3 KillUp <| Flat 30 ]
    , bond 272 "Heaven Scorcher Halberd" "Lu Bu Fengxian" IconBusterUp
      [ party_ Buster 15 ]
    , bond 273 "What can be Left Behind" "Georgios" IconShield
      [ When "defeated" << Times 1 <| Grant Party 0 Invincibility Full
      , When "defeated" << Grant Party 3 DamageDown <| Flat 1000
      ]
    , bond 274 "Thermopylae" "Leonidas I" IconBusterUp
      [ party_ Buster 15 ]
    , bond 275 "Haydn Quartets" "Wolfgang Amadeus Mozart" IconBeamUp
      [ party NPUp 20 ]
    , { name    = "Anniversary Heroines"
      , id      = 276
      , rarity  = 4
      , icon    = IconSwordUp
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Grant Self 0 AttackUp <| Flat 10
                  , Grant Self 0 StarsPerTurn <| Flat 3
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Leisure Stroll"
      , id      = 277
      , rarity  = 5
      , icon    = IconStarUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Grant Self 0 StarAbsorb <| Range 400 600
                  , Grant Self 0 (Performance Arts) <| Range 10 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Partake with the King"
      , id      = 278
      , rarity  = 5
      , icon    = IconBusterUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 10 15
                  , To Self GaugeUp <| Range 50 60
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Goldfish Scooping"
      , id      = 279
      , rarity  = 4
      , icon    = IconBullseye
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 SureHit Full
                  , Grant Self 0 (Performance Buster) <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Fire Flower"
      , id      = 280
      , rarity  = 3
      , icon    = IconStarHaloUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 StarUp <| Range 5 10
                  , Grant Self 0 CritUp <| Range 5 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , bond 281 "Arm of Raiden" "Nikola Tesla" IconBeamUp
      [ party NPUp 20 ]
    , bond 282 "Endowed Hero" "Arjuna" IconBeamUp
      [ self NPUp 30, self StarAbsorb 1000 ]
    , bond 283 "Lamp of the Unfortunate" "Karna" IconAllUp
      [ party_ Quick 8, party_ Arts 8, party_ Buster 8 ]
    , bond 284 "Procedure to Humanity" "Frankenstein" IconQuickUp
      [ party_ Quick 15 ]
    , bond 285 "Black Helmet" "Altria Pendragon (Lancer Alter)" IconNobleUp
      [ party NPGen 15 ]
    , bond 286 "Legend of the Gallic War" "Gaius Julius Caesar" IconQuickUp
      [ party_ Quick 15 ]
    , bond 287 "Rome" "Romulus" IconBeamUp
      [ party NPUp 20 ]
    , bond 288 "Encounter at Gojou Bridge" "Musashibou Benkei" IconNobleRedUp
      [ party NPFromDamage 20 ]
    , bond 289 "Impure Death Mask" "Phantom of the Opera" IconQuickUp
      [ party_ Quick 15 ]
    , bond 290 "Really Convenient" "William Shakespeare" IconNobleUp
      [ party NPGen 15 ]
    , { name    = "Pirates Party!"
      , id      = 291
      , rarity  = 5
      , icon    = IconShieldBreak
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 IgnoreInvinc Full
                  , Grant Party 0 StarsPerTurn <| Range 3 4
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Summertime Mistress"
      , id      = 292
      , rarity  = 5
      , icon    = IconExclamationUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 CritUp <| Range 15 20
                  , To Self GaugeUp <| Range 30 50
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Twilight Memory"
      , id      = 293
      , rarity  = 4
      , icon    = IconQuickUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Times 1 <| Grant Self 0 Evasion Full
                  , Grant Self 0 (Performance Quick) <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Shiny Goddess"
      , id      = 294
      , rarity  = 3
      , icon    = IconArtsUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 DefenseUp <| Range 3 5
                  , Grant Self 0 (Performance Arts) <| Range 3 5
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Knights of Marines"
      , id      = 295
      , rarity  = 5
      , icon    = IconQuickUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 10 15
                  , To Self GaugeUp <| Range 50 60
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Chaldea Lifesavers"
      , id      = 296
      , rarity  = 5
      , icon    = IconKneel
      , stats   = { base = { atk = 0, hp = 750 }
                  , max  = { atk = 0, hp = 3000 }
                  }
      , effect  = [ Times 1 << Grant Self 0 Guts <| Flat 1
                  , Grant Self 0 NPGen <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Meat Wars"
      , id      = 297
      , rarity  = 4
      , icon    = IconHealTurn
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 HealPerTurn <| Range 200 300
                  , Grant Self 0 (Performance Arts) <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Shaved Ice (Void's Dust Flavor)"
      , id      = 298
      , rarity  = 3
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 DamageDown <| Range 100 200
                  , Grant Self 0 DebuffResist <| Range 5 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , bond 299 "Annihilation List" "Mysterious Heroine X" IconDamageUp
      [ party (Special AttackUp <| VsClass Saber) 20 ]
    , bond 300 "Imperishable Flames" "Brynhild" IconBusterUp
      [ party_ Buster 10, party NPGen 10 ]
    , bond 301 "Ring of Bay Laurel" "Nero Claudius (Bride)" IconArtsUp
      [ party_ Arts 15 ]
    , bond 302 "Final Battle" "Beowulf" IconDamageUp
      [ party (Special AttackUp <| VsTrait Dragon) 20 ]
    , bond 303 "Bratan of Wisdom" "Fionn mac Cumhaill" IconArtsUp
      [ party_ Arts 10, party NPUp 10 ]
    , bond 304 "Prelati's Spellbook" "Gilles de Rais" IconBusterUp
      [ party_ Buster 20, demeritAll DebuffVuln 20 ]
    , bond 305 "Parasitic Bomb" "Mephistopheles" IconBeamUp
      [ party NPUp 20 ]
    , bond 306 "Seethe of a Warrior" "Fergus mac Roich" IconBusterUp
      [ party_ Buster 10, party NPUp 10 ]
    , bond 307 "My Loathsome Life" "Charles-Henri Sanson" IconReaperUp
      [ party KillUp 10, party NPGen 10 ]
    , bond 308 "There is No Love Here" "Caligula" IconBusterUp
      [ party_ Buster 20, demeritAll DefenseDown 10 ]
    , { name    = "Magical Girl of Sapphire"
      , id      = 309
      , rarity  = 5
      , icon    = IconNobleUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 25 30
                  , To Self GaugeUp <| Range 40 50
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Kill on Sight"
      , id      = 310
      , rarity  = 4
      , icon    = IconArtsUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 8 10
                  , Grant Self 0 NPUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Zunga Zunga!"
      , id      = 311
      , rarity  = 3
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 DamageDown <| Range 100 200
                  , Grant Self 0 HealingReceived <| Range 5 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Kaleid Ruby"
      , id      = 312
      , rarity  = 4
      , icon    = IconBusterUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 10 15
                  , Grant Self 0 NPUp <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Kaleid Sapphire"
      , id      = 313
      , rarity  = 4
      , icon    = IconArtsUp
      , stats   = { base = { atk = 400,  hp = 0 }
                  , max  = { atk = 1500, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 10 15
                  , Grant Self 0 NPUp <| Range 8 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , bond 315 "Mugashiki—Shinkuu Myou" "Ryougi Shiki (Saber)" IconArtsUp
      [ party_ Arts 15 ]
    , bond 317 "Château d'If" "Edmond Dantes" IconQuickUp
      [ party_ Quick 15 ]
    , bond 318 "Unlimited Pancakes" "Medea (Lily)" IconHealUp
      [ party HealingReceived 30 ]
    , bond 319 "Red Leather Jacket" "Ryougi Shiki (Assassin)" IconReaperUp
      [ party KillUp 30 ]
    , bond 321 "Letter From a Friend" "Gilles de Rais (Caster)" IconBusterUp
      [ party_ Buster 20, demeritAll StarDown 20 ]
    , bond 322 "Hound of Culann" "Cu Chulainn (Prototype)" IconDamageUp
      [ party (Special AttackUp <| VsTrait Beast) 20 ]
    , bond 323 "Radiance of the Goddess (Euryale)" "Euryale" IconArtsUp
      [ party_ Arts 15 ]
    , bond 324 "Hero's Armament" "Hektor" IconBeamUp
      [ party NPUp 20 ]
    , { name    = "Glory Is With Me"
      , id      = 325
      , rarity  = 5
      , icon    = IconBeamUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 15 20
                  , Grant Self 0 CritUp <| Range 15 20
                  , Grant Self 0 StarsPerTurn <| Range 3 4
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Original Legion"
      , id      = 326
      , rarity  = 4
      , icon    = IconShieldUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 DefenseUp <| Range 8 20
                  , Grant Self 0 NPUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Howl at the Moon"
      , id      = 327
      , rarity  = 3
      , icon    = IconBusterUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 10 15
                  , Debuff Self 0 DebuffVuln <| Flat 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Princess of the White Rose"
      , id      = 328
      , rarity  = 5
      , icon    = IconKneel
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Times 1 << Grant Self 0 Guts <| Flat 1
                  , To Self GaugeUp <| Range 10 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Joint Recital"
      , id      = 329
      , rarity  = 5
      , icon    = IconBusterUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 15 20
                  , Grant Self 0 CritUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Chaldea Lunchtime"
      , id      = 330
      , rarity  = 5
      , icon    = IconRainbow
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ Bonus Bond True <| Range 2 10 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Fragarach"
      , id      = 331
      , rarity  = 3
      , icon    = IconStarUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 StarAbsorb <| Range 200 300 ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Inverted Moon of the Heavens"
      , id      = 332
      , rarity  = 3
      , icon    = IconStarTurn
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 StarsPerTurn <| Range 1 2
                  , Grant Self 0 DebuffResist <| Range 5 10
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Hydra Dagger"
      , id      = 333
      , rarity  = 3
      , icon    = IconReaperUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 KillUp <| Range 5 10 ]
      , bond    = Nothing
      , limited = False
      }
    , bond 334 "Indomitableness" "Florence Nightingale" IconBusterUp
      [ party_ Buster 10, party HealingReceived 20 ]
    , bond 335 "One-Man War" "Cu Chulainn (Alter)" IconKneel
      [ self NPUp 30, gutsPercent 20 ]
    , bond 336 "Sacred Spring" "Queen Medb" IconNobleUp
      [ party NPGen 15 ]
    , bond 337 "Indestructible Blade" "Rama" IconExclamationUp
      [ party CritUp 25 ]
    , bond 338 "Concealed Goddess" "Helena Blavatsky" IconDamageUp
      [ party (Special AttackUp <| VsClass Assassin) 20 ]
    , bond 339 "Lights of Civilization" "Thomas Edison" IconNobleUp
      [ party NPGen 15 ]
    , bond 340 "Reaching the Zenith of My Skill" "Li Shuwen" IconArtsUp
      [ party_ Arts 15 ]
    , bond 341 "Knight's Oath" "Diarmuid Ua Duibhne" IconArtsUp
      [ party_ Quick 10, party_ Arts 10 ]
    , bond 342 "Elemental" "Paracelsus von Hohenheim" IconArtsUp
      [ party_ Arts 10, party NPUp 10 ]
    , bond 343 "NEO Difference Engine" "Charles Babbage" IconBusterUp
      [ party_ Buster 20, demeritAll DefenseDown 10 ]
    , { name    = "Dangerous Beast"
      , id      = 344
      , rarity  = 5
      , icon    = IconQuickUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 15 20
                  , Grant Party 0 StarsPerTurn <| Range 3 4
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Witch Under the Moonlight"
      , id      = 345
      , rarity  = 4
      , icon    = IconArtsUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 10 15
                  , Grant Self 0 NPGen <| Range 5 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Count Romani Archaman's Hospitality"
      , id      = 346
      , rarity  = 3
      , icon    = IconNobleUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 5 10
                  , Grant Self 0 DefenseUp <| Range 3 5
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Hero Elly's Adventure"
      , id      = 347
      , rarity  = 5
      , icon    = IconBusterUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Buster) <| Range 10 15
                  , Grant Self 0 NPUp <| Range 20 25
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Wizard & Priest"
      , id      = 348
      , rarity  = 4
      , icon    = IconBeamUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 NPUp <| Range 15 20
                  , Grant Self 0 HealingReceived <| Range 10 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Mata Hari's Tavern"
      , id      = 349
      , rarity  = 3
      , icon    = IconAllUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Arts) <| Range 2 3
                  , Grant Self 0 (Performance Buster) <| Range 2 3
                  , Grant Self 0 (Performance Quick) <| Range 2 3
                  , Grant Self 0 CritUp <| Range 5 10
                  ]
      , bond    = Nothing
      , limited = True
      }
    , bond 350 "Hell of Blazing Punishment" "Jeanne d'Arc (Alter)" IconBusterUp
      [ party_ Buster 15 ]
    , bond 351 "Gordian Knot" "Iskandar" IconSwordUp
      [ party AttackUp 15 ]
    , bond 352 "Bai Long" "Xuanzang Sanzang" IconBusterUp
      [ party_ Buster 20, demeritAll DefenseDown 10 ]
    , bond 353 "The Sun Shines Here" "Emiya (Assassin)" IconArtsQuickUp
      [ party_ Quick 10, party_ Arts 10 ]
    , bond 354 "Dress of Heaven" "Irisviel (Holy Grail)" IconHealUp
      [ party HealingReceived 30 ]
    , bond 355 "Manifestation of The Golden Rule" "Gilgamesh (Child)" IconNobleUp
      [ party NPGen 15 ]
    , bond 356 "Spirit of The Vast Land" "Geronimo" IconNobleUp
      [ party NPGen 15 ]
    , bond 357 "Extolling The Revolver" "Billy the Kid" IconExclamationUp
      [ party CritUp 25 ]
    , bond 358 "Library of Hundred Men" "Hassan of the Hundred Personas" IconAllUp
      [ party_ Buster 8, party_ Quick 8, party_ Arts 8 ]
    , bond 359 "Last Splinter" "Angra Mainyu" IconDamageUp
      [ self (Special AttackUp <| VsTrait Beast) 200, gutsPercent 20 ]
    , { name    = "Fate/EXTELLA"
      , id      = 360
      , rarity  = 4
      , icon    = IconExclamationUp
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Grant Self 0 CritUp <| Flat 15
                  , Grant Party 0 StarsPerTurn <| Flat 3
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Spiritron Portrait: Nero Claudius"
      , id      = 361
      , rarity  = 4
      , icon    = IconRoad
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Bonus EXP False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Spiritron Portrait: Nameless"
      , id      = 362
      , rarity  = 4
      , icon    = IconRoad
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Bonus EXP False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Spiritron Portrait: Tamamo-no-Mae"
      , id      = 363
      , rarity  = 4
      , icon    = IconRoad
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Bonus EXP False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Spiritron Portrait: Karna"
      , id      = 364
      , rarity  = 4
      , icon    = IconRoad
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Bonus EXP False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Spiritron Portrait: Altera"
      , id      = 365
      , rarity  = 4
      , icon    = IconRoad
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Bonus EXP False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Spiritron Portrait: Gilgamesh"
      , id      = 366
      , rarity  = 4
      , icon    = IconRoad
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Bonus EXP False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , bond 367 "Divine Oni-Poison Sake" "Shuten-Douji" IconArtsQuickUp
      [ party_ Quick 10, party_ Arts 10 ]
    , bond 368 "Doujigiri Yasutsuna" "Minamoto-no-Raikou" IconBusterUp
      [ party_ Buster 10, party CritUp 15 ]
    , bond 369 "Ramesseum" "Ozymandias" IconBusterArtsUp
      [ party_ Arts 10, party_ Buster 10 ]
    , bond 370 "Bone Sword (Nameless)" "Ibaraki-Douji" IconBusterUp
      [ party_ Buster 20, demeritAll DefenseDown 10 ]
    , bond 371 "Unit Golden Bear" "Sakata Kintoki (Rider)" IconStarHaloUp
      [ party StarUp 20 ]
    , bond 372 "Gringolet" "Gawain" IconBusterUp
      [ party_ Buster 15 ]
    , bond 373 "But I Lied Once" "Tristan" IconExclamationUp
      [ party CritUp 25 ]
    , bond 374 "Exercising the Royal Authority" "Nitocris" IconNobleUp
      [ party NPGen 10, party NPUp 10 ]
    , bond 375 "Mask of A Demon" "Fuuma \"Evil-wind\" Kotarou" IconQuickUp
      [ party_ Quick 15 ]
    , bond 376 "Cook Despite of Exhaustion" "Tawara Touta" IconHealTurn
      [ party HealPerTurn 500 ]
    , bond 377 "Dun Stallion" "Altria Pendragon (Lancer)" IconSwordUp
      [ party AttackUp 10, party NPUp 10 ]
    , bond 378 "All-Encompassing Wisdom" "Leonardo da Vinci" IconBeamUp
      [ party NPUp 20 ]
    , bond 379 "Sunset Beach" "Tamamo-no-Mae (Lancer)" IconQuickBusterUp
      [ party_ Quick 10, party_ Buster 10 ]
    , bond 380 "Lady of the Lake" "Lancelot (Saber)" IconNobleUp
      [ party NPGen 10, party CritUp 10 ]
    , bond 381 "Reminiscence of the Summer"
              "Marie Antoinette (Caster)" IconExclamationUp
      [ party CritUp 25 ]
    , bond 382 "Currently In The Middle Of A Shower"
              "Anne Bonny & Mary Read (Archer)" IconBusterArtsUp
      [ party_ Buster 10, party_ Arts 10 ]
    , bond 383 "Prydwen" "Mordred (Rider)" IconBeamUp
      [ party NPUp 20 ]
    , bond 384 "Beach Love Letter (Terror)" "Kiyohime (Lancer)" IconBusterUp
      [ party_ Buster 20, demeritAll DefenseDown 10 ]
    , bond 385 "My Long Lost Right Arm" "Bedivere" IconBusterUp
      [ party_ Buster 10, party NPGen 10 ]
    , bond 386 "Proof of Existence" "Hassan of the Serenity" IconQuickUp
      [ party_ Quick 15 ]
    , { name    = "A Moment of Tranquility"
      , id      = 387
      , rarity  = 5
      , icon    = IconArtsQuickUp
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ Grant Self 0 (Performance Quick) <| Range 10 15 
                  , Grant Self 0 (Performance Arts) <| Range 10 15
                  , Grant Self 0 NPGen <| Range 10 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Reading on the Holy Night"
      , id      = 388
      , rarity  = 4
      , icon    = IconNobleUp
      , stats   = { base = { atk = 200, hp = 320 }
                  , max  = { atk = 750, hp = 1200 }
                  }
      , effect  = [ Grant Self 0 NPGen <| Range 15 20 
                  , Grant Self 0 NPUp <| Range 15 20
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Saint's Invitation"
      , id      = 389
      , rarity  = 3
      , icon    = IconShieldUp
      , stats   = { base = { atk = 0, hp = 300 }
                  , max  = { atk = 0, hp = 1500 }
                  }
      , effect  = [ Grant Self 0 DefenseUp <| Range 3 5 
                  , Grant Self 0 DamageDown <| Range 100 200
                  ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Holy Night Supper"
      , id      = 390
      , rarity  = 5
      , icon    = IconNoble
      , stats   = { base = { atk = 500,  hp = 0 }
                  , max  = { atk = 2000, hp = 0 }
                  }
      , effect  = [ To Self GaugeUp <| Range 30 50 
                  , Grant Self 0 CritUp <| Range 10 15
                  , Grant Self 0 NPUp <| Range 10 15
                  ]
      , bond    = Nothing
      , limited = True
      }
    , bond 391 "Champion Cup" "Altria Pendragon (Archer)" IconSwordUp
      [ party AttackUp 15 ]
    , bond 392 "Phantasmal Summoning (Install)" "Illyasviel von Einzbern" IconAllUp
      [ party_ Buster 8, party_ Quick 8, party_ Arts 8 ]
    , bond 393 "Serpent of Fate" "Cleopatra" IconBeamUp
      [ party NPUp 25, demeritAll DefenseDown 10 ]
    , bond 394 "Holy Knuckle" "Martha (Ruler)" IconBusterUp
      [ party_ Buster 15 ]
    , bond 395 "Minimal Prudence" "Scathach (Assassin)" IconQuickUp
      [ party_ Quick 15 ]
    , bond 396 "Sharing of Pain" "Chloe von Einzbern" IconExclamationUp
      [ party CritUp 30, demeritAll HealthLoss 200 ]
    , bond 397 "Creed at the Bottom of the Earth" "Vlad III (EXTRA)" IconQuickBusterUp
      [ party_ Quick 10, party_ Buster 10 ]
    , bond 398 "Invitation to Halloween" "Elisabeth Bathory (Brave)" IconBusterUp
      [ party_ Buster 20, demeritAll DefenseDown 10 ]
    , { name    = "First Order"
      , id      = 399
      , rarity  = 4
      , icon    = IconRoad
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Bonus MysticCode False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    , { name    = "Devilish Bodhisattva"
      , id      = 400
      , rarity  = 5
      , icon    = IconSunUp
      , stats   = { base = { atk = 250,  hp = 400 }
                  , max  = { atk = 1000, hp = 1600 }
                  }
      , effect  = [ To Self GaugeUp <| Range 50 60 
                  , Times 1 << Grant Self 0 Overcharge <| Flat 2
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Room Guard"
      , id      = 401
      , rarity  = 4
      , icon    = IconStarUp
      , stats   = { base = { atk = 0, hp = 600 }
                  , max  = { atk = 0, hp = 2250 }
                  }
      , effect  = [ Grant Self 0 StarAbsorb <| Range 300 400
                  , Grant Self 0 DamageDown <| Range 300 400
                  ]
      , bond    = Nothing
      , limited = False
      }
    , { name    = "Seeker of Miracles"
      , id      = 402
      , rarity  = 3
      , icon    = IconBeamUp
      , stats   = { base = { atk = 200,  hp = 0 }
                  , max  = { atk = 1000, hp = 0 }
                  }
      , effect  = [ When "equipped by [Divine]" << Grant Self 0 NPUp <| Range 15 25 ]
      , bond    = Nothing
      , limited = False
      }
    , bond 403 "Seven-Headed Warhammer Sita" "Ishtar" IconBusterUp
      [party_ Buster 20, demeritAll DebuffVuln 20]
    , bond 406 "Door To The Ocean" "Jeanne d'Arc Alter Santa Lily" IconHealUp
      [party HealingReceived 30]
    , bond 410 "Primeval Flame" "Jaguar Warrior" IconBusterUp
      [party_ Buster 15]
    , bond 411 "The Furthest Tower" "Merlin" IconBusterUp
      [party_ Buster 10, party CritUp 15]
    , { name    = "Heroic Costume: Medusa"
      , id      = 670
      , rarity  = 4
      , icon    = IconRoad
      , stats   = { base = { atk = 100, hp = 100 }
                  , max  = { atk = 100, hp = 100 }
                  }
      , effect  = [ Bonus MysticCode False <| Flat 50 ]
      , bond    = Nothing
      , limited = True
      }
    ]

equipped : Class -> SkillEffect -> SkillEffect
equipped = When << (++) "equipped by a " << Show.class

{-| Retrieves the corresponding Bond CE. Memoized for performance. -}
getBond : Servant -> Maybe CraftEssence
getBond s = Dict.get s.name bondMap

{-| Memoization table for `getBond`. -}
bondMap : Dict String CraftEssence
bondMap =
  let
    go ce = case ce.bond of
        Nothing   -> Nothing
        Just bond -> Just (bond, ce)
  in
    craftEssences
    |> List.map go
    >> Maybe.values
    >> Dict.fromList

type alias CraftEssence =
    { name  : String
    , id  : Int
    , rarity  : Int
    , icon  : Icon
    , stats  : { base : Stat, max : Stat }
    , effect : List SkillEffect
    , bond : Maybe String
    , limited : Bool
    }
