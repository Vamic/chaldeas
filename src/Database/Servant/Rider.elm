module Database.Servant.Rider exposing (riders)

import Database.Base exposing (..)
import Database.Passive exposing (..)
import Database.Servant exposing (..)
import Database.Skill exposing (..)

riders : List Servant
riders = 
  [ { name  =   "Ozymandias"
    , id        = 118
    , rarity    = 5
    , class     = Rider
    , attr      = Heaven
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 5
    , stats     = { base  = { atk = 1850,  hp = 1881 }
                  , max   = { atk = 11971, hp = 12830 }
                  , grail = { atk = 13104, hp = 14056 }
                  }
    , skills    = [ { name   = "Charisma"
                    , rank   = B
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Party 3 AttackUp <| Range 9 18 ]
                    }
                  , { name   = "Imperial Privilege"
                    , rank   = A
                    , icon   = IconHeal
                    , cd     = 7
                    , effect = [ To Self Heal <| Range 1000 3000
                               , Chance 60 << Grant Self 3 AttackUp <| Range 20 40
                               , Chance 60 << Grant Self 3 DefenseUp <| Range 20 40
                               ]
                    }
                  , { name   = "Protection of the Sun God"
                    , rank   = A
                    , icon   = IconNoble
                    , cd     = 8
                    , effect = [ To Party GaugeUp <| Flat 20
                               , Grant Party 1 BuffUp <| Range 20 40
                               ]
                    }
                  ]
    , passives  = [magicResistance B, riding APlus, divinity B]
    , phantasm  = { name   = "Ramesseum Tentyris"
                  , desc   = "The Shining Great Temple Complex"
                  , rank   = EX
                  , card   = Buster
                  , kind   = "Anti-Fortress"
                  , hits   = 5
                  , effect = [ To Enemy Damage <| Range 600 1000
                             , Debuff Enemy 1 SealNP Full
                             ]
                  , over   = [ Debuff Enemy 3 DefenseDown <| Range 20 60 ]
                  , first  = False
                  }
    , gen       = { starWeight = 205, starRate = 9, npAtk = 0.59, npDef = 3 }
    , hits      = { quick = 5, arts = 3, buster = 1, ex = 5 }
    , traits    = [Male, Brynhild, Divine, Riding, EnumaElish, King]
    , death     = 30
    , align     = [Chaotic, Balanced]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 5)]
                  [(Piece Rider, 12), (FoolsChain, 22)]
                  [(Monument Rider, 5), (HeartOfTheForeignGod, 2), (SerpentJewel, 8)]
                  [(Monument Rider, 12), (HeartOfTheForeignGod, 4), (ScarabOfWisdom, 5)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 5)]
                  [(GemOf Rider, 12)]
                  [(MagicGemOf Rider, 5)]
                  [(MagicGemOf Rider, 12), (SerpentJewel, 4)]
                  [(SecretGemOf Rider, 5), (SerpentJewel, 8)]
                  [(SecretGemOf Rider, 12), (FoolsChain, 15)]
                  [(FoolsChain, 29), (ForbiddenPage, 6)]
                  [(ForbiddenPage, 18), (LampOfEvilSealing, 15)]
    }
  , { name      = "Francis Drake"
    , id        = 65
    , rarity    = 5
    , class     = Rider
    , attr      = Star
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 10
    , stats     = { base  = { atk = 1750,  hp = 1881 }
                  , max   = { atk = 11326, hp = 12830 }
                  , grail = { atk = 12398, hp = 14056 }
                  }
    , skills    = [ { name   = "Voyager of the Storm"
                    , rank   = APlus
                    , icon   = IconBeamUp
                    , cd     = 7
                    , effect = [ Grant Party 1 NPUp <| Range 8.5 17
                               , Grant Party 1 AttackUp <| Range 8.5 17
                               ]
                    }
                  , { name   = "Golden Rule"
                    , rank   = B
                    , icon   = IconNobleUp
                    , cd     = 8
                    , effect = [ Grant Self 3 NPGen <| Range 18 45 ]
                    }
                  , { name   = "Pioneer of the Stars"
                    , rank   = EX
                    , icon   = IconNoble
                    , cd     = 8
                    , effect = [ To Self GaugeUp <| Range 30 50
                               , Grant Self 3 IgnoreInvinc Full
                               , To Party GainStars <| Flat 10
                               ]
                    }
                  ]
    , passives  = [magicResistance D, riding D]
    , phantasm  = { name   = "Golden Wild Hunt"
                  , desc   = "Golden Stag and the Eventide Tempest"
                  , rank   = APlus
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 400 600 ]
                  , over   = [ To Party GainStars <| Range 20 40 ]
                  , first  = False
                  }
    , gen       = { starWeight = 208, starRate = 9, npAtk = 0.42, npDef = 3 }
    , hits      = { quick = 6, arts = 4, buster = 2, ex = 4 }
    , traits    = [Female, Riding]
    , death     = 50
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 5)]
                  [(Piece Rider, 12), (EvilBone, 22)]
                  [(Monument Rider, 5), (DragonsReverseScale, 2), (DragonFang, 24)]
                  [( Monument Rider, 12), (DragonsReverseScale, 4), (HeartOfTheForeignGod, 5)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 5)]
                  [(GemOf Rider, 12)]
                  [(MagicGemOf Rider, 5)]
                  [(MagicGemOf Rider, 12), (DragonFang, 12)]
                  [(SecretGemOf Rider, 5), (DragonFang, 24)]
                  [(SecretGemOf Rider, 12), (EvilBone, 15)]
                  [(GhostLantern, 6), (EvilBone, 29)]
                  [(GhostLantern, 18), (HeartOfTheForeignGod, 10)]
    }
  , { name      = "Iskandar"
    , id        = 108
    , rarity    = 5
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 10
    , stats     = { base  = { atk = 1786,  hp = 1938 }
                  , max   = { atk = 11560, hp = 13219 }
                  , grail = { atk = 12654, hp = 14482 }
                  }
    , skills    = [ { name   = "Charisma"
                    , rank   = A
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Party 3 AttackUp <| Range 10 20 ]
                    }
                  , { name   = "Tactics"
                    , rank   = B
                    , icon   = IconBeamUp
                    , cd     = 7
                    , effect = [ Grant Party 1 NPUp <| Range 9 18 ]
                    }
                  , { name   = "Lightning Conqueror"
                    , rank   = EX
                    , icon   = IconBusterUp
                    , cd     = 8
                    , effect = [ Grant Self 1 (Performance Buster) <| Range 30 50
                               , Grant Self 1 StarUp <| Flat 50
                               ]
                    }
                  ]
    , passives  = [magicResistance D, riding APlus, divinity C]
    , phantasm  = { name   = "Ionioi Hetairoi"
                  , desc   = "Army of the King"
                  , rank   = EX
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 8
                  , effect = [ To Enemies Damage <| Range 300 500 ]
                  , over   = [ Debuff Enemies 3 DefenseDown <| Range 10 30 ]
                  , first  = False
                  }
    , gen       = { starWeight = 205, starRate = 8.8, npAtk = 0.66, npDef = 3 }
    , hits      = { quick = 3, arts = 2, buster = 1, ex = 6 }
    , traits    = [Male, Brynhild, Divine, GreekMythMale, King, Riding, EnumaElish]
    , death     = 40
    , align     = [Neutral, Good]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 5)]
                  [(Piece Rider, 12), (MeteorHorseshoe, 8)]
                  [(Monument Rider, 5), (ProofOfHero, 29), (OctupletCrystals, 5)]
                  [(Monument Rider, 12), (OctupletCrystals, 10), (WarhorsesYoungHorn, 8)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 5)]
                  [(GemOf Rider, 12)]
                  [(MagicGemOf Rider, 5)]
                  [(MagicGemOf Rider, 12), (ProofOfHero, 15)]
                  [(SecretGemOf Rider, 5), (ProofOfHero, 29)]
                  [(SecretGemOf Rider, 12), (MeteorHorseshoe, 5)]
                  [(MeteorHorseshoe, 10), (SeedOfYggdrasil, 8)]
                  [(SeedOfYggdrasil, 22), (PhoenixFeather, 20)]
    }
  , { name      = "Queen Medb"
    , id        = 99
    , rarity    = 5
    , class     = Rider
    , attr      = Earth
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 5
    , stats     = { base  = { atk = 1591,  hp = 2048 }
                  , max   = { atk = 10296, hp = 13968 }
                  , grail = { atk = 11270, hp = 15303 }
                  }
    , skills    = [ { name   = "Golden Rule (Body)"
                    , rank   = A
                    , icon   = IconHealTurn
                    , cd     = 8
                    , effect = [ Grant Self 3 DebuffResist Full
                               , Grant Self 3 HealPerTurn <| Range 500 1000
                               , Grant Self 3 GaugePerTurn <| Flat 10
                               ]
                    }
                  , { name   = "Charisma"
                    , rank   = B
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Party 3 AttackUp <| Range 9 18 ]
                    }
                  , { name   = "Siren Song"
                    , rank   = C
                    , icon   = IconHeart
                    , cd     = 9
                    , effect = [ Chances 50 80 <| Debuff (EnemyType Male) 1 Charm Full ]
                    }
                  ]
    , passives  = [magicResistance B, riding A]
    , phantasm  = { name   = "Chariot My Love"
                  , desc   = "My Dear Iron Chariot"
                  , rank   = B
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 5
                  , effect = [ To Enemy Damage <| Range 600 1000 ]
                  , over   = [ To Enemy (DamageVs Male) <| Range 150 200
                             , Debuff Enemy 3 MentalVuln <| Range 10 50
                             ]
                  , first  = False
                  }
    , gen       = { starWeight = 208, starRate = 9, npAtk = 0.86, npDef = 3 }
    , hits      = { quick = 3, arts = 2, buster = 1, ex = 7 }
    , traits    = [Female, Riding, EnumaElish, King]
    , death     = 40
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 5)]
                  [(Piece Rider, 12), (SerpentJewel, 6)]
                  [(Monument Rider, 5), (PhoenixFeather, 8), (MeteorHorseshoe, 5)]
                  [(Monument Rider, 12), (MeteorHorseshoe, 10), (WarhorsesYoungHorn, 8)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 5)]
                  [(GemOf Rider, 12)]
                  [(MagicGemOf Rider, 5)]
                  [(MagicGemOf Rider, 12), (PhoenixFeather, 4)]
                  [(SecretGemOf Rider, 5), (PhoenixFeather, 8)]
                  [(SecretGemOf Rider, 12), (SerpentJewel, 4)]
                  [(SerpentJewel, 8), (GhostLantern, 6)]
                  [(GhostLantern, 18), (HeartOfTheForeignGod, 10)]
    }
  , { name      = "Sakata Kintoki (Rider)"
    , id        = 115
    , rarity    = 4
    , class     = Rider
    , attr      = Earth
    , deck      = Deck Quick Quick Arts Buster Buster
    , curve     = 9
    , stats     = { base  = { atk = 1636,  hp = 1728 }
                  , max   = { atk = 9819,  hp = 10800 }
                  , grail = { atk = 11889, hp = 13095 }
                  }
    , skills    = [ { name   = "Long-Distance Dash"
                    , rank   = A
                    , icon   = IconQuickUp
                    , cd     = 7
                    , effect = [ Grant Self 3 (Performance Quick) <| Range 20 30
                               , Grant Self 3 StarUp <| Range 30 50
                               ]
                    }
                  , { name   = "Animal Communication"
                    , rank   = C
                    , icon   = IconNoble
                    , cd     = 8
                    , effect = [ To Self GaugeUp <| Range 30 50 ]
                    }
                  , { name   = "Natural Body"
                    , rank   = A
                    , icon   = IconHoodUp
                    , cd     = 7
                    , effect = [ Grant Self 3 OffensiveResist <| Range 60 120
                               , To Self Heal <| Range 1000 3000
                               ]
                    }
                  ]
    , passives  = [divinity C]
    , phantasm  = { name   = "Golden Drive, Good Night"
                  , desc   = ""
                  , rank   = B
                  , card   = Quick
                  , kind   = "Anti-Army"
                  , hits   = 4
                  , effect = [ To Enemy Damage <| Range 1200 2000 ]
                  , over   = [ Grant Self 1 (Performance Quick) <| Range 10 90 ]
                  , first  = False
                  }
    , gen       = { starWeight = 198, starRate = 9, npAtk = 1.15, npDef = 3 }
    , hits      = { quick = 4, arts = 2, buster = 1, ex = 5 }
    , traits    = [Male, Brynhild, Divine, EnumaElish]
    , death     = 40
    , align     = [Lawful, Good]
    , limited   = True
    , free      = True
    , ascendUp  = Welfare "Golden Bear Lighter"
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 10)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 10), (OctupletCrystals, 4)]
                  [(SecretGemOf Rider, 4), (OctupletCrystals, 8)]
                  [(SecretGemOf Rider, 10), (SeedOfYggdrasil, 5)]
                  [(SeedOfYggdrasil, 10), (MeteorHorseshoe, 5)]
                  [(MeteorHorseshoe, 15), (WarhorsesYoungHorn, 12)]
    }
  , { name      = "Altria Pendragon (Santa Alter)"
    , id        = 73
    , rarity    = 4
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 29
    , stats     = { base  = { atk = 1543,  hp = 1805 }
                  , max   = { atk = 9258,  hp = 11286 }
                  , grail = { atk = 11209, hp = 13684 }
                  }
    , skills    = [ { name   = "Saint's Gift"
                    , rank   = EX
                    , icon   = IconHeal
                    , cd     = 7
                    , effect = [ To Ally Heal <| Range 1500 3500
                               , Grant Ally 3 StarUp <| Flat 30
                               ]
                    }
                  , { name   = "Intuition"
                    , rank   = A
                    , icon   = IconStar
                    , cd     = 7
                    , effect = [ To Party GainStars <| Range 5 15 ]
                    }
                  , { name   = "Mana Burst"
                    , rank   = AMinus
                    , icon   = IconBusterUp
                    , cd     = 7
                    , effect = [ Grant Self 1 (Performance Buster) <| Range 25 45 ]
                    }
                  ]
    , passives  = [magicResistance A, riding A]
    , phantasm  = { name   = "Excalibur Morgan"
                  , desc   = "Sword of Promised Victory"
                  , rank   = APlusPlus
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 3
                  , effect = [ To Enemies Damage <| Range 450 650 ]
                  , over   = [ To Self GaugeUp <| Range 10 30 ]
                  , first  = False
                  }
    , gen       = { starWeight = 204, starRate = 8.9, npAtk = 0.87, npDef = 3 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Female, Arthur, Dragon, Riding, Saberface, EnumaElish, King]
    , death     = 35
    , align     = [Lawful, Good]
    , limited   = True
    , free      = True
    , ascendUp  = Welfare "Bucket of Chicken"
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 10)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 10), (DragonFang, 10)]
                  [(SecretGemOf Rider, 4), (DragonFang, 20)]
                  [(SecretGemOf Rider, 10), (MeteorHorseshoe, 4)]
                  [(MeteorHorseshoe, 8), (DragonsReverseScale, 2)]
                  [(DragonsReverseScale, 6), (HeartOfTheForeignGod, 8)]
    }
  , { name      = "Martha"
    , id        = 30
    , rarity    = 4
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Arts Arts Arts Buster
    , curve     = 4
    , stats     = { base  = { atk = 1335,  hp = 2090 }
                  , max   = { atk = 8014, hp = 13068 }
                  , grail = { atk = 9703, hp = 15845 }
                  }
    , skills    = [ { name   = "Protection of the Faith"
                    , rank   = A
                    , icon   = IconHoodUp
                    , cd     = 7
                    , effect = [ Grant Self 3 DebuffResist <| Range 50 100
                               , To Self Heal <| Range 1000 2500
                               ]
                    }
                  , { name   = "Miracle"
                    , rank   = DPlus
                    , icon   = IconHeal
                    , cd     = 8
                    , effect = [ To Party Heal <| Range 1000 2000
                               , To Party RemoveDebuffs Full
                               ]
                    }
                  , { name   = "Oath of the Holy Maiden"
                    , rank   = C
                    , icon   = IconShieldDown
                    , cd     = 7
                    , effect = [ Debuff Enemy 3 DefenseDown <| Range 10 30
                               , To Enemy RemoveBuffs Full
                               ]
                    }
                  ]
    , passives  = [magicResistance A, riding APlusPlus, divinity C]
    , phantasm  = { name   = "Tarasque"
                  , desc   = "O Tragic Drake Who Knew Naught of Love"
                  , rank   = APlus
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 1
                  , effect = [ To Enemies Damage <| Range 300 500 ]
                  , over   = [ Debuff Enemies 3 DefenseDown <| Range 20 40 ]
                  , first  = False
                  }
    , gen       = { starWeight = 205, starRate = 9, npAtk = 1.58, npDef = 3 }
    , hits      = { quick = 2, arts = 1, buster = 1, ex = 3 }
    , traits    = [Female, Riding, Divine, EnumaElish]
    , death     = 30
    , align     = [Lawful, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 4)]
                  [(Piece Rider, 10), (DragonFang, 15)]
                  [(Monument Rider, 4), (ClawOfChaos, 3), (SeedOfYggdrasil, 10)]
                  [(Monument Rider, 10), (ClawOfChaos, 5), (DragonsReverseScale, 4)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 10)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 10), (SeedOfYggdrasil, 5)]
                  [(SecretGemOf Rider, 4), (SeedOfYggdrasil, 10)]
                  [(SecretGemOf Rider, 10), (DragonFang, 10)]
                  [(PhoenixFeather, 4), (DragonFang, 20)]
                  [(PhoenixFeather, 12), (DragonsReverseScale, 8)]
    }
  , { name      = "Mordred (Rider)"
    , id        = 132
    , rarity    = 4
    , class     = Rider
    , attr      = Earth
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 24
    , stats     = { base  = { atk = 1535,  hp = 1824 }
                  , max   = { atk = 9212,  hp = 11400 }
                  , grail = { atk = 11154, hp = 13822 }
                  }
    , skills    = [ { name   = "Cerulean Ride"
                    , rank   = A
                    , icon   = IconArtsUp
                    , cd     = 7
                    , effect = [ Grant Self 3 (Performance Arts) <| Range 20 30 ]
                    }
                  , { name   = "Rodeo Flip"
                    , rank   = APlus
                    , icon   = IconDodge
                    , cd     = 9
                    , effect = [ Grant Self 1 Evasion Full
                               , Grant Self 1 StarUp <| Range 30 50
                               ]
                    }
                  , { name   = "Endless Summer"
                    , rank   = B
                    , icon   = IconKneel
                    , cd     = 9
                    , effect = [ Times 1 << Grant Self 3 Guts <| Flat 1000
                               , To Self GaugeUp <| Range 20 30
                               ]
                    }
                  ]
    , passives  = [magicResistance B, surfing A]
    , phantasm  = { name   = "Prydwen Tube Riding"
                  , desc   = ""
                  , rank   = A
                  , card   = Arts
                  , kind   = "Anti-Wave"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 450 750 ]
                  , over   = [ Chances 50 90 << To Enemies GaugeDown <| Flat 1 ]
                  , first  = False
                  }
    , gen       = { starWeight = 204, starRate = 9.2, npAtk = 0.71, npDef = 3 }
    , hits      = { quick = 3, arts = 2, buster = 1, ex = 4 }
    , traits    = [Female, Dragon, Saberface, EnumaElish]
    , death     = 35
    , align     = [Chaotic, Good]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 4)]
                  [(Piece Rider, 10), (ShellOfReminiscence, 6)]
                  [(Monument Rider, 4), (GreatKnightMedal, 16), (DragonFang, 10)]
                  [(Monument Rider, 10), (DragonFang, 20), (DragonsReverseScale, 4)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 10)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 10), (GreatKnightMedal, 8)]
                  [(SecretGemOf Rider, 4), (GreatKnightMedal, 16)]
                  [(SecretGemOf Rider, 10), (ShellOfReminiscence, 4)]
                  [(ShellOfReminiscence, 8), (HomunculusBaby, 5)]
                  [(HomunculusBaby, 15), (TearstoneOfBlood, 12)]
    }
  , { name      = "Marie Antoinette"
    , id        = 29
    , rarity    = 4
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 9
    , stats     = { base  = { atk = 1382,  hp = 1975 }
                  , max   = { atk = 8293, hp = 12348 }
                  , grail = { atk = 10041, hp = 14972 }
                  }
    , skills    = [ { name   = "Siren Song"
                    , rank   = C
                    , icon   = IconHeart
                    , cd     = 9
                    , effect = [ Chances 50 80 <| Debuff (EnemyType Male) 1 Charm Full ]
                    }
                  , { name   = "Beautiful Princess"
                    , rank   = A
                    , icon   = IconShield
                    , cd     = 8
                    , effect = [ Times 3 <| Grant Self 0 Invincibility Full
                               , Grant Self 5 HealPerTurn <| Range 200 600
                               ]
                    }
                  , { name   = "God's Grace"
                    , rank   = B
                    , icon   = IconStaffUp
                    , cd     = 7
                    , effect = [ Grant Self 3 MentalSuccess <| Range 10 30
                               , To Self Heal <| Range 1000 2500
                               ]
                    }
                  ]
    , passives  = [magicResistance C, riding APlus]
    , phantasm  = { name   = "Guillotine Breaker"
                  , desc   = "Glory to the Crown of Lilies"
                  , rank   = A
                  , card   = Quick
                  , kind   = "Anti-Army"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 600 1000
                             , To Party RemoveDebuffs Full
                             ]
                  , over   = [ To Party Heal <| Range 500 2500 ]
                  , first  = False
                  }
    , gen       = { starWeight = 201, starRate = 9, npAtk = 1, npDef = 3 }
    , hits      = { quick = 2, arts = 1, buster = 1, ex = 3 }
    , traits    = [Female, Riding, EnumaElish]
    , death     = 35
    , align     = [Lawful, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 4)]
                  [(Piece Rider, 10), (PhoenixFeather, 5)]
                  [(Monument Rider, 4), (MeteorHorseshoe, 4), (SerpentJewel, 7)]
                  [(Monument Rider, 10), (MeteorHorseshoe, 8), (EternalGear, 10)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 10)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 10), (SerpentJewel, 4)]
                  [(SecretGemOf Rider, 4), (SerpentJewel, 7)]
                  [(SecretGemOf Rider, 10), (PhoenixFeather, 4)]
                  [(PhoenixFeather, 7), (HeartOfTheForeignGod, 2)]
                  [(EternalGear, 20), (HeartOfTheForeignGod, 6)]
    }
  , { name      = "Anne Bonny & Mary Read"
    , id        = 66
    , rarity    = 4
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 29
    , stats     = { base  = { atk = 1504,  hp = 1805 }
                  , max   = { atk = 9029,  hp = 11286 }
                  , grail = { atk = 10932, hp = 13684 }
                  }
    , skills    = [ { name   = "Voyage"
                    , rank   = A
                    , icon   = IconStarHaloUp
                    , cd     = 8
                    , effect = [ Grant Self 3 StarUp <| Range 30 50 ]
                    }
                  , { name   = "Marksmanship"
                    , rank   = B
                    , icon   = IconExclamationUp
                    , cd     = 7
                    , effect = [ Grant Self 1 CritUp <| Range 50 100 ]
                    }
                  , { name   = "Combination"
                    , rank   = C
                    , icon   = IconStarUp
                    , cd     = 7
                    , effect = [ Grant Self 1 StarAbsorb <| Range 300 600
                               , Grant Self 1 AttackUp <| Range 20 30
                               ]
                    }
                  ]
    , passives  = [magicResistance D]
    , phantasm  = { name   = "Caribbean Free Bird"
                  , desc   = "Bond of Lovebirds"
                  , rank   = CPlusPlus
                  , card   = Quick
                  , kind   = "Anti-Personnel"
                  , hits   = 6
                  , effect = [ To Enemy Damage <| Range 1600 2400 ]
                  , over   = [ To Enemy LastStand <| Range 1200 2000 ]
                  , first  = False
                  }
    , gen       = { starWeight = 200, starRate = 9.1, npAtk = 0.84, npDef = 3 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 1 }
    , traits    = [Female, EnumaElish]
    , death     = 50
    , align     = [Chaotic, Evil, Chaotic, Balanced]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 4)]
                  [(Piece Rider, 10), (HomunculusBaby, 6)]
                  [(Monument Rider, 4), (GhostLantern, 8), (EvilBone, 12)]
                  [(Monument Rider, 10), (ClawOfChaos, 6), (EvilBone, 24)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 10)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 10), (GhostLantern, 4)]
                  [(SecretGemOf Rider, 4), (GhostLantern, 8)]
                  [(SecretGemOf Rider, 10), (HomunculusBaby, 4)]
                  [(VoidsDust, 10), (HomunculusBaby, 8)]
                  [(VoidsDust, 30), (ClawOfChaos, 12)]
    }
  , { name      = "Astolfo"
    , id        = 94
    , rarity    = 4
    , class     = Rider
    , attr      = Earth
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 24
    , stats     = { base  = { atk = 1489,  hp = 1787 }
                  , max   = { atk = 8937,  hp = 11172 }
                  , grail = { atk = 10821, hp = 13546 }
                  }
    , skills    = [ { name   = "Monstrous Strength"
                    , rank   = CMinus
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Self 1 AttackUp <| Range 8 28 ]
                    }
                  , { name   = "Trap of Argalia"
                    , rank   = D
                    , icon   = IconStun
                    , cd     = 10
                    , effect = [ Chances 60 90 <| Debuff Enemy 1 Stun Full ]
                    }
                  , { name   = "Evaporation of Reason"
                    , rank   = D
                    , icon   = IconStarTurn
                    , cd     = 10
                    , effect = [ Chance 65 << Grant Party 3 StarsPerTurn <| Range 5 10
                               , Chance 65 << Grant Self 3 StarUp <| Range 30 50
                               , Chance 65 << Grant Self 3 CritUp <| Range 20 40
                               ]
                    }
                  ]
    , passives  = [magicResistance A, riding APlus, independentAction B]
    , phantasm  = { name   = "Hippogriff"
                  , desc   = "Otherworldly Phantom Horse"
                  , rank   = BPlus
                  , card   = Quick
                  , kind   = "Anti-Army"
                  , hits   = 1
                  , effect = [ To Enemies DamageThruDef <| Range 600 1000
                             , Times 3 <| Grant Self 0 Evasion Full
                             ]
                  , over   = [ To Party GainStars <| Range 5 25 ]
                  , first  = False
                  }
    , gen       = { starWeight = 205, starRate = 9, npAtk = 0.66, npDef = 1 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 4 }
    , traits    = [Nonbinary, Riding, EnumaElish]
    , death     = 40
    , align     = [Chaotic, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 4)]
                  [(Piece Rider, 10), (MeteorHorseshoe, 6)]
                  [(Monument Rider, 4), (OctupletCrystals, 8), (PhoenixFeather, 4)]
                  [(Monument Rider, 10), (ClawOfChaos, 6), (PhoenixFeather, 7)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 10)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 10), (OctupletCrystals, 4)]
                  [(SecretGemOf Rider, 4), (OctupletCrystals, 8)]
                  [(SecretGemOf Rider, 10), (MeteorHorseshoe, 4)]
                  [(ForbiddenPage, 5), (MeteorHorseshoe, 8)]
                  [(ForbiddenPage, 15), (SeedOfYggdrasil, 24)]
    }
  , { name      = "Georgios"
    , id        = 24
    , rarity    = 2
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 2
    , stats     = { base  = { atk = 929,  hp = 1731 }
                  , max   = { atk = 5236, hp = 9200 }
                  , grail = { atk = 7587, hp = 13278 }
                  }
    , skills    = [ { name   = "Guardian Knight"
                    , rank   = APlus
                    , icon   = IconCrosshairUp
                    , cd     = 7
                    , effect = [ Grant Self 3 Taunt Full
                               , Grant Self 3 DefenseUp <| Range 20 40
                               ]
                    }
                  , { name   = "Martyr's Soul"
                    , rank   = BPlus
                    , icon   = IconHoodUp
                    , cd     = 7
                    , effect = [ Grant Self 3 MentalResist <| Range 50 100
                               , To Self Heal <| Range 750 2000
                               ]
                    }
                  , { name   = "Battle Continuation"
                    , rank   = A
                    , icon   = IconKneel
                    , cd     = 9
                    , effect = [ Times 1 << Grant Self 5 Guts <| Range 1000 2500 ]
                    }
                  ]
    , passives  = [magicResistance A, riding B]
    , phantasm  = { name   = "Ascalon"
                  , desc   = "Blessed Sword of Force Subversion"
                  , rank   = B
                  , card   = Arts
                  , kind   = "Anti-Personnel"
                  , hits   = 4
                  , effect = [ Debuff Enemy 3 (ApplyTrait Dragon) Full
                             , To Enemy Damage <| Range 900 1500
                             ]
                  , over   = [ Grant Self 1 DefenseUp <| Range 20 40 ]
                  , first  = False
             }
    , gen       = { starWeight = 205, starRate = 8.9, npAtk = 0.85, npDef = 3 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Male, Riding, Brynhild, EnumaElish]
    , death     = 45
    , align     = [Lawful, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 3)]
                  [(Piece Rider, 6), (ProofOfHero, 11)]
                  [(Monument Rider, 3), (PhoenixFeather, 4), (ForbiddenPage, 3)]
                  [(Monument Rider, 6), (ForbiddenPage, 5), (MeteorHorseshoe, 6)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 3)]
                  [(GemOf Rider, 6)]
                  [(MagicGemOf Rider, 3)]
                  [(MagicGemOf Rider, 6), (PhoenixFeather, 2)]
                  [(SecretGemOf Rider, 3), (PhoenixFeather, 4)]
                  [(SecretGemOf Rider, 6), (ProofOfHero, 8)]
                  [(ProofOfHero, 15), (EternalGear, 3)]
                  [(EternalGear, 9), (MeteorHorseshoe, 12)]

    }
  , { name      = "Ushiwakamaru"
    , id        = 27
    , rarity    = 3
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 8
    , stats     = { base  = { atk = 1314, hp = 1625 }
                  , max   = { atk = 7076, hp = 9028 }
                  , grail = { atk = 9576, hp = 12240 }
                  }
    , skills    = [ { name   = "Tengu's Strategy"
                    , rank   = A
                    , icon   = IconNobleUp
                    , cd     = 7
                    , effect = [ Grant Party 3 NPGen <| Range 10 20 ]
                    }
                  , { name   = "Charisma"
                    , rank   = CPlus
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Party 3 AttackUp <| Range 8.5 17 ]
                    }
                  , { name   = "Art of the Swallow"
                    , rank   = B
                    , icon   = IconDodge
                    , cd     = 8
                    , effect = [ Times 1 <| Grant Self 1 Evasion Full
                               , Grant Self 1 StarUp <| Range 50 100
                               ]
                    }
                  ]
    , passives  = [magicResistance C, riding APlus]
    , phantasm  = { name   = "Dan-No-Ura Eight-Boat Leap"
                  , desc   = "Dan No Ura—Hassoutobi"
                  , rank   = C
                  , card   = Quick
                  , kind   = "Anti-Personnel"
                  , hits   = 1
                  , effect = [ To Enemy Damage <| Range 1600 2400 ]
                  , over   = [ Grant Self 3 StarUp <| Range 50 90 ]
                  , first  = False
                  }
    , gen       = { starWeight = 204, starRate = 9.1, npAtk = 0.87, npDef = 3 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Female, Riding, Brynhild, EnumaElish]
    , death     = 35
    , align     = [Chaotic, Balanced]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 4)]
                  [(Piece Rider, 8), (ProofOfHero, 15)]
                  [(Monument Rider, 4), (MeteorHorseshoe, 7), (GhostLantern, 4)]
                  [(Monument Rider, 8), (GhostLantern, 7), (OctupletCrystals, 8)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 8)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 8), (MeteorHorseshoe, 4)]
                  [(SecretGemOf Rider, 4), (MeteorHorseshoe, 7)]
                  [(SecretGemOf Rider, 8), (ProofOfHero, 10)]
                  [(ProofOfHero, 20), (EternalGear, 4)]
                  [(EternalGear, 12), (OctupletCrystals, 16)]
    }
  , { name      = "Medusa"
    , id        = 23
    , rarity    = 3
    , class     = Rider
    , attr      = Earth
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 24
    , stats     = { base  = { atk = 1337, hp = 1608 }
                  , max   = { atk = 7200, hp = 8937 }
                  , grail = { atk = 9744, hp = 12117 }
                  }
    , skills    = [ { name   = "Mystic Eyes"
                    , rank   = APlus
                    , icon   = IconStun
                    , cd     = 8
                    , effect = [ Chances 50 100 <| Debuff Enemy 1 Stun Full ]
                    }
                  , { name   = "Monstrous Strength"
                    , rank   = B
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Self 2 AttackUp <| Range 10 30 ]
                    }
                  , { name   = "Blood Fort Andromeda"
                    , rank   = B
                    , icon   = IconNoble
                    , cd     = 8
                    , effect = [ To Self GaugeUp <| Flat 20
                               , Grant Self 3 NPGen <| Range 20 30
                               ]
                    }
                  ]
    , passives  = [ magicResistance B, independentAction C, divinity EMinus
                  , riding APlus
                  ]
    , phantasm  = { name   = "Bellerophon"
                  , desc   = "Bridle of Chivalry"
                  , rank   = APlus
                  , card   = Quick
                  , kind   = "Anti-Army"
                  , hits   = 1
                  , effect = [ To Enemies Damage <| Range 600 1000 ]
                  , over   = [ Grant Party 3 StarUp <| Range 20 60 ]
                  , first  = False
                  }
    , gen       = { starWeight = 194, starRate = 9, npAtk = 0.58, npDef = 3 }
    , hits      = { quick = 2, arts = 3, buster = 1, ex = 3 }
    , traits    = [Female, Riding, Divine]
    , death     = 35
    , align     = [Chaotic, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 4)]
                  [(Piece Rider, 8), (SerpentJewel, 4)]
                  [(Monument Rider, 4), (VoidsDust, 13), (MeteorHorseshoe, 4)]
                  [(Monument Rider, 8), (MeteorHorseshoe, 7), (PhoenixFeather, 7)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 8)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 8), (VoidsDust, 7)]
                  [(SecretGemOf Rider, 4), (VoidsDust, 13)]
                  [(SecretGemOf Rider, 8), (SerpentJewel, 3)]
                  [(SerpentJewel, 6), (HeartOfTheForeignGod, 2)]
                  [(HeartOfTheForeignGod, 5), (PhoenixFeather, 13)]
    }
  , { name      = "Edward Teach"
    , id        = 25
    , rarity    = 2
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 12
    , stats     = { base  = { atk = 1097,  hp = 1488 }
                  , max   = { atk = 6188, hp = 7907 }
                  , grail = { atk = 8967, hp = 11411 }
                  }
    , skills    = [ { name   = "Voyager of the Storm"
                    , rank   = A
                    , icon   = IconBeamUp
                    , cd     = 7
                    , effect = [ Grant Party 1 NPUp <| Range 8 16
                               , Grant Party 1 AttackUp <| Range 8 16
                               ]
                    }
                  , { name   = "Pirate's Glory"
                    , rank   = B
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Self 3 AttackUp <| Range 9 27
                               , Times 1 << Grant Self 0 Guts <| Flat 1
                               , Debuff Self 3 DebuffVuln <| Flat 50
                               ]
                    }
                  , { name   = "Gentlemanly Love"
                    , rank   = C
                    , icon   = IconHeal
                    , cd     = 8
                    , effect = [ To Party Heal <| Range 1000 2000
                               , To (AlliesType Female) Heal <| Range 1000 2000
                               , To (AlliesType Nonbinary) Heal <| Range 1000 2000
                               ]
                    }
                  ]
    , passives  = [magicResistance E]
    , phantasm  = { name   = "Queen Anne's Revenge"
                  , desc   = ""
                  , rank   = CPlusPlus
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 300 500
                             , To Party GainStars <| Flat 5
                             ]
                  , over   = [ Chances 30 70 << To Enemies GaugeDown <| Flat 1 ]
                  , first  = False
                  }
    , gen       = { starWeight = 198, starRate = 8.8, npAtk = 0.56, npDef = 3 }
    , hits      = { quick = 2, arts = 3, buster = 1, ex = 3 }
    , traits    = [Male, EnumaElish]
    , death     = 45
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 3)]
                  [(Piece Rider, 6), (EvilBone, 11)]
                  [(Monument Rider, 3), (OctupletCrystals, 5), (ForbiddenPage, 3)]
                  [(Monument Rider, 6), (ForbiddenPage, 5), (GhostLantern, 6)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 3)]
                  [(GemOf Rider, 6)]
                  [(MagicGemOf Rider, 3)]
                  [(MagicGemOf Rider, 6), (OctupletCrystals, 3)]
                  [(SecretGemOf Rider, 3), (OctupletCrystals, 5)]
                  [(SecretGemOf Rider, 6), (EvilBone, 8)]
                  [(EvilBone, 15), (HomunculusBaby, 3)]
                  [(HomunculusBaby, 9), (GhostLantern, 15)]
    }
  , { name      = "Alexander"
    , id        = 28
    , rarity    = 3
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 3
    , stats     = { base  = { atk = 1366, hp = 1979 }
                  , max   = { atk = 7356, hp = 8640 }
                  , grail = { atk = 9955, hp = 11714 }
                  }
    , skills    = [ { name   = "Charisma"
                    , rank   = C
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Party 3 AttackUp <| Range 8 16 ]
                    }
                  , { name   = "Fair Youth"
                    , rank   = B
                    , icon   = IconHeart
                    , cd     = 9
                    , effect = [ Chances 50 80 <| Debuff (EnemyType Human) 1 Charm Full ]
                    }
                  , { name   = "Omen of the Conqueror"
                    , rank   = A
                    , icon   = IconQuickUp
                    , cd     = 7
                    , effect = [ Grant Party 3 (Performance Quick) <| Range 10 20 ]
                    }
                  ]
    , passives  = [magicResistance D, divinity E, riding APlus]
    , phantasm  = { name   = "Bucephalus"
                  , desc   = "The Beginning of Trampling Conquest"
                  , rank   = BPlusPlus
                  , card   = Quick
                  , kind   = "Anti-Army"
                  , hits   = 1
                  , effect = [ To Enemies Damage <| Range 800 1200 ]
                  , over   = [ To Party GainStars <| Range 15 35 ]
                  , first  = False
                  }
    , gen       = { starWeight = 205, starRate = 9, npAtk = 0.86, npDef = 3 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Male, Riding, Divine, EnumaElish, GreekMythMale, King]
    , death     = 40
    , align     = [Neutral, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Rider, 4)]
                  [(Piece Rider, 8), (MeteorHorseshoe, 5)]
                  [(Monument Rider, 4), (PhoenixFeather, 3), (OctupletCrystals, 7)]
                  [(Monument Rider, 8), (PhoenixFeather, 6), (ProofOfHero, 24)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 8)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 8), (OctupletCrystals, 4)]
                  [(SecretGemOf Rider, 4), (OctupletCrystals, 7)]
                  [(SecretGemOf Rider, 8), (MeteorHorseshoe, 4)]
                  [(ClawOfChaos, 3), (MeteorHorseshoe, 7)]
                  [(ClawOfChaos, 8), (ProofOfHero, 48)]
    }
  , { name      = "Boudica"
    , id        = 26
    , rarity    = 3
    , class     = Rider
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 3
    , stats     = { base  = { atk = 1168,  hp = 1823 }
                  , max   = { atk = 6289, hp = 10130 }
                  , grail = { atk = 8511, hp = 13735 }
                  }
    , skills    = [ { name   = "Vow to the Goddess"
                    , rank   = B
                    , icon   = IconDamageUp
                    , cd     = 7
                    , effect = [ Grant Self 3 (AttackVs Roman) <| Range 40 60 ]
                    }
                  , { name   = "Battle Continuation"
                    , rank   = A
                    , icon   = IconKneel
                    , cd     = 9
                    , effect = [ Times 1 << Grant Self 5 Guts <| Range 1000 2500 ]
                    }
                  , { name   = "Andraste's Protection"
                    , rank   = A
                    , icon   = IconArtsUp
                    , cd     = 7
                    , effect = [ Grant Party 3 (Performance Arts) <| Range 10 20 ]
                    }
                  ]
    , passives  = [magicResistance D, riding A]
    , phantasm  = { name   = "Chariot of Boudica"
                  , desc   = "Chariot of Promised Protection"
                  , rank   = BPlus
                  , card   = Arts
                  , kind   = "Anti-Army"
                  , hits   = 0
                  , effect = [ Grant Party 3 DefenseUp <| Range 10 20 ]
                  , over   = [ Grant Party 1 DefenseUp <| Range 20 40 ]
                  , first  = False
                  }
    , gen       = { starWeight = 196, starRate = 8.9, npAtk = 0.85, npDef = 3 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Female, Riding, EnumaElish, King]
    , death     = 45
    , align     = [Neutral, Good]
    , limited   = False
    , free      = True
    , ascendUp  = Ascension
                  [(Piece Rider, 4)]
                  [(Piece Rider, 8), (PhoenixFeather, 4)]
                  [(Monument Rider, 4), (EvilBone, 20), (MeteorHorseshoe, 4)]
                  [(Monument Rider, 8), (MeteorHorseshoe, 7), (VoidsDust, 16)]
    , skillUp   = Reinforcement
                  [(GemOf Rider, 4)]
                  [(GemOf Rider, 8)]
                  [(MagicGemOf Rider, 4)]
                  [(MagicGemOf Rider, 8), (EvilBone, 10)]
                  [(SecretGemOf Rider, 4), (EvilBone, 20)]
                  [(SecretGemOf Rider, 8), (PhoenixFeather, 3)]
                  [(PhoenixFeather, 6), (SerpentJewel, 4)]
                  [(SerpentJewel, 10), (VoidsDust, 32)]
    }
  ]
