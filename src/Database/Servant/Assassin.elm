module Database.Servant.Assassin exposing (assassins)

import Database.Base exposing (..)
import Database.Passive exposing (..)
import Database.Servant exposing (..)
import Database.Skill exposing (..)

assassins : List Servant
assassins = 
  [ { name      = "Jack the Ripper"
    , id        = 75
    , rarity    = 5
    , class     = Assassin
    , attr      = Earth
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 25
    , stats     = { base  = { atk = 1786,  hp = 1862 }
                  , max   = { atk = 11557, hp = 12696 }
                  , grail = { atk = 12651, hp = 13909 }
                  }
    , skills    = [ { name   = "Murder on a Misty Night"
                    , rank   = A
                    , icon   = IconDodge
                    , cd     = 8
                    , effect = [ Grant Self 1 Evasion Full
                               , Grant Self 1 (Performance Quick) <| Range 30 50
                               ]
                    }
                  , { name   = "Information Erasure"
                    , rank   = B
                    , icon   = IconCircuits
                    , cd     = 7
                    , effect = [ To Enemy RemoveBuffs Full
                               , Debuff Enemy 3 CritChance <| Range 10 30
                               ]
                    }
                  , { name   = "Surgery"
                    , rank   = E
                    , icon   = IconHeal
                    , cd     = 6
                    , effect = [ To Ally Heal <| Range 500 2500 ]
                    }
                  ]
    , passives  = [presenceConcealment APlus]
    , phantasm  = { name   = "Maria the Ripper"
                  , desc   = "Holy Mother of Dismemberment"
                  , rank   = DPlus
                  , card   = Quick
                  , kind   = "Anti-Personnel"
                  , hits   = 4
                  , effect = [ To Enemy DamageThruDef <| Range 1400 2200 ]
                  , over   = [ Grant Self 1 (Special AttackUp <| VsTrait Female) <| Range 50 100 ]
                  , first  = True
                  }
    , gen       = { starWeight = 97, starRate = 25.5, npAtk = 1.07, npDef = 4 }
    , hits      = { quick = 5, arts = 2, buster = 2, ex = 4 }
    , gender    = Female
    , traits    = [EnumaElish]
    , death     = 44
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 5)]
                  [(Piece Assassin, 12), (EvilBone, 22)]
                  [(Monument Assassin, 5), (EternalGear, 5), (VoidsDust, 20)]
                  [(Monument Assassin, 12), (ClawOfChaos, 8), (EternalGear, 10)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 5)]
                  [(GemOf Assassin, 12)]
                  [(MagicGemOf Assassin, 5)]
                  [(MagicGemOf Assassin, 12), (VoidsDust, 10)]
                  [(SecretGemOf Assassin, 5), (VoidsDust, 20)]
                  [(SecretGemOf Assassin, 12), (EvilBone, 15)]
                  [(HeartOfTheForeignGod, 3), (EvilBone, 29)]
                  [(HeartOfTheForeignGod, 8), (ClawOfChaos, 15)]
    }
  , { name      = "Cleopatra"
    , id        = 139
    , rarity    = 5
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Buster Buster
    , curve     = 5
    , stats     = { base  = { atk = 1713,  hp = 1965 } 
                  , max   = { atk = 11088, hp = 13402 }
                  , grail = { atk = 12138, hp = 14682 }
                  }
    , skills    = [ { name   = "Imperial Privilege"
                    , rank   = A
                    , icon   = IconHeal
                    , cd     = 7
                    , effect = [ To Self Heal <| Range 1000 3000
                               , Chance 60 << Grant Self 3 AttackUp  <| Range 20 40 
                               , Chance 60 << Grant Self 3 DefenseUp <| Range 20 40
                               ]
                    } 
                  , { name   = "Golden Rule (Wealth & Body)"
                    , rank   = B
                    , icon   = IconNobleTurn
                    , cd     = 8
                    , effect = [ Grant Self 3 NPGen <| Range 20 40 
                               , Grant Self 3 GaugePerTurn <| Flat 10
                               , Grant Self 3 HealPerTurn <| Range 500 100
                               ]
                    }
                  , { name   = "Protection of the Goddess"
                    , rank   = C
                    , icon   = IconShield
                    , cd     = 8
                    , effect = [ Grant Self 1 Invincibility Full 
                               , To Self RemoveDebuffs Full
                               , To Party GainStars <| Range 10 20 
                               ]
                    }
                  ]
    , passives  = [presenceConcealment B, divinity D]
    , phantasm  = { name   = "Uraeus Astrape" 
                  , desc   = "O, serpent who finishes the time of daybreak, come to me"
                  , rank   = A
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 300 500 
                             , To Self DemeritDamage <| Flat 1000
                             ]
                  , over   = [ Grant Self 1 (Performance Buster) <| Range 30 70 ]
                  , first  = True
                  }
    , gen       = { starWeight = 98, starRate = 25.5, npAtk = 1.06, npDef = 4 }
    , hits      = { quick = 4, arts = 2, buster = 3, ex = 6 }
    , gender    = Female
    , traits    = [Divine, EnumaElish, King]
    , death     = 49.5
    , align     = [Lawful, Neutral]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 5)]
                  [(Piece Assassin, 12), (PhoenixFeather, 6)]
                  [(Monument Assassin, 5), (LampOfEvilSealing, 6), (TearstoneOfBlood, 3)]
                  [(Monument Assassin, 12), (TearstoneOfBlood, 6), (SerpentJewel, 10)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 5)]
                  [(GemOf Assassin, 12)]
                  [(MagicGemOf Assassin, 5)]
                  [(MagicGemOf Assassin, 12), (LampOfEvilSealing, 3)]
                  [(SecretGemOf Assassin, 5), (LampOfEvilSealing, 6)]
                  [(SecretGemOf Assassin, 12), (PhoenixFeather, 4)]
                  [(PhoenixFeather, 8), (ShellOfReminiscence, 6)]
                  [(ShellOfReminiscence, 18), (ScarabOfWisdom, 10)]
    }
  , { name      = "Shuten-Douji"
    , id        = 112
    , rarity    = 5
    , class     = Assassin
    , attr      = Earth
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 25
    , stats     = { base  = { atk = 1853,  hp = 1881 }
                  , max   = { atk = 11993, hp = 12825 }
                  , grail = { atk = 13128, hp = 14050 }
                  }
    , skills    = [ { name   = "Intoxicating Aroma of Fruits"
                    , rank   = A
                    , icon   = IconHeart
                    , cd     = 9
                    , effect = [ Chance 60 <| Debuff Enemies 1 Charm Full
                               , Debuff Enemies 3 DefenseDown <| Range 10 20
                               ]
                    }
                  , { name   = "Demonic Nature of Oni"
                    , rank   = A
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Party 3 AttackUp <| Range 10 20
                               , Grant Self 3 NPUp <| Range 20 30
                               ]
                    }
                  , { name   = "Battle Continuation"
                    , rank   = APlus
                    , icon   = IconKneel
                    , cd     = 9
                    , effect = [ Times 1 << Grant Self 5 Guts <| Range 1200 2700 ]
                    }
                  ]
    , passives  = [presenceConcealment C, divinity C]
    , phantasm  = { name   = "Multicolored Poison—Shinpen Kidoku"
                  , desc   = ""
                  , rank   = B
                  , card   = Arts
                  , kind   = "Anti-Army"
                  , hits   = 1
                  , effect = [ To Enemies Damage <| Range 450 750
                             , Debuff Enemies 3 DebuffVuln <| Flat 10
                             , Debuff Enemies 3 AttackDown <| Flat 10
                             , Debuff Enemies 3 NPDown <| Flat 10
                             , Debuff Enemies 3 CritChance <| Flat 10
                             , Debuff Enemies 3 DefenseDown <| Flat 10
                             , Debuff Enemies 1 SealSkills Full
                             ]
                  , over   = [ Debuff Enemies 5 Poison <| Range 1000 5000 ]
                  , first  = False
                  }
    , gen       = { starWeight = 98, starRate = 25, npAtk = 0.55, npDef = 4 }
    , hits      = { quick = 4, arts = 3, buster = 1, ex = 6 }
    , gender    = Female
    , traits    = [Demonic, EnumaElish, Divine, Dragon]
    , death     = 31.6
    , align     = [Chaotic, Evil]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 5)]
                  [(Piece Assassin, 12), (EvilBone, 22)]
                  [(Monument Assassin, 5), (SerpentJewel, 8), (GhostLantern, 5)]
                  [(Monument Assassin, 12), (GhostLantern, 10), (HeartOfTheForeignGod, 5)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 5)]
                  [(GemOf Assassin, 12)]
                  [(MagicGemOf Assassin, 5)]
                  [(MagicGemOf Assassin, 12), (SerpentJewel, 4)]
                  [(SecretGemOf Assassin, 5), (SerpentJewel, 8)]
                  [(SecretGemOf Assassin, 12), (EvilBone, 15)]
                  [(EvilBone, 29), (ClawOfChaos, 4)]
                  [(ClawOfChaos, 11), (SpiritRoot, 10)]
    }
  , { name      = "Mysterious Heroine X"
    , id        = 86
    , rarity    = 5
    , class     = Assassin
    , attr      = Star
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 5
    , stats     = { base  = { atk = 1817,  hp = 1862 }
                  , max   = { atk = 11761, hp = 12696 }
                  , grail = { atk = 12874, hp = 13909 }
                  }
    , skills    = [ { name   = "Fire Support"
                    , rank   = EX
                    , icon   = IconStun
                    , cd     = 10
                    , effect = [ Chances 60 80 <| Debuff Enemies 1 StunBomb Full ]
                    }
                , { name     = "Intuition"
                    , rank   = CPlus
                    , icon   = IconStar
                    , cd     = 7
                    , effect = [ To Party GainStars <| Range 4 14 ]
                    }
                  , { name   = "Galactic Meteor Sword"
                    , rank   = C
                    , icon   = IconStarHaloUp
                    , cd     = 8
                    , effect = [ Grant Self 3 (Special AttackUp <| VsClass Saber) <| Range 30 50
                               , Grant Self 3 (Special StarUp <| VsClass Saber) <| Range 50 100
                               ]
                    }
                  ]
    , passives  = [riding EX, cosmoReactor A]
    , phantasm  = { name   = "Secret Calibur"
                  , desc   = "Sword of Unnamed Victory"
                  , rank   = EX
                  , card   = Quick
                  , kind   = "Anti-Unit"
                  , hits   = 12
                  , effect = [ To Enemy Damage <| Range 1600 2400 ]
                  , over   = [ To Enemy (SpecialDamage <| VsTrait Saberface) <| Range 150 200 ]
                  , first  = False
                  }
    , gen       = { starWeight = 98, starRate = 25.6, npAtk = 0.81, npDef = 4 }
    , hits      = { quick = 4, arts = 2, buster = 1, ex = 4 }
    , gender    = Female
    , traits    = [Riding, Dragon, Saberface, Arthur, King]
    , death     = 38.5
    , align     = [Chaotic, Good]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 5)]
                  [(Piece Assassin, 12), (DragonFang, 18)]
                  [(Monument Assassin, 5), (DragonsReverseScale, 2), (PhoenixFeather, 8)]
                  [(Monument Assassin, 12), (DragonsReverseScale, 4), (ProofOfHero, 36)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 5)]
                  [(GemOf Assassin, 12)]
                  [(MagicGemOf Assassin, 5)]
                  [(MagicGemOf Assassin, 12), (PhoenixFeather, 4)]
                  [(SecretGemOf Assassin, 5), (PhoenixFeather, 8)]
                  [(SecretGemOf Assassin, 12), (DragonFang, 12)]
                  [(VoidsDust, 12), (DragonFang, 24)]
                  [(VoidsDust, 36), (OctupletCrystals, 24)]
    }
  , { name      = "Ryougi Shiki (Assassin)"
    , id        = 92
    , rarity    = 4
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 4
    , stats     = { base  = { atk = 1477,  hp = 1768 }
                  , max   = { atk = 8867,  hp = 11055 }
                  , grail = { atk = 10736, hp = 13404 }
                  }
    , skills    = [ { name   = "Mystic Eyes of Death Perception"
                    , rank   = A
                    , icon   = IconMystic
                    , cd     = 7
                    , effect = [ Grant Self 1 IgnoreInvinc Full
                               , Grant Self 1 (Performance Arts) <| Range 30 50
                               , Debuff Enemy 1 DeathDown <| Range 80 100
                               ]
                    }
                  , { name   = "Mind's Eye (Fake)"
                    , rank   = A
                    , icon   = IconDodge
                    , cd     = 8
                    , effect = [ Grant Self 1 Evasion Full
                               , Grant Self 3 CritUp <| Range 20 40
                               ]
                    }
                  , { name   = "Yin-Yang"
                    , rank   = B
                    , icon   = IconYinYang
                    , cd     = 8
                    , effect = [ To Self GaugeUp <| Range 20 30
                               , To Self DemeritDamage <| Flat 1000
                               ]
                    }
                  ]
    , passives  = [presenceConcealment C, independentAction A]
    , phantasm  = { name   = "Vijñāpti-mātratā—Mystic Eyes of Death Perception"
                  , desc   = "Yuishiki・Chokushi no Magan"
                  , rank   = EX
                  , card   = Arts
                  , kind   = "Anti-Unit"
                  , hits   = 3
                  , effect = [ To Enemy DamageThruDef <| Range 900 1500 ]
                  , over   = [ To Enemy Kill <| Range 100 140 ]
                  , first  = True
                  }
    , gen       = { starWeight = 102, starRate = 25.6, npAtk = 0.8, npDef = 4 }
    , hits      = { quick = 4, arts = 2, buster = 1, ex = 5 }
    , gender    = Female
    , traits    = [EnumaElish, PseudoServant]
    , death     = 44
    , align     = [Chaotic, Good]
    , limited   = True
    , free      = True
    , ascendUp  = Welfare "Sharp Knife"
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 10)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 10), (EvilBone, 12)]
                  [(SecretGemOf Assassin, 4), (EvilBone, 24)]
                  [(SecretGemOf Assassin, 10), (ClawOfChaos, 3)]
                  [(ClawOfChaos, 5), (EternalGear, 5)]
                  [(EternalGear, 15), (HomunculusBaby, 20)]
    }
  , { name      = "Carmilla"
    , id        = 46
    , rarity    = 4
    , class     = Assassin
    , attr      = Earth
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 14
    , stats     = { base  = { atk = 1568,  hp = 1675 }
                  , max   = { atk = 9408,  hp = 10473 }
                  , grail = { atk = 11391, hp = 12698 }
                  }
    , skills    = [ { name   = "Vampirism"
                    , rank   = C
                    , icon   = IconDarkMagic
                    , cd     = 8
                    , effect = [ Chances 60 80 << To Enemy GaugeDown <| Flat 1
                               , To Self GaugeUp <| Range 18 27
                               ]
                    }
                  , { name   = "Torture Technique"
                    , rank   = A
                    , icon   = IconShieldDown
                    , cd     = 7
                    , effect = [ Debuff Enemy 3 DefenseDown <| Range 10 20 ]
                    }
                  , { name   = "Bath of Fresh Blood"
                    , rank   = A
                    , icon   = IconExclamationDown
                    , cd     = 8
                    , effect = [ Debuff Enemy 3 CritChance <| Range 30 50
                               , Grant Party 3 StarsPerTurn <| Range 5 10
                               ]
                    }
                  ]
    , passives  = [presenceConcealment D]
    , phantasm  = { name   = "Phantom Maiden"
                  , desc   = "Phantasmal Iron Maiden"
                  , rank   = C
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 1
                  , effect = [ To Enemy Damage <| Range 600 1000
                             , To Self Heal <| Flat 2000
                             , Grant Self 3 AttackUp <| Flat 20
                             ]
                  , over   = [ To Enemy (SpecialDamage <| VsTrait Female) <| Range 120 170 ]
                  , first  = False
                  }
    , gen       = { starWeight = 98, starRate = 25.2, npAtk = 2.15, npDef = 4 }
    , hits      = { quick = 2, arts = 1, buster = 1, ex = 3 }
    , gender    = Female
    , traits    = [EnumaElish]
    , death     = 44
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 4)]
                  [(Piece Assassin, 10), (SerpentJewel, 5)]
                  [(Monument Assassin, 4), (HeartOfTheForeignGod, 2), (HomunculusBaby, 8)]
                  [(Monument Assassin, 10), (HeartOfTheForeignGod, 4), (ClawOfChaos, 6)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 10)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 10), (HomunculusBaby, 4)]
                  [(SecretGemOf Assassin, 4), (HomunculusBaby, 8)]
                  [(SecretGemOf Assassin, 10), (SerpentJewel, 4)]
                  [(EvilBone, 15), (SerpentJewel, 7)]
                  [(EvilBone, 45), (ClawOfChaos, 12)]
    }
  , { name      = "Emiya (Assassin)"
    , id        = 109
    , rarity    = 4
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 24
    , stats     = { base  = { atk = 1493,  hp = 1786 }
                  , max   = { atk = 8958,  hp = 11168 }
                  , grail = { atk = 10846, hp = 13541 }
                  }
    , skills    = [ { name   = "Magecraft"
                    , rank   = B
                    , icon   = IconArtsUp
                    , cd     = 7
                    , effect = [ Grant Self 1 (Performance Arts) <| Range 24 40 ]
                    }
                  , { name   = "Affection of the Holy Grail"
                    , rank   = APlus
                    , icon   = IconShieldBreak
                    , cd     = 7
                    , effect = [ Grant Self 3 IgnoreInvinc Full
                               , Grant Self 3 CritUp <| Range 30 50
                               , Debuff Others 3 DebuffVuln <| Flat 20
                               ]
                    }
                  , { name   = "Scapegoat"
                    , rank   = C
                    , icon   = IconCrosshairUp
                    , cd     = 7
                    , effect = [ Grant Ally 1 Taunt Full
                               , To Party GainStars <| Range 5 15
                               ]
                    }
                  ]
    , passives  = [presenceConcealment APlus, independentAction A]
    , phantasm  = { name   = "Chronos Rose"
                  , desc   = "Gather Ye Rosebuds While Ye May"
                  , rank   = B
                  , card   = Arts
                  , kind   = "Anti-Unit"
                  , hits   = 15
                  , effect = [ To Enemy Damage <| Range 900 1500
                             , To Enemy GaugeDown <| Flat 1
                             ]
                  , over   = [ Debuff Enemy 3 CritChance <| Range 10 50 ]
                  , first  = False
                  }
    , gen       = { starWeight = 97, starRate = 25.6, npAtk = 0.46, npDef = 4 }
    , hits      = { quick = 4, arts = 2, buster = 6, ex = 8 }
    , gender    = Male
    , traits    = [BrynhildsBeloved, EnumaElish]
    , death     = 44
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 4)]
                  [(Piece Assassin, 10), (ProofOfHero, 18)]
                  [(Monument Assassin, 4), (EvilBone, 24), (VoidsDust, 8)]
                  [(Monument Assassin, 10), (VoidsDust, 16), (TearstoneOfBlood, 6)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 10)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 10), (EvilBone, 12)]
                  [(SecretGemOf Assassin, 4), (EvilBone, 24)]
                  [(SecretGemOf Assassin, 10), (ProofOfHero, 12)]
                  [(ProofOfHero, 24), (HomunculusBaby, 5)]
                  [(HomunculusBaby, 15), (HeartOfTheForeignGod, 8)]
    }
  , { name      = "Scathach (Assassin)"
    , id        = 133
    , rarity    = 4
    , class     = Assassin
    , attr      = Star
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 4
    , stats     = { base  = { atk = 1851,  hp = 1786 }
                  , max   = { atk = 9049,  hp = 11168 }
                  , grail = { atk = 10956, hp = 13541 }
                  }
    , skills    = [ { name   = "Beach Crisis"
                    , rank   = APlus
                    , icon   = IconCrosshairUp
                    , cd     = 8
                    , effect = [ Chances 100 300 <| Grant Self 1 Taunt Full
                               , Grant Self 1 CritUp <| Range 30 50
                               ]
                    }
                  , { name   = "Primordial Rune (Sea)"
                    , rank   = A
                    , icon   = IconHeal
                    , cd     = 8
                    , effect = [ To Ally Heal <| Range 1000 3000
                               , Grant Ally 1 DamageDown <| Range 500 1000
                               ]
                    }
                  , { name   = "Midsummer Mistake"
                    , rank   = C
                    , icon   = IconShieldBreak
                    , cd     = 8
                    , effect = [ Grant Self 1 IgnoreInvinc Full
                               , Grant Self 1 (Performance Quick) <| Range 30 50
                               ]
                    }
                  ]
    , passives  = [presenceConcealment E]
    , phantasm  = { name   = "Gáe Bolg Alternative"
                  , desc   = "Soaring Spear of Kicked Piercing Death"
                  , rank   = BPlus
                  , card   = Quick
                  , kind   = "Anti-Army"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 600 1000 ]
                  , over   = [ To Enemies Kill <| Range 30 70 ]
                  , first  = False
                  }
    , gen       = { starWeight = 98, starRate = 25.6, npAtk = 0.71, npDef = 4 }
    , hits      = { quick = 3, arts = 3, buster = 3, ex = 5 }
    , gender    = Female
    , traits    = [EnumaElish, King]
    , death     = 44
    , align     = [Neutral, Good]
    , limited   = True
    , free      = True
    , ascendUp  = Welfare "Bell-Ringing Branch"
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 10)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 10), (SeedOfYggdrasil, 5)]
                  [(SecretGemOf Assassin, 4), (SeedOfYggdrasil, 10)]
                  [(SecretGemOf Assassin, 10), (ShellOfReminiscence, 4)]
                  [(ShellOfReminiscence, 8), (SpiritRoot, 2)]
                  [(SpiritRoot, 6), (ScarabOfWisdom, 8)]
    }
  , { name      = "Stheno"
    , id        = 41
    , rarity    = 4
    , class     = Assassin
    , attr      = Heaven
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 4
    , stats     = { base  = { atk = 1497,  hp = 1843 }
                  , max   = { atk = 8985,  hp = 11518 }
                  , grail = { atk = 10879, hp = 13965 }
                  }
    , skills    = [ { name   = "Vampirism"
                    , rank   = C
                    , icon   = IconDarkMagic
                    , cd     = 8
                    , effect = [ Chances 60 80 << To Enemy GaugeDown <| Flat 1
                               , To Self GaugeUp <| Range 18 27
                               ]
                    }
                  , { name   = "Siren Song"
                    , rank   = A
                    , icon   = IconHeart
                    , cd     = 9
                    , effect = [ Chances 70 100 <| Debuff (EnemyType Male) 1 Charm Full ]
                    }
                  , { name   = "Whim of the Goddess"
                    , rank   = A
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Party 3 AttackUp <| Range 10 20
                               , Grant (AlliesType Divine) 3 AttackUp <| Range 10 20
                               ]
                    }
                  ]
    , passives  = [magicResistance A, presenceConcealment APlus, coreOfGoddess EX]
    , phantasm  = { name   = "Smile of the Stheno"
                  , desc   = "Goddess' Smile"
                  , rank   = B
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 0
                  , effect = [ To (EnemyType Male) Kill <| Range 100 150
                             , Chance 150 << Debuff Enemy 3 DefenseDown <| Flat 20
                             ]
                  , over   = [ Chances 100 200 <| Debuff (EnemyType Male) 1 Charm Full ]
                  , first  = False
                  }
    , gen       = { starWeight = 104, starRate = 25, npAtk = 2.26, npDef = 4 }
    , hits      = { quick = 2, arts = 1, buster = 1, ex = 3 }
    , gender    = Female
    , traits    = [Divine, EnumaElish]
    , death     = 27.5
    , align     = [Chaotic, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 4)]
                  [(Piece Assassin, 10), (SerpentJewel, 5)]
                  [(Monument Assassin, 4), (HeartOfTheForeignGod, 4), (VoidsDust, 8)]
                  [(Monument Assassin, 10), (VoidsDust, 16), (DragonsReverseScale, 4)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 10)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 10), (HeartOfTheForeignGod, 2)]
                  [(SecretGemOf Assassin, 4), (HeartOfTheForeignGod, 4)]
                  [(SecretGemOf Assassin, 10), (SerpentJewel, 4)]
                  [(SerpentJewel, 7), (ClawOfChaos, 3)]
                  [(ClawOfChaos, 9), (DragonsReverseScale, 8)]
    }
  , { name      = "Fuuma \"Evil-wind\" Kotarou"
    , id        = 117
    , rarity    = 3
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 8
    , stats     = { base  = { atk = 1316,  hp = 1592 }
                  , max   = { atk = 7091, hp = 8844 }
                  , grail = { atk = 9597, hp = 11991 }
                  }
    , skills    = [ { name   = "Sabotage"
                    , rank   = BPlus
                    , icon   = IconSwordDown
                    , cd     = 7
                    , effect = [ Debuff Enemies 3 AttackDown <| Flat 10
                               , Debuff Enemies 3 CritChance <| Range 10 20
                               ]
                    }
                  , { name   = "Ninjutsu"
                    , rank   = APlusPlusPlus
                    , icon   = IconDodge
                    , cd     = 8
                    , effect = [ Grant Ally 1 Evasion Full
                               , Grant Ally 1 StarUp <| Range 30 50
                               ]
                    }
                  , { name   = "Suspicious Shadow"
                    , rank   = C
                    , icon   = IconHoodDown
                    , cd     = 7
                    , effect = [ Debuff Enemies 1 DebuffVuln <| Range 50 100 ]
                    }
                  ]
    , passives  = [presenceConcealment APlus]
    , phantasm  = { name   = "Immortal Chaos Brigade"
                  , desc   = "Fumetsu no Konton Ryodan"
                  , rank   = B
                  , card   = Quick
                  , kind   = "Anti-Army"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 600 1000 ]
                  , over   = [ Debuff Enemies 5 Confusion <| Range 30 70 ]
                  , first  = False
                  }
    , gen       = { starWeight = 100, starRate = 25.6, npAtk = 0.54, npDef = 4 }
    , hits      = { quick = 4, arts = 4, buster = 1, ex = 4 }
    , gender    = Male
    , traits    = [EnumaElish]
    , death     = 38.5
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 4)]
                  [(Piece Assassin, 8), (EvilBone, 15)]
                  [(Monument Assassin, 4), (VoidsDust, 13), (OctupletCrystals, 4)]
                  [(Monument Assassin, 8), (OctupletCrystals, 7), (BlackBeastGrease, 5)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 8)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 8), (VoidsDust, 7)]
                  [(SecretGemOf Assassin, 4), (VoidsDust, 13)]
                  [(SecretGemOf Assassin, 8), (EvilBone, 10)]
                  [(EvilBone, 20), (SeedOfYggdrasil, 5)]
                  [(SeedOfYggdrasil, 15), (ClawOfChaos, 10)]
    }
  , { name      = "Sasaki Kojirou"
    , id        = 39
    , rarity    = 1
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 6
    , stats     = { base  = { atk = 1042, hp = 1244 }
                  , max   = { atk = 5735, hp = 6220 }
                  , grail = { atk = 8912, hp = 9588 }
                  }
    , skills    = [ { name   = "Mind's Eye (Fake)"
                    , rank   = A
                    , icon   = IconDodge
                    , cd     = 8
                    , effect = [ Grant Self 1 Evasion Full
                               , Grant Self 3 CritUp <| Range 20 40
                               ]
                    }
                  , { name   = "Vitrify"
                    , rank   = BPlus
                    , icon   = IconBubbles
                    , cd     = 6
                    , effect = [ To Self RemoveMental Full
                               , To Party GainStars <| Range 5 15
                               ]
                    }
                  , { name   = "Knowledge of the Sowa"
                    , rank   = B
                    , icon   = IconBullseye
                    , cd     = 7
                    , effect = [ Grant Self 3 SureHit Full
                               , Grant Self 3 StarUp <| Range 10 30
                               ]
                    }
                  ]
    , passives  = [presenceConcealment D]
    , phantasm  = { name   = "Swallow Reversal"
                  , desc   = "Hiken—Tsubame Gaeshi"
                  , rank   = Unknown
                  , card   = Quick
                  , kind   = "Anti-Personnel"
                  , hits   = 3
                  , effect = [ To Enemy Damage <| Range 1200 2000 ]
                  , over   = [ To Party GainStars <| Range 15 35 ]
                  , first  = False
                  }
    , gen       = { starWeight = 102, starRate = 25.3, npAtk = 1.05, npDef = 4 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , gender    = Male
    , traits    = [EnumaElish]
    , death     = 55
    , align     = [Neutral, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 2)]
                  [(Piece Assassin, 4), (ProofOfHero, 8)]
                  [(Monument Assassin, 2), (EternalGear, 2), (VoidsDust, 7)]
                  [(Monument Assassin, 4), (EternalGear, 4), (OctupletCrystals, 4)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 2)]
                  [(GemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 2)]
                  [(MagicGemOf Assassin, 4), (VoidsDust, 4)]
                  [(SecretGemOf Assassin, 2), (VoidsDust, 7)]
                  [(SecretGemOf Assassin, 4), (ProofOfHero, 5)]
                  [(ProofOfHero, 10), (EvilBone, 6)]
                  [(OctupletCrystals, 8), (EvilBone, 18)]
    }
  , { name      = "Hassan of the Cursed Arm"
    , id        = 40
    , rarity    = 2
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 2
    , stats     = { base  = { atk = 1114, hp = 1429 }
                  , max   = { atk = 6280, hp = 7594 }
                  , grail = { atk = 9100, hp = 10960 }
                  }
    , skills    = [ { name   = "Throw (Dagger)"
                    , rank   = B
                    , icon   = IconStar
                    , cd     = 6
                    , effect = [ To Party GainStars <| Range 3 12 ]
                    }
                  , { name   = "Self-Modification"
                    , rank   = C
                    , icon   = IconExclamationUp
                    , cd     = 7
                    , effect = [ Grant Self 3 CritUp <| Range 20 50 ]
                    }
                  , { name   = "Protection Against the Wind"
                    , rank   = A
                    , icon   = IconDodge
                    , cd     = 7
                    , effect = [ Times 3 <| Grant Self 0 Evasion Full
                               , Grant Self 3 StarUp <| Range 10 30
                               ]
                    }
                  ]
    , passives  = [presenceConcealment APlus]
    , phantasm  = { name   = "Zabaniya"
                  , desc   = "Delusional Heartbeat"
                  , rank   = C
                  , card   = Quick
                  , kind   = "Anti-Personnel"
                  , hits   = 1
                  , effect = [ To Enemy Damage <| Range 1200 2000 ]
                  , over   = [ To Enemy Kill <| Range 80 120 ]
                  , first  = True
                  }
    , gen       = { starWeight = 97, starRate = 25.2, npAtk = 1.07, npDef = 4 }
    , hits      = { quick = 3, arts = 2, buster = 1, ex = 3 }
    , gender    = Male
    , traits    = [EnumaElish]
    , death     = 44
    , align     = [Lawful, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 3)]
                  [(Piece Assassin, 6), (EvilBone, 11)]
                  [(Monument Assassin, 3), (EternalGear, 5), (VoidsDust, 5)]
                  [(Monument Assassin, 6), (HomunculusBaby, 6), (VoidsDust, 10)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 3)]
                  [(GemOf Assassin, 6)]
                  [(MagicGemOf Assassin, 3)]
                  [(MagicGemOf Assassin, 6), (EternalGear, 3)]
                  [(SecretGemOf Assassin, 3), (EternalGear, 5)]
                  [(SecretGemOf Assassin, 6), (EvilBone, 8)]
                  [(GhostLantern, 3), (EvilBone, 15)]
                  [(GhostLantern, 9), (HomunculusBaby, 12)]
    }
  , { name      = "Henry Jekyll & Hyde"
    , id        = 81
    , rarity    = 3
    , class     = Assassin
    , attr      = Earth
    , deck      = Deck Quick Quick Arts Buster Buster
    , curve     = 23
    , stats     = { base  = { atk = 1173, hp = 1741 }
                  , max   = { atk = 6320, hp = 9675 }
                  , grail = { atk = 8553, hp = 13118 }
                  }
    , skills    = [ { name   = "Monstrous Strength"
                    , rank   = B
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Self 3 AttackUp <| Range 5 15
                               , When "transformed into Hyde" << Grant Self 3 AttackUp <| Range 25 35
                               ]
                    }
                  , { name   = "Panicky Voice"
                    , rank   = A
                    , icon   = IconStun
                    , cd     = 8
                    , effect = [ Times 1 << Grant Self 0 (Success Stun) <| Range 5 15
                               , When "transformed into Hyde" << Times 1 << Grant Self 0 (Success Stun) <| Range 85 135
                               , Chance 10 <| Debuff Enemy 1 Stun Full
                               ]
                    }
                  , { name   = "Self-Modification"
                    , rank   = D
                    , icon   = IconExclamationUp
                    , cd     = 7
                    , effect = [ Grant Self 3 CritUp <| Range 5 15
                               , When "transformed into Hyde" << Grant Self 3 CritUp <| Range 25 35
                               ]
                    }
                  ]
    , passives  = [presenceConcealment A, madness A]
    , phantasm  = { name   = "Dangerous Game"
                  , desc   = "The Secret Game of Sin"
                  , rank   = CPlus
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 0
                  , effect = [ To Self BecomeHyde Full
                             , Grant Self 0 MaxHP <| Range 3000 6000
                             , To Self Heal Full
                             ]
                  , over   = [ Grant Self 0 (Performance Buster) <| Range 40 80 ]
                  , first  = False
                  }
    , gen       = { starWeight = 99, starRate = 25.6, npAtk = 1.05, npDef = 4 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , gender    = Male
    , traits    = [EnumaElish, BrynhildsBeloved]
    , death     = 55
    , align     = [Lawful, Good, Chaotic, Evil]
    , limited   = False
    , free      = True
    , ascendUp  = Ascension
                  [(Piece Assassin, 4)]
                  [(EvilBone, 15), (Piece Assassin, 8)]
                  [(VoidsDust, 13), (OctupletCrystals, 4), (Monument Assassin, 4)]
                  [(Monument Assassin, 8), (OctupletCrystals, 7), (HomunculusBaby, 8)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 8)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 8), (VoidsDust, 7)]
                  [(SecretGemOf Assassin, 4), (VoidsDust, 13)]
                  [(SecretGemOf Assassin, 8), (EvilBone, 10)]
                  [(EvilBone, 20), (HeartOfTheForeignGod, 2)]
                  [(HeartOfTheForeignGod, 5), (HomunculusBaby, 16)]
    }
  , { name      = "Jing Ke"
    , id        = 42
    , rarity    = 3
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 8
    , stats     = { base  = { atk = 1338, hp = 1492 }
                  , max   = { atk = 7207, hp = 8293 }
                  , grail = { atk = 9754, hp = 11244 }
                  }
    , skills    = [ { name   = "Restrain"
                    , rank   = A
                    , icon   = IconStarUp
                    , cd     = 7
                    , effect = [ Grant Self 3 StarAbsorb <| Flat 200
                               , To Party GainStars <| Range 5 15
                               ]
                    }
                  , { name   = "Planning"
                    , rank   = B
                    , icon   = IconStarHaloUp
                    , cd     = 7
                    , effect = [ Grant Self 3 StarUp <| Range 10 30 ]
                    }
                  , { name   = "Insolent"
                    , rank   = A
                    , icon   = IconQuickUp
                    , cd     = 7
                    , effect = [ Grant Self 1 (Performance Quick) <| Range 20 30
                               , Grant Self 1 CritUp <| Range 30 50
                               ]
                    }
                  ]
    , passives  = [presenceConcealment B]
    , phantasm  = { name   = "All I Do Is Kill"
                  , desc   = ""
                  , rank   = B
                  , card   = Quick
                  , kind   = "Anti-Personnel"
                  , hits   = 1
                  , effect = [ To Enemy Damage <| Range 1200 2000
                             , To Self DemeritDamage <| Flat 1000
                             ]
                  , over   = [ To Enemy Kill <| Range 50 100
                             , To Party GainStars <| Range 15 35
                             ]
                  , first  = True
                  }
    , gen       = { starWeight = 98, starRate = 25.2, npAtk = 1.05, npDef = 4 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , gender    = Female
    , traits    = [EnumaElish]
    , death     = 55
    , align     = [Chaotic, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 4)]
                  [(EvilBone, 15), (Piece Assassin, 8)]
                  [(GhostLantern, 4), (OctupletCrystals, 7), (Monument Assassin, 4)]
                  [(GhostLantern, 7), (VoidsDust, 16), (Monument Assassin, 8)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 8)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 8), (OctupletCrystals, 4)]
                  [(SecretGemOf Assassin, 4), (OctupletCrystals, 7)]
                  [(SecretGemOf Assassin, 8), (EvilBone, 10)]
                  [(PhoenixFeather, 4), (EvilBone, 20)]
                  [(PhoenixFeather, 10), (VoidsDust, 32)]
    }
  , { name      = "Charles-Henri Sanson"
    , id        = 43
    , rarity    = 2
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 12
    , stats     = { base  = { atk = 968,  hp = 1568 }
                  , max   = { atk = 5456, hp = 8309 }
                  , grail = { atk = 7906, hp = 11991 }
                  }
    , skills    = [ { name   = "Executioner"
                    , rank   = APlusPlus
                    , icon   = IconDamageUp
                    , cd     = 7
                    , effect = [ Grant Self 3 (Special AttackUp <| VsAlignment Evil) <| Range 40 60 ]
                    }
                  , { name   = "Medicine"
                    , rank   = APlus
                    , icon   = IconHeal
                    , cd     = 7
                    , effect = [ To Ally Heal <| Range 1000 3000
                               , To Ally RemoveDebuffs Full
                               ]
                    }
                  , { name   = "Human Study"
                    , rank   = B
                    , icon   = IconDamageUp
                    , cd     = 7
                    , effect = [ Grant Self 3 (Special AttackUp <| VsTrait Human) <| Range 40 60 ]
                    }
                  ]
    , passives  = [presenceConcealment D]
    , phantasm  = { name   = "La Mort Espoir"
                  , desc   = "Death is Hope For Tomorrow"
                  , rank   = A
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 1
                  , effect = [ To Enemy Damage <| Range 600 1000 ]
                  , over   = [ To Enemy Kill <| Range 30 80
                             , Debuff Enemy 3 DefenseDown <| Range 20 40
                             ]
                  , first  = True
                  }
    , gen       = { starWeight = 102, starRate = 24.8, npAtk = 1.06, npDef = 4 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , gender    = Male
    , traits    = [EnumaElish]
    , death     = 49.5
    , align     = [Lawful, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 3)]
                  [(Piece Assassin, 6), (EvilBone, 11)]
                  [(Monument Assassin, 3), (HeartOfTheForeignGod, 1), (VoidsDust, 10)]
                  [(Monument Assassin, 6), (HeartOfTheForeignGod, 2), (HomunculusBaby, 6)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 3)]
                  [(GemOf Assassin, 6)]
                  [(MagicGemOf Assassin, 3)]
                  [(MagicGemOf Assassin, 6), (VoidsDust, 5)]
                  [(SecretGemOf Assassin, 3), (VoidsDust, 10)]
                  [(SecretGemOf Assassin, 6), (EvilBone, 8)]
                  [(ForbiddenPage, 3), (EvilBone, 15)]
                  [(ForbiddenPage, 9), (HomunculusBaby, 12)]
    }
  , { name      = "Hassan of the Hundred Personas"
    , id        = 110
    , rarity    = 3
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 28
    , stats     = { base  = { atk = 1241, hp = 1675 }
                  , max   = { atk = 6686, hp = 9310 }
                  , grail = { atk = 9049, hp = 12623 }
                  }
    , skills    = [ { name   = "Librarian of Knowledge"
                    , rank   = C
                    , icon   = IconNobleUp
                    , cd     = 7
                    , effect = [ Grant Self 3 NPGen <| Range 10 20
                               , Grant Self 3 StarUp <| Range 20 40
                               ]
                    }
                  , { name   = "Wide Specialization"
                    , rank   = APlus
                    , icon   = IconAllUp
                    , cd     = 7
                    , effect = [ Chances 60 80 << Grant Self 3 (Performance Buster) <| Flat 30
                               , Chances 60 80 << Grant Self 3 (Performance Quick) <| Flat 30
                               , Chances 60 80 << Grant Self 3 (Performance Arts) <| Flat 30
                               , Grant Self 1 Evasion Full
                               ]
                    }
                  , { name   = "Battle Retreat"
                    , rank   = B
                    , icon   = IconHeal
                    , cd     = 8
                    , effect = [ To Self Heal <| Range 2000 4000
                               , To Self DemeritBuffs Full
                               ]
                    }
                  ]
    , passives  = [presenceConcealment A]
    , phantasm  = { name   = "Zabaniya"
                  , desc   = "Delusional Illusion"
                  , rank   = BPlus
                  , card   = Arts
                  , kind   = "Anti-Unit"
                  , hits   = 13
                  , effect = [ To Enemy Damage <| Range 900 1500 ]
                  , over   = [ Debuff Enemy 3 CritChance <| Range 10 50 ]
                  , first  = False
                  }
    , gen       = { starWeight = 97, starRate = 25.5, npAtk = 0.48, npDef = 4 }
    , hits      = { quick = 3, arts = 3, buster = 1, ex = 6 }
    , gender    = Female
    , traits    = [EnumaElish]
    , death     = 44
    , align     = [Lawful, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 4)]
                  [(Piece Assassin, 8), (VoidsDust, 10)]
                  [(Monument Assassin, 4), (OctupletCrystals, 7), (SerpentJewel, 3)]
                  [(Monument Assassin, 4), (SerpentJewel, 6), (BlackBeastGrease, 5)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 8)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 8), (OctupletCrystals, 4)]
                  [(SecretGemOf Assassin, 4), (OctupletCrystals, 7)]
                  [(SecretGemOf Assassin, 8), (VoidsDust, 7)]
                  [(VoidsDust, 13), (EvilBone, 12)]
                  [(EvilBone, 36), (ClawOfChaos, 10)]
    }
  , { name      = "Hassan of the Serenity"
    , id        = 124
    , rarity    = 3
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 23
    , stats     = { base  = { atk = 1232, hp = 1675 }
                  , max   = { atk = 6636, hp = 9310 }
                  , grail = { atk = 8981, hp = 12623 }
                  }
    , skills    = [ { name   = "Morph (Infiltration)"
                    , rank   = C
                    , icon   = IconExclamationDown
                    , cd     = 9
                    , effect = [ Debuff Enemy 3 CritChance <| Range 10 20
                               , To Enemy GaugeDown <| Flat 1
                               ]
                    }
                  , { name   = "Throw (Dagger)"
                    , rank   = C
                    , icon   = IconStar
                    , cd     = 6
                    , effect = [ To Party GainStars <| Range 2 12 ]
                    }
                  , { name   = "Dance of Silence"
                    , rank   = B
                    , icon   = IconReaperUp
                    , cd     = 8
                    , effect = [ Grant Self 3 KillUp <| Range 20 50
                               , Grant Self 3 DebuffSuccess <| Range 20 50
                               ]
                    }
                  ]
    , passives  = [presenceConcealment APlus, independentAction A]
    , phantasm  = { name   = "Zabaniya"
                  , desc   = "Delusional Poison Body"
                  , rank   = C
                  , card   = Arts
                  , kind   = "Anti-Unit"
                  , hits   = 1
                  , effect = [ Debuff Enemy 5 Poison <| Flat 1000
                             , Chance 40 <| Debuff Enemy 1 SealSkills Full
                             , Chance 40 <| Debuff Enemy 1 SealNP Full
                             , To Enemy Damage <| Range 900 1500
                             ]
                  , over   = [ To Enemy Kill <| Range 60 100 ]
                  , first  = True
                  }
    , gen       = { starWeight = 102, starRate = 25.6, npAtk = 0.53, npDef = 4 }
    , hits      = { quick = 3, arts = 3, buster = 4, ex = 5 }
    , gender    = Female
    , traits    = [EnumaElish]
    , death     = 44
    , align     = [Lawful, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 4)]
                  [(Piece Assassin, 8), (EvilBone, 15)]
                  [(Monument Assassin, 4), (FoolsChain, 20), (TearstoneOfBlood, 2)]
                  [(Monument Assassin, 8), (TearstoneOfBlood, 4), (BlackBeastGrease, 5)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 4)]
                  [(GemOf Assassin, 8)]
                  [(MagicGemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 8), (FoolsChain, 10)]
                  [(SecretGemOf Assassin, 4), (FoolsChain, 20)]
                  [(SecretGemOf Assassin, 8), (EvilBone, 10)]
                  [(EvilBone, 20), (VoidsDust, 8)]
                  [(VoidsDust, 10), (LampOfEvilSealing, 10)]
    }
  , { name      = "Phantom of the Opera"
    , id        = 44
    , rarity    = 2
    , class     = Assassin
    , attr      = Earth
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 2
    , stats     = { base  = { atk = 1003, hp = 1580 }
                  , max   = { atk = 5654, hp = 8393 }
                  , grail = { atk = 8193, hp = 12112 }
                  }
    , skills    = [ { name  = "Innocent Monster"
                    , rank   = D
                    , icon    = IconStarTurn
                    , cd      = 7
                    , effect = [ Grant Party 3 StarsPerTurn <| Range 3 9
                               , Debuff Self 3 DefenseDown <| Flat 14
                               ]
                    }
                  , { name   = "Siren Song"
                    , rank   = B
                    , icon   = IconHeart
                    , cd     = 9
                    , effect = [ Chances 60 90 <| Debuff (EnemyType Female) 1 Charm Full ]
                    }
                  , { name   = "Mental Corruption"
                    , rank   = A
                    , icon   = IconStaffUp
                    , cd     = 7
                    , effect = [ Grant Self 3 MentalSuccess <| Range 5 25
                               , Grant Self 3 MentalResist <| Range 50 100
                               ]
                    }
                  ]
    , passives  = [presenceConcealment A]
    , phantasm  = { name   = "Christine Christine"
                  , desc   = "Love Song Resounding through Hell"
                  , rank   = BPlus
                  , card   = Arts
                  , kind   = "Anti-Army"
                  , hits   = 1
                  , effect = [ To Enemies DamageThruDef <| Range 600 900 ]
                  , over   = [ Debuff Enemies 6 DebuffVuln <| Range 50 100 ]
                  , first  = False
                  }
    , gen       = { starWeight = 98, starRate = 25.2, npAtk = 0.71, npDef = 4 }
    , hits      = { quick = 2, arts = 3, buster = 2, ex = 3 }
    , gender    = Male
    , traits    = [EnumaElish]
    , death     = 49.5
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 3)]
                  [(Piece Assassin, 6), (GhostLantern, 4)]
                  [(Monument Assassin, 3), (VoidsDust, 10), (EternalGear, 3)]
                  [(Monument Assassin, 6), (EternalGear, 5), (EvilBone, 18)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 3)]
                  [(GemOf Assassin, 6)]
                  [(MagicGemOf Assassin, 3)]
                  [(MagicGemOf Assassin, 6), (VoidsDust, 5)]
                  [(SecretGemOf Assassin, 3), (VoidsDust, 10)]
                  [(SecretGemOf Assassin, 6), (GhostLantern, 3)]
                  [(GhostLantern, 5), (HeartOfTheForeignGod, 2)]
                  [(HeartOfTheForeignGod, 4), (EvilBone, 36)]
    }
  , { name      = "Mata Hari"
    , id        = 45
    , rarity    = 1
    , class     = Assassin
    , attr      = Mankind
    , deck      = Deck Quick Quick Quick Arts Buster
    , curve     = 6
    , stats     = { base  = { atk = 977,  hp = 1313 }
                  , max   = { atk = 5377, hp = 6565 }
                  , grail = { atk = 8355, hp = 10120 }
                  }
    , skills    = [ { name   = "Espionage"
                    , rank   = APlusPlus
                    , icon   = IconStarHaloUp
                    , cd     = 7
                    , effect = [ Grant Self 3 StarUp <| Range 10 30 ]
                    }
                  , { name   = "Pheromone"
                    , rank   = B
                    , icon   = IconHeart
                    , cd     = 8
                    , effect = [ Chances 30 60 <| Debuff (EnemiesType Male) 1 Charm Full
                               , Debuff Enemies 3 DefenseDown <| Range 10 20
                               ]
                    }
                  , { name   = "Double-cross"
                    , rank   = B
                    , icon   = IconCircuits
                    , cd     = 8
                    , effect = [ Debuff Enemy 1 SealSkills Full
                               , Debuff Enemy 3 DefenseDown <| Range 10 20
                               ]
                    }
                  ]
    , passives  = []
    , phantasm  = { name   = "Mata Hari"
                  , desc   = "The Woman with Sunny Eyes"
                  , rank   = A
                  , card   = Arts
                  , kind   = "Anti-Army"
                  , hits   = 0
                  , effect = [ Chances 40 60 <| Debuff Enemies 1 Charm Full ]
                  , over   = [ Debuff Enemies 1 AttackDown <| Range 20 40
                             , Debuff Enemies 1 DefenseDown <| Range 20 40
                             ]
                  , first  = False
                  }
    , gen       = { starWeight = 98, starRate = 24.6, npAtk = 2.1, npDef = 4 }
    , hits      = { quick = 2, arts = 1, buster = 1, ex = 3 }
    , gender    = Female
    , traits    = [EnumaElish]
    , death     = 55
    , align     = [Chaotic, Balanced]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Assassin, 2)]
                  [(Piece Assassin, 4), (PhoenixFeather, 2)]
                  [(Monument Assassin, 2), (EternalGear, 4), (GhostLantern, 2)]
                  [(Monument Assassin, 4), (SerpentJewel, 4), (GhostLantern, 4)]
    , skillUp   = Reinforcement
                  [(GemOf Assassin, 2)]
                  [(GemOf Assassin, 4)]
                  [(MagicGemOf Assassin, 2)]
                  [(MagicGemOf Assassin, 4), (EternalGear, 2)]
                  [(SecretGemOf Assassin, 2), (EternalGear, 4)]
                  [(SecretGemOf Assassin, 4), (PhoenixFeather, 2)]
                  [(HomunculusBaby, 2), (PhoenixFeather, 3)]
                  [(HomunculusBaby, 6), (SerpentJewel, 7)]
    }
  ]
