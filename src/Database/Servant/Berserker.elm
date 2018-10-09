module Database.Servant.Berserker exposing (berserkers)

import Database.Base exposing (..)
import Database.Passive exposing (..)
import Database.Servant exposing (..)
import Database.Skill exposing (..)

berserkers : List Servant
berserkers = 
  [ { name  =   "Cu Chulainn (Alter)"
    , id        = 98
    , rarity    = 5
    , class     = Berserker
    , attr      = Earth
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 10
    , stats     = { base =  { atk = 1979,  hp = 1790 }
                  , max =   { atk = 12805, hp = 12210 }
                  , grail = { atk = 14017, hp = 13377 }
                  }
    , skills    = [ { name  = "Madness of the Spirits"
                  , rank   = A
                  , icon    = IconExclamationDown
                  , cd      = 8
                  , effect = [ Debuff Enemies 3 AttackDown <| Range 10 20
                             , Debuff Enemies 3 CritChance <| Range 30 50
                             ]
                  }
                  , { name    = "Protection from Arrows"
                  , rank   = C
                  , icon    = IconDodge
                  , cd      = 7
                  , effect = [ Times 2 <| Grant Self 0 Evasion Full
                             , Grant Self 3 DefenseUp <| Range 7 14
                             ]
                  }
                  , { name    = "Battle Continuation"
                  , rank   = A
                  , icon    = IconKneel
                  , cd      = 9
                  , effect = [ Times 1 << Grant Self 5 Guts <| Range 1000 2500 ]
                  }
                ]
    , passives  = [madness C, divinity C]
    , phantasm  = { name   = "Curruid Coinchenn"
                  , desc   = "Beast of Crunching Deathtusk"
                  , rank   = A
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 12
                  , effect = [ To Enemy Damage <| Range 600 1000 ]
                  , over   = [ Grant Self 1 AttackUp <| Range 30 70
                             , Grant Self 1 DefenseUp <| Range 30 70
                             ]
                  , first  = True
                  }
    , gen       = { starWeight = 9, starRate = 5.1, npAtk = 0.69, npDef = 5 }
    , hits      = { quick = 4, arts = 3, buster = 3, ex = 5 }
    , traits    = [Male, Divine, Brynhild, EnumaElish]
    , death     = 52
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 5)]
                  [(Piece Berserker, 12), (EvilBone, 22)]
                  [(Monument Berserker, 5), (ClawOfChaos, 6), (HeartOfTheForeignGod, 2)]
                  [( Monument Berserker, 12), (HeartOfTheForeignGod, 4)
                  , (TearstoneOfBlood, 8)
                ]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 5)]
                  [(GemOf Berserker, 12)]
                  [(MagicGemOf Berserker, 5)]
                  [(MagicGemOf Berserker, 12), (ClawOfChaos, 3)]
                  [(SecretGemOf Berserker, 5), (ClawOfChaos, 6)]
                  [(SecretGemOf Berserker, 12), (EvilBone, 15)]
                  [(EvilBone, 29), (SerpentJewel, 5)]
                  [(SerpentJewel, 15), (VoidsDust, 48)]
    }
  , { name      = "Minamoto-no-Raikou"
    , id        = 114
    , rarity    = 5
    , class     = Berserker
    , attr      = Heaven
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 30
    , stats     = { base =  { atk = 1786,  hp = 1980 }
                  , max =   { atk = 11556, hp = 13500 }
                  , grail = { atk = 12650, hp = 14790 }
                  }
    , skills    = [ { name  = "Eternal Arms Mastery"
                  , rank   = APlus
                  , icon    = IconStarUp
                  , cd      = 7
                  , effect = [ Grant Self 3 StarAbsorb <| Range 3000 6000 ]
                  }
                  , { name    = "Mana Burst (Lightning)"
                  , rank   = A
                  , icon    = IconBusterUp
                  , cd      = 8
                  , effect = [ Grant Self 1 (Performance Buster) <| Range 20 30
                             , Grant Self 1 Evasion Full
                             ]
                  }
                  , { name    = "Mystic Slayer"
                  , rank   = A
                  , icon    = IconDamageUp
                  , cd      = 8
                  , effect = [ Grant Self 3 (AttackVs Demonic) <| Range 30 50
                             , Grant Self 3 (AttackVs HeavenOrEarth) <| Range 30 50
                             ]
                  }
                ]
    , passives  = [magicResistance D, madness EX, riding APlus, divinity C]
    , phantasm  = { name   = "Vengeful Lightning of the Ox-King"
                  , desc   = "The Inescapable Net of Heaven"
                  , rank   = BPlusPlus
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 7
                  , effect = [ To Enemies Damage <| Range 300 500 ]
                  , over   = [ Grant Self 1 StarUp <| Range 100 300 ]
                  , first  = True
                  }
    , gen       = { starWeight = 9, starRate = 4.9, npAtk = 0.46, npDef = 5 }
    , hits      = { quick = 3, arts = 4, buster = 1, ex = 5 }
    , traits    = [Female, Divine, Riding, EnumaElish]
    , death     = 39
    , align     = [Chaotic, Good]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 5)]
                  [(Piece Berserker, 12), (ClawOfChaos, 5)]
                  [(Monument Berserker, 5), (EvilBone, 29), (TearstoneOfBlood, 3)]
                  [( Monument Berserker, 12), (TearstoneOfBlood, 6)
                  , (DragonsReverseScale, 5)
                ]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 5)]
                  [(GemOf Berserker, 12)]
                  [(MagicGemOf Berserker, 5)]
                  [(MagicGemOf Berserker, 12), (EvilBone, 15)]
                  [(SecretGemOf Berserker, 5), (EvilBone, 29)]
                  [(SecretGemOf Berserker, 12), (ClawOfChaos, 3)]
                  [(ClawOfChaos, 6), (OctupletCrystals, 6)]
                  [(OctupletCrystals, 18), (SpiritRoot, 10)]
    }
  , { name      = "Sakata Kintoki"
    , id        = 51
    , rarity    = 5
    , class     = Berserker
    , attr      = Mankind
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 9
    , stats     = { base =  { atk = 1964,  hp = 1782 }
                  , max =   { atk = 12712, hp = 12150 }
                  , grail = { atk = 13915, hp = 13311 }
                  }
    , skills    = [ { name  = "Monstrous Strength"
                  , rank   = APlus
                  , icon    = IconSwordUp
                  , cd      = 7
                  , effect = [ Grant Self 1 AttackUp <| Range 30 50 ]
                  }
                  , { name    = "Animal Communication"
                  , rank   = C
                  , icon    = IconNoble
                  , cd      = 8
                  , effect = [ To Self GaugeUp <| Range 30 50 ]
                  }
                  , { name    = "Natural Body"
                  , rank   = A
                  , icon    = IconHoodUp
                  , cd      = 7
                  , effect = [ Grant Self 3 OffensiveResist <| Range 60 120
                             , To Self Heal <| Range 1000 3000
                             ]
                  }
                ]
    , passives  = [madness E, divinity D]
    , phantasm  = { name   = "Golden Spark"
                  , desc   = "Golden Impact"
                  , rank   = CMinus
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 1
                  , effect = [ To Enemy DamageThruDef <| Range 600 1000 ]
                  , over   = [ Chances 50 100 <| Debuff Enemy 1 Stun Full ]
                  , first  = False
                  }
    , gen       = { starWeight = 9, starRate = 5, npAtk = 1.03, npDef = 5 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Male, Divine, Brynhild, EnumaElish]
    , death     = 52
    , align     = [Lawful, Good]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 5)]
                  [(Piece Berserker, 12), (ProofOfHero, 22)]
                  [(Monument Berserker, 5), (OctupletCrystals, 10), (SeedOfYggdrasil, 6)]
                  [( Monument Berserker, 12), (SeedOfYggdrasil, 12)
                  , (HeartOfTheForeignGod, 5)
                ]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 5)]
                  [(GemOf Berserker, 12)]
                  [(MagicGemOf Berserker, 5)]
                  [(MagicGemOf Berserker, 12), (OctupletCrystals, 5)]
                  [(SecretGemOf Berserker, 5), (OctupletCrystals, 10)]
                  [(SecretGemOf Berserker, 12), (ProofOfHero, 15)]
                  [(ProofOfHero, 29), (VoidsDust, 12)]
                  [(VoidsDust, 36), (HeartOfTheForeignGod, 10)]
    }
  , { name      = "Vlad III"
    , id        = 52
    , rarity    = 5
    , class     = Berserker
    , attr      = Earth
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 15
    , stats     = { base =  { atk = 1777,  hp = 2019 }
                  , max =   { atk = 11499, hp = 13770 }
                  , grail = { atk = 12587, hp = 15086 }
                  }
    , skills    = [ { name  = "Vampirism"
                  , rank   = A
                  , icon    = IconDarkMagic
                  , cd      = 8
                  , effect = [ Chances 80 100 << To Enemy GaugeDown <| Flat 1
                             , To Self GaugeUp <| Range 20 30
                             ]
                  }
                  , { name    = "Morph"
                  , rank   = C
                  , icon    = IconShieldUp
                  , cd      = 7
                  , effect = [ Grant Self 3 DefenseUp <| Range 16 24 ]
                  }
                  , { name    = "Battle Continuation"
                  , rank   = A
                  , icon    = IconKneel
                  , cd      = 9
                  , effect = [ Times 1 << Grant Self 5 Guts <| Range 1000 2500 ]
                  }
                ]
    , passives  = [madness EX]
    , phantasm  = { name   = "Kazikli Bey"
                  , desc   = "Bloody Monster King"
                  , rank   = CPlus
                  , card   = Arts
                  , kind   = "Anti-Personnel"
                  , hits   = 10
                  , effect = [ To Enemy Damage <| Range 900 1500 ]
                  , over   = [ To Party GainStars <| Range 20 40 ]
                  , first  = False
                  }
    , gen       = { starWeight = 9, starRate = 4.9, npAtk = 0.5, npDef = 5 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Male, EnumaElish, Brynhild, King]
    , death     = 45.5
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 5)]
                  [(Piece Berserker, 12), (DragonsReverseScale, 3)]
                  [(Monument Berserker, 5), (ClawOfChaos, 6), (ForbiddenPage, 5)]
                  [(Monument Berserker, 12), (VoidsDust, 24), (ForbiddenPage, 10)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 5)]
                  [(GemOf Berserker, 12)]
                  [(MagicGemOf Berserker, 5)]
                  [(MagicGemOf Berserker, 12), (ClawOfChaos, 3)]
                  [(SecretGemOf Berserker, 5), (ClawOfChaos, 6)]
                  [(SecretGemOf Berserker, 12), (DragonsReverseScale, 2)]
                  [(DragonsReverseScale, 4), (EvilBone, 18)]
                  [(VoidsDust, 48), (EvilBone, 54)]
    }
  , { name      = "Florence Nightingale"
    , id        = 97
    , rarity    = 5
    , class     = Berserker
    , attr      = Mankind
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 30
    , stats     = { base =  { atk = 1573,  hp = 2232 }
                  , max =   { atk = 10184, hp = 15221 }
                  , grail = { atk = 11148, hp = 16675 }
                  }
    , skills    = [ { name  = "Nurse of Steel"
                  , rank   = A
                  , icon    = IconHeal
                  , cd      = 7
                  , effect = [ To Ally Heal <| Range 2000 4000 ]
                  }
                  , { name    = "Understanding of the Human Body"
                  , rank   = A
                  , icon    = IconDamageUp
                  , cd      = 8
                  , effect = [ Grant Self 3 (AttackVs Humanoid) <| Range 30 50
                             , Grant Self 3 (DefenseVs Humanoid) <| Range 15 25
                             ]
                  }
                  , { name    = "Angel's Cry"
                  , rank   = EX
                  , icon    = IconBusterUp
                  , cd      = 7
                  , effect = [ Grant Ally 3 (Performance Buster) <| Range 30 50 ]
                  }
                ]
    , passives  = [madness EX]
    , phantasm  = { name   = "Nightingale Pledge"
                  , desc   = "I Will Abstain From All Poisons and Evils"
                  , rank   = C
                  , card   = Arts
                  , kind   = "Anti-Army"
                  , hits   = 0
                  , effect = [ To Party RemoveDebuffs Full
                             , To Party Heal <| Range 3000 5000
                             ]
                  , over   = [ Debuff Enemies 1 NPDown <| Range 50 100 ]
                  , first  = False
                  }
    , gen       = { starWeight = 10, starRate = 5, npAtk = 0.77, npDef = 5 }
    , hits      = { quick = 6, arts = 2, buster = 1, ex = 5 }
    , traits    = [Female, EnumaElish]
    , death     = 56.8
    , align     = [Lawful, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 5)]
                  [(Piece Berserker, 12), (PhoenixFeather, 6)]
                  [(Monument Berserker, 5), (SeedOfYggdrasil, 12), (ClawOfChaos, 3)]
                  [(Monument Berserker, 12), (ClawOfChaos, 6), (HomunculusBaby, 12)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 5)]
                  [(GemOf Berserker, 12)]
                  [(MagicGemOf Berserker, 5)]
                  [(MagicGemOf Berserker, 12), (SeedOfYggdrasil, 6)]
                  [(SecretGemOf Berserker, 5), (SeedOfYggdrasil, 12)]
                  [(SecretGemOf Berserker, 12), (PhoenixFeather, 4)]
                  [(PhoenixFeather, 8), (TearstoneOfBlood, 4)]
                  [(TearstoneOfBlood, 11), (GhostLantern, 24)]
    }
  , { name      = "Heracles"
    , id        = 47
    , rarity    = 4
    , class     = Berserker
    , attr      = Heaven
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 9
    , stats     = { base =  { atk = 1775,  hp = 1652 }
                  , max =   { atk = 10655, hp = 10327 }
                  , grail = { atk = 12901, hp = 12521 }
                  }
    , skills    = [ { name  = "Valor"
                  , rank   = APlus
                  , icon    = IconSwordUp
                  , cd      = 7
                  , effect = [ Grant Self 3 AttackUp <| Range 10.5 31
                             , Grant Self 3 MentalResist <| Range 21 42
                             ]
                  }
                  , { name    = "Mind's Eye (Fake)"
                  , rank   = B
                  , icon    = IconDodge
                  , cd      = 8
                  , effect = [ Grant Self 1 Evasion Full
                             , Grant Self 3 CritUp <| Range 18 36
                             ]
                  }
                  , { name    = "Battle Continuation"
                  , rank   = A
                  , icon    = IconKneel
                  , cd      = 9
                  , effect = [ Times 1 << Grant Self 5 Guts <| Range 1000 2500 ]
                  }
                ]
    , passives  = [madness B, divinity A]
    , phantasm  = { name   = "Nine Lives"
                  , desc   = "Shooting Hundred Heads"
                  , rank   = APlus
                  , card   = Buster
                  , kind   = "Unknown"
                  , hits   = 15
                  , effect = [ To Enemy Damage <| Range 600 1000 ]
                  , over   = [ Debuff Enemy 3 DefenseDown <| Range 10 30 ]
                  , first  = False
                  }
    , gen       = { starWeight = 10, starRate = 5, npAtk = 1.07, npDef = 5 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Male, Divine, EnumaElish, GreekMythMale]
    , death     = 39
    , align     = [Chaotic, Mad]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 4)]
                  [(Piece Berserker, 10), (OctupletCrystals, 6)]
                  [(Monument Berserker, 4), (ClawOfChaos, 3), (HeartOfTheForeignGod, 4)]
                  [(Monument Berserker, 10), (ClawOfChaos, 5), (DragonsReverseScale, 4)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 4)]
                  [(GemOf Berserker, 10)]
                  [(MagicGemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 10), (HeartOfTheForeignGod, 2)]
                  [(SecretGemOf Berserker, 4), (HeartOfTheForeignGod, 4)]
                  [(SecretGemOf Berserker, 10), (OctupletCrystals, 4)]
                  [(ProofOfHero, 15), (OctupletCrystals, 8)]
                  [(ProofOfHero, 45), (DragonsReverseScale, 8)]
    }
  , { name      = "Frankenstein"
    , id        = 82
    , rarity    = 4
    , class     = Berserker
    , attr      = Earth
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 24
    , stats     = { base =  { atk = 1573,  hp = 1710 }
                  , max =   { atk = 9441, hp = 10687 }
                  , grail = { atk = 11431, hp = 12958 }
                  }
    , skills    = [ { name  = "Galvanism"
                  , rank   = B
                  , icon    = IconNobleUp
                  , cd      = 7
                  , effect = [ Grant Self 3 NPGen <| Range 25 45 ]
                  }
                  , { name    = "Wail of the Living Dead"
                  , rank   = C
                  , icon    = IconStun
                  , cd      = 8
                  , effect = [ Chance 60 <| Debuff Enemy 1 Stun Full
                             , Debuff Enemy 1 DefenseDown <| Range 20 30
                             ]
                  }
                  , { name    = "Overload"
                  , rank   = C
                  , icon    = IconBeamUp
                  , cd      = 7
                  , effect = [ Grant Self 1 NPUp <| Range 20 30
                             , Debuff Self 5 Burn <| Flat 300
                             ]
                  }
                ]
    , passives  = [madness D]
    , phantasm  = { name   = "Blasted Tree"
                  , desc   = "Lightning Tree of Crucifixion"
                  , rank   = BPlus
                  , card   = Quick
                  , kind   = "Anti-Army"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 900 1300
                             , Chance 500 <| Debuff Self 2 Stun Full
                             ]
                  , over   = [ Debuff Enemies 3 CritChance <| Range 20 40 ]
                  , first  = False
                  }
    , gen       = { starWeight = 10, starRate = 4.9, npAtk = 0.83, npDef = 5 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Female, EnumaElish]
    , death     = 58.5
    , align     = [Chaotic, Balanced]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 4)]
                  [(Piece Berserker, 10), (HomunculusBaby, 6)]
                  [(Monument Berserker, 4), (GhostLantern, 4), (EvilBone, 24)]
                  [(Monument Berserker, 10), (GhostLantern, 8), (EternalGear, 10)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 4)]
                  [(GemOf Berserker, 10)]
                  [(MagicGemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 10), (EvilBone, 12)]
                  [(SecretGemOf Berserker, 4), (EvilBone, 24)]
                  [(SecretGemOf Berserker, 10), (HomunculusBaby, 5)]
                  [(OctupletCrystals, 5), (HomunculusBaby, 8)]
                  [(OctupletCrystals, 15), (EternalGear, 20)]
    }
  , { name      = "Ibaraki-Douji"
    , id        = 116
    , rarity    = 4
    , class     = Berserker
    , attr      = Earth
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 29
    , stats     = { base =  { atk = 1606,  hp = 1752 }
                  , max =   { atk = 9636, hp = 10954 }
                  , grail = { atk = 11667, hp = 13282 }
                  }
    , skills    = [ { name  = "Demonic Nature of Oni"
                  , rank   = A
                  , icon    = IconSwordUp
                  , cd      = 7
                  , effect = [ Grant Party 3 AttackUp <| Range 10 20
                             , Grant Self 3 NPUp <| Range 20 30
                             ]
                  }
                  , { name    = "Disengage"
                  , rank   = A
                  , icon    = IconHeal
                  , cd      = 7
                  , effect = [ To Self RemoveDebuffs Full
                             , To Self Heal <| Range 1000 2500
                             ]
                  }
                  , { name    = "Morph"
                  , rank   = A
                  , icon    = IconShieldUp
                  , cd      = 7
                  , effect = [ Grant Self 3 DefenseUp <| Range 10 30
                             , Grant Self 1 DefenseUp <| Flat 30
                             ]
                  }
                ]
    , passives  = [madness B]
    , phantasm  = { name   = "Great Grudge of Rashomon"
                  , desc   = "Rashoumon Dai Engi"
                  , rank   = B
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 1
                  , effect = [ To Enemy Damage <| Range 600 1000
                             , To Enemy RemoveBuffs Full
                             ]
                  , over   = [ Debuff Enemy 3 DefenseDown <| Range 10 30 ]
                  , first  = False
                  }
    , gen       = { starWeight = 10, starRate = 4.9, npAtk = 1.03, npDef = 5 }
    , hits      = { quick = 4, arts = 2, buster = 1, ex = 5 }
    , traits    = [Female, Demonic, EnumaElish]
    , death     = 52
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 4)]
                  [(Piece Berserker, 10), (OctupletCrystals, 6)]
                  [(Monument Berserker, 4), (ClawOfChaos, 5), (PhoenixFeather, 4)]
                  [(Monument Berserker, 10), (PhoenixFeather, 7)
                  , (HeartOfTheForeignGod, 4)
                ]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 4)]
                  [(GemOf Berserker, 10)]
                  [(MagicGemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 10), (ClawOfChaos, 3)]
                  [(SecretGemOf Berserker, 4), (ClawOfChaos, 5)]
                  [(SecretGemOf Berserker, 10), (OctupletCrystals, 4)]
                  [(OctupletCrystals, 8), (VoidsDust, 10)]
                  [(VoidsDust, 30), (TearstoneOfBlood, 12)]

    }
  , { name      = "Lancelot"
    , id        = 48
    , rarity    = 4
    , class     = Berserker
    , attr      = Earth
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 14
    , stats     = { base =  { atk = 1746,  hp = 1652 }
                  , max =   { atk = 10477, hp = 10327 }
                  , grail = { atk = 12685, hp = 12521 }
                  }
    , skills    = [ { name  = "Eternal Arms Mastery"
                  , rank   = APlus
                  , icon    = IconStarUp
                  , cd      = 7
                  , effect = [ Grant Self 3 StarAbsorb <| Range 3000 6000 ]
                  }
                  , { name    = "Protection of the Spirits"
                  , rank   = A
                  , icon    = IconStarHaloUp
                  , cd      = 7
                  , effect = [ Grant Self 3 StarUp <| Range 10 30 ]
                  }
                  , { name    = "Mana Reversal"
                  , rank   = A
                  , icon    = IconNobleUp
                  , cd      = 7
                  , effect = [ Grant Self 1 NPGen <| Range 50 100
                             , Grant Self 3 CritUp <| Range 30 50
                             ]
                  }
                ]
    , passives  = [magicResistance E, madness C]
    , phantasm  = { name   = "Knight of Owner"
                  , desc   = "A Knight Does Not Die with Empty Hands"
                  , rank   = APlusPlus
                  , card   = Quick
                  , kind   = "Anti-Personnel"
                  , hits   = 10
                  , effect = [ To Enemies Damage <| Range 600 1000 ]
                  , over   = [ Grant Self 3 AttackUp <| Range 10 30 ]
                  , first  = True
                  }
    , gen       = { starWeight = 10, starRate = 5, npAtk = 0.5, npDef = 5 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Male, EnumaElish]
    , death     = 52
    , align     = [Lawful, Mad]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 4)]
                  [(Piece Berserker, 10), (DragonsReverseScale, 3)]
                  [(Monument Berserker, 4), (ClawOfChaos, 5), (VoidsDust, 8)]
                  [(Monument Berserker, 10), (DragonFang, 24), (VoidsDust, 16)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 4)]
                  [(GemOf Berserker, 10)]
                  [(MagicGemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 10), (ClawOfChaos, 3)]
                  [(SecretGemOf Berserker, 4), (ClawOfChaos, 5)]
                  [(SecretGemOf Berserker, 10), (DragonsReverseScale, 2)]
                  [(MeteorHorseshoe, 5), (DragonsReverseScale, 4)]
                  [(MeteorHorseshoe, 15), (DragonFang, 48)]
    }
  , { name      = "Beowulf"
    , id        = 89
    , rarity    = 4
    , class     = Berserker
    , attr      = Earth
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 4
    , stats     = { base =  { atk = 1707,  hp = 1652 }
                  , max =   { atk = 10247, hp = 10327 }
                  , grail = { atk = 12407, hp = 12521 }
                  }
    , skills    = [ { name  = "Berserk"
                  , rank   = A
                  , icon    = IconSwordUp
                  , cd      = 7
                  , effect = [ Grant Self 1 AttackUp <| Range 20 30
                             , Grant Self 1 NPUp <| Range 10 20
                             ]
                  }
                  , { name    = "Intuition"
                  , rank   = B
                  , icon    = IconStar
                  , cd      = 7
                  , effect = [ To Party GainStars <| Range 4 14 ]
                  }
                  , { name    = "Battle Continuation"
                  , rank   = B
                  , icon    = IconKneel
                  , cd      = 9
                  , effect = [ Times 1 << Grant Self 4 Guts <| Range 750 2000 ]
                  }
                ]
    , passives  = [madness EMinus]
    , phantasm  = { name   = "Grendel Buster"
                  , desc   = "Conflict Wellspring"
                  , rank   = APlus
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 12
                  , effect = [ Grant Self 1 SureHit Full
                             , To Enemy Damage <| Range 700 1100
                             ]
                  , over   = [ Debuff Enemy 3 CritChance <| Range 30 50 ]
                  , first  = False
                  }
    , gen       = { starWeight = 10, starRate = 4.9, npAtk = 0.68, npDef = 5 }
    , hits      = { quick = 3, arts = 3, buster = 1, ex = 4 }
    , traits    = [Male, EnumaElish, Brynhild, King]
    , death     = 58.5
    , align     = [Chaotic, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 4)]
                  [(Piece Berserker, 10), (ProofOfHero, 18)]
                  [( Monument Berserker, 4), (DragonsReverseScale, 2)
                  , (OctupletCrystals, 8)
                ]
                  [(Monument Berserker, 10), (DragonsReverseScale, 4), (EvilBone, 30)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 4)]
                  [(GemOf Berserker, 10)]
                  [(MagicGemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 10), (OctupletCrystals, 4)]
                  [(SecretGemOf Berserker, 4), (OctupletCrystals, 8)]
                  [(SecretGemOf Berserker, 10), (ProofOfHero, 12)]
                  [(SeedOfYggdrasil, 6), (ProofOfHero, 24)]
                  [(SeedOfYggdrasil, 18), (DragonFang, 48)]
    }
  , { name      = "Tamamo Cat"
    , id        = 58
    , rarity    = 4
    , class     = Berserker
    , attr      = Earth
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 9
    , stats     = { base =  { atk = 1504,  hp = 1833 }
                  , max =   { atk = 9026, hp = 11458 }
                  , grail = { atk = 10929, hp = 13893 }
                  }
    , skills    = [ { name  = "Monstrous Strength"
                  , rank   = B
                  , icon    = IconSwordUp
                  , cd      = 7
                  , effect = [ Grant Self 2 AttackUp <| Range 10 30 ]
                  }
                  , { name    = "Curse"
                  , rank   = E
                  , icon    = IconDarkMagic
                  , cd      = 7
                  , effect = [ Chances 40 60 << To Enemy GaugeDown <| Flat 1 ]
                  }
                  , { name    = "Morph"
                  , rank   = B
                  , icon    = IconShieldUp
                  , cd      = 7
                  , effect = [ Grant Self 3 DefenseUp <| Range 18 27 ]
                  }
                ]
    , passives  = [madness C]
    , phantasm  = { name   = "Napping in the Dazzling Sunshine and Feasting"
                  , desc   = "Opulence of Sun and Shine and Catnap"
                  , rank   = D
                  , card   = Quick
                  , kind   = "Anti-Personnel"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 600 1000
                             , Chance 500 <| Debuff Self 2 Stun Full
                             ]
                  , over   = [ Grant Self 3 HealPerTurn <| Range 2000 6000 ]
                  , first  = False
                  }
    , gen       = { starWeight = 10, starRate = 5, npAtk = 0.71, npDef = 5 }
    , hits      = { quick = 2, arts = 3, buster = 2, ex = 3 }
    , traits    = [Female, Beast, EnumaElish]
    , death     = 39
    , align     = [Chaotic, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 4)]
                  [(Piece Berserker, 10), (GhostLantern, 6)]
                  [(Monument Berserker, 4), (HomunculusBaby, 8), (ClawOfChaos, 3)]
                  [(Monument Berserker, 10), (ClawOfChaos, 5), (HeartOfTheForeignGod, 4)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 4)]
                  [(GemOf Berserker, 10)]
                  [(MagicGemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 10), (HomunculusBaby, 4)]
                  [(SecretGemOf Berserker, 4), (HomunculusBaby, 8)]
                  [(SecretGemOf Berserker, 10), (GhostLantern, 4)]
                  [(GhostLantern, 8), (OctupletCrystals, 5)]
                  [(OctupletCrystals, 15), (HeartOfTheForeignGod, 8)]
    }
  , { name      = "Lu Bu Fengxian"
    , id        = 49
    , rarity    = 3
    , class     = Berserker
    , attr      = Mankind
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 8
    , stats     = { base =  { atk = 1507,  hp = 1494 }
                  , max =   { atk = 8119, hp = 8302 }
                  , grail = { atk = 10988, hp = 11256 }
                  }
    , skills    = [ { name  = "Valor"
                  , rank   = B
                  , icon    = IconSwordUp
                  , cd      = 7
                  , effect = [ Grant Self 3 AttackUp <| Range 9 27
                             , Grant Self 3 MentalResist <| Range 18 36
                             ]
                  }
                  , { name    = "Defiant"
                  , rank   = B
                  , icon    = IconSwordUp
                  , cd      = 7
                  , effect = [ Grant Self 3 DefenseUp <| Range 15 25
                             , Debuff Self 3 BuffFail <| Flat 50
                             ]
                  }
                  , { name    = "Chaotic Villain"
                  , rank   = A
                  , icon    = IconBeamUp
                  , cd      = 8
                  , effect = [ Grant Self 1 NPUp <| Range 20 30
                             , Grant Self 1 StarAbsorb <| Flat 3000
                             , Debuff Others 1 DefenseDown <| Flat 20
                             ]
                  }
                ]
    , passives  = [madness A]
    , phantasm  = { name   = "God Force"
                  , desc   = "Five Weapons of the War God"
                  , rank   = A
                  , card   = Buster
                  , kind   = "Anti-Personnel/Anti-Army/Anti-Fortress"
                  , hits   = 1
                  , effect = [ To Enemy DamageThruDef <| Range 600 1000 ]
                  , over   = [ Chances 30 70 <| Debuff Enemy 1 Stun Full ]
                  , first  = False
                  }
    , gen       = { starWeight = 9, starRate = 5, npAtk = 1.04, npDef = 5 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Male, EnumaElish]
    , death     = 50.3
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 4)]
                  [(Piece Berserker, 8), (MeteorHorseshoe, 5)]
                  [(Monument Berserker, 4), (ClawOfChaos, 2), (EvilBone, 20)]
                  [(Monument Berserker, 8), (ClawOfChaos, 4), (VoidsDust, 16)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 4)]
                  [(GemOf Berserker, 8)]
                  [(MagicGemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 8), (EvilBone, 10)]
                  [(SecretGemOf Berserker, 4), (EvilBone, 20)]
                  [(SecretGemOf Berserker, 8), (MeteorHorseshoe, 4)]
                  [(OctupletCrystals, 4), (MeteorHorseshoe, 7)]
                  [(OctupletCrystals, 12), (VoidsDust, 32)]
    }
  , { name      = "Spartacus"
    , id        = 50
    , rarity    = 1
    , class     = Berserker
    , attr      = Mankind
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 6
    , stats     = { base =  { atk = 922,  hp = 1544 }
                  , max =   { atk = 5073, hp = 7722 }
                  , grail = { atk = 7883, hp = 11904 }
                  }
    , skills    = [ { name  = "Honor of Suffering"
                  , rank   = BPlus
                  , icon    = IconHealTurn
                  , cd      = 9
                  , effect = [ Grant Self 5 HealPerTurn <| Range 500 1500 ]
                  }
                  , { name    = "Unyielding Will"
                  , rank   = A
                  , icon    = IconKneel
                  , cd      = 7
                  , effect = [ Times 1 << Grant Self 5 Guts <| Flat 1
                             , To Self GaugeUp <| Range 10 30
                             ]
                  }
                  , { name    = "Triumphant Return of the Sword"
                  , rank   = B
                  , icon    = IconBusterUp
                  , cd      = 8
                  , effect = [ Grant Self 1 (Performance Buster) <| Range 20 40
                             , To Self Heal <| Range 1000 2000
                             ]
                  }
                ]
    , passives  = [madness EX]
    , phantasm  = { name   = "Crying Warmonger"
                  , desc   = "Howl of the Wounded Beast"
                  , rank   = A
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 1
                  , effect = [ To Enemies DamageThruDef <| Range 300 500 ]
                  , over   = [ To Self Heal <| Range 3000 7000 ]
                  , first  = False
                  }
    , gen       = { starWeight = 9, starRate = 4.9, npAtk = 1.01, npDef = 5 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Male, Roman, EnumaElish]
    , death     = 65
    , align     = [Neutral, Balanced]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 2)]
                  [(Piece Berserker, 4), (OctupletCrystals, 3)]
                  [(Monument Berserker, 2), (HomunculusBaby, 2), (ProofOfHero, 10)]
                  [(Monument Berserker, 4), (HomunculusBaby, 4), (EvilBone, 12)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 2)]
                  [(GemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 2)]
                  [(MagicGemOf Berserker, 4), (ProofOfHero, 5)]
                  [(SecretGemOf Berserker, 2), (ProofOfHero, 10)]
                  [(SecretGemOf Berserker, 4), (OctupletCrystals, 2)]
                  [(SeedOfYggdrasil, 3), (OctupletCrystals, 4)]
                  [(SeedOfYggdrasil, 8), (EvilBone, 24)]
    }
  , { name      = "Asterios"
    , id        = 53
    , rarity    = 1
    , class     = Berserker
    , attr      = Earth
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 1
    , stats     = { base =  { atk = 1097,  hp = 1320 }
                  , max =   { atk = 6037, hp = 6604 }
                  , grail = { atk = 9381, hp = 10181 }
                  }
    , skills    = [ { name  = "Monstrous Strength"
                  , rank   = A
                  , icon    = IconSwordUp
                  , cd      = 7
                  , effect = [ Grant Self 3 AttackUp <| Range 10 30 ]
                  }
                  , { name    = "Natural Demon"
                  , rank   = APlusPlus
                  , icon    = IconHoodUp
                  , cd      = 7
                  , effect = [ Grant Self 3 OffensiveResist <| Range 50 100
                             , Grant Self 3 DefenseUp <| Range 20 40
                             ]
                  }
                  , { name    = "Labrys of the Abyss"
                  , rank   = C
                  , icon    = IconStarUp
                  , cd      = 7
                  , effect = [ Grant Self 1 StarAbsorb <| Range 3000 6000
                             , Grant Self 1 (Performance Buster) <| Range 20 30
                             ]
                  }
                ]
    , passives  = [madness B]
    , phantasm  = { name   = "Chaos Labyrinth"
                  , desc   = "Eternally Unchanging Labyrinth"
                  , rank   = EX
                  , card   = Arts
                  , kind   = "Maze"
                  , hits   = 0
                  , effect = [ Debuff Enemies 6 AttackDown <| Range 10 20
                             , Debuff Enemies 1 AttackDown <| Flat 40
                             , Debuff Enemies 1 DefenseDown <| Flat 40
                             ]
                  , over   = [ Debuff Enemies 6 DefenseDown <| Range 10 30 ]
                  , first  = False
                  }
    , gen       = { starWeight = 9, starRate = 4.9, npAtk = 0.68, npDef = 5 }
    , hits      = { quick = 2, arts = 3, buster = 1, ex = 3 }
    , traits    = [Male, EnumaElish, GreekMythMale]
    , death     = 58.5
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 2)]
                  [(OctupletCrystals, 3), (Piece Berserker, 4)]
                  [(MeteorHorseshoe, 2), (ProofOfHero, 10), (Monument Berserker, 2)]
                  [(MeteorHorseshoe, 4), (SeedOfYggdrasil, 5), (Monument Berserker, 4)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 2)]
                  [(GemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 2)]
                  [(MagicGemOf Berserker, 4), (ProofOfHero, 5)]
                  [(SecretGemOf Berserker, 2), (ProofOfHero, 10)]
                  [(SecretGemOf Berserker, 4), (OctupletCrystals, 2)]
                  [(VoidsDust, 4), (OctupletCrystals, 4)]
                  [(SeedOfYggdrasil, 10), (VoidsDust, 12)]
    }
  , { name      = "Kiyohime"
    , id        = 56
    , rarity    = 3
    , class     = Berserker
    , attr      = Earth
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 3
    , stats     = { base =  { atk = 1233,  hp = 1649 }
                  , max =   { atk = 6644, hp = 9166 }
                  , grail = { atk = 8992, hp = 12428 }
                  }
    , skills    = [ { name  = "Morph"
                  , rank   = C
                  , icon    = IconShieldUp
                  , cd      = 7
                  , effect = [ Grant Self 3 DefenseUp <| Range 16 24 ]
                  }
                  , { name    = "Stalking"
                  , rank   = B
                  , icon    = IconShieldDown
                  , cd      = 7
                  , effect = [ Debuff Enemy 4 DefenseDown <| Range 12 24
                             , Grant Enemy 3 AttackUp <| Flat 20
                             ]
                  }
                  , { name    = "Flame-Coloured Kiss"
                  , rank   = A
                  , icon    = IconBusterUp
                  , cd      = 7
                  , effect = [ Grant Self 1 (Performance Buster) <| Range 20 30
                             , To Self RemoveDebuffs Full
                             ]
                  }
                ]
    , passives  = [madness EX]
    , phantasm  = { name   = "Transforming, Flame-Emitting Meditation"
                  , desc   = "Tenshin Kasyou Zanmai Achieving Acala's Samadhi"
                  , rank   = EX
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 3
                  , effect = [ To Enemies Damage <| Range 300 500 ]
                  , over   = [ Chances 50 80 <| Debuff Enemies 1 Stun Full
                             , Debuff Enemies 10 Burn <| Range 500 900
                             ]
                  , first  = False
                  }
    , gen       = { starWeight = 9, starRate = 4.9, npAtk = 2.03, npDef = 5 }
    , hits      = { quick = 2, arts = 1, buster = 1, ex = 3 }
    , traits    = [Female, Dragon, EnumaElish]
    , death     = 65
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = True
    , ascendUp  = Ascension
                  [(Piece Berserker, 4)]
                  [(Piece Berserker, 8), (GhostLantern, 5)]
                  [(Monument Berserker, 4), (EvilBone, 20), (DragonFang, 8)]
                  [(Monument Berserker, 8), (DragonsReverseScale, 4), (DragonFang, 16)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 4)]
                  [(GemOf Berserker, 8)]
                  [(MagicGemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 8), (EvilBone, 10)]
                  [(SecretGemOf Berserker, 4), (EvilBone, 20)]
                  [(SecretGemOf Berserker, 8), (GhostLantern, 4)]
                  [(SeedOfYggdrasil, 5), (GhostLantern, 7)]
                  [(SeedOfYggdrasil, 15), (DragonsReverseScale, 7)]
    }
  , { name      = "Eric Bloodaxe"
    , id        = 57
    , rarity    = 2
    , class     = Berserker
    , attr      = Mankind
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 2
    , stats     = { base =  { atk = 1116,  hp = 1447 }
                  , max =   { atk = 6290, hp = 7688 }
                  , grail = { atk = 9115, hp = 11095 }
                  }
    , skills    = [ { name  = "Supporting Curse"
                  , rank   = CPlus
                  , icon    = IconSwordDown
                  , cd      = 7
                  , effect = [ Debuff Enemy 2 AttackDown <| Range 5 15
                             , Debuff Enemy 2 DefenseDown <| Range 10 30
                             ]
                  }
                  , { name    = "Battle Continuation"
                  , rank   = B
                  , icon    = IconKneel
                  , cd      = 9
                  , effect = [ Times 1 << Grant Self 4 Guts <| Range 750 2000 ]
                  }
                  , { name    = "Half-Dead Bloodaxe"
                  , rank   = APlus
                  , icon    = IconBubbles
                  , cd      = 8
                  , effect = [ To Self RemoveDebuffs Full
                             , Grant Self 3 MaxHP <| Range 1000 3000
                             ]
                  }
                ]
    , passives  = [madness B]
    , phantasm  = { name   = "Bloodbath Crown"
                  , desc   = "Bloody Coronation"
                  , rank   = B
                  , card   = Buster
                  , kind   = "Anti-Personnel"
                  , hits   = 5
                  , effect = [ To Enemies Damage <| Range 300 500
                             , To Self DemeritDamage <| Flat 1000
                             ]
                  , over   = [ Grant Self 1 AttackUp <| Range 30 50 ]
                  , first  = True
                  }
    , gen       = { starWeight = 9, starRate = 4.9, npAtk = 1.02, npDef = 5 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Male, EnumaElish, King]
    , death     = 58.5
    , align     = [Chaotic, Balanced]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 3)]
                  [(Piece Berserker, 6), (EvilBone, 11)]
                  [(Monument Berserker, 3), (SerpentJewel, 2), (HomunculusBaby, 5)]
                  [(Monument Berserker, 6), (SerpentJewel, 4), (VoidsDust, 12)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 3)]
                  [(GemOf Berserker, 6)]
                  [(MagicGemOf Berserker, 3)]
                  [(MagicGemOf Berserker, 6), (HomunculusBaby, 3)]
                  [(SecretGemOf Berserker, 3), (HomunculusBaby, 5)]
                  [(SecretGemOf Berserker, 6), (EvilBone, 8)]
                  [(ClawOfChaos, 2), (EvilBone, 15)]
                  [(ClawOfChaos, 6), (VoidsDust, 24)]
    }
  , { name      = "Darius III"
    , id        = 55
    , rarity    = 3
    , class     = Berserker
    , attr      = Mankind
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 3
    , stats     = { base =  { atk = 1412,  hp = 1577 }
                  , max =   { atk = 7608, hp = 8763 }
                  , grail = { atk = 10297, hp = 11881 }
                  }
    , skills    = [ { name  = "Golden Rule"
                  , rank   = B
                  , icon    = IconNobleUp
                  , cd      = 8
                  , effect = [ Grant Self 3 NPGen <| Range 18 45 ]
                  }
                  , { name    = "Disengage"
                  , rank   = A
                  , icon    = IconHeal
                  , cd      = 7
                  , effect = [ To Self RemoveDebuffs Full
                             , To Self Heal <| Range 1000 2500
                             ]
                  }
                  , { name    = "Battle Continuation"
                  , rank   = A
                  , icon    = IconKneel
                  , cd      = 9
                  , effect = [ Times 1 << Grant Self 5 Guts <| Range 1000 2500 ]
                  }
                ]
    , passives  = [madness B]
    , phantasm  = { name   = "Athanaton Ten Thousand"
                  , desc   = "Immortal Cavalry of Ten Thousand Rank"
                  , rank   = A
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 10
                  , effect = [ To Enemies Damage <| Range 300 500 ]
                  , over   = [ Debuff Enemies 3 AttackDown <| Range 10 30
                             , Debuff Enemies 3 DefenseDown <| Range 10 30
                             ]
                  , first  = False
                  }
    , gen       = { starWeight = 9, starRate = 5, npAtk = 0.67, npDef = 5 }
    , hits      = { quick = 2, arts = 3, buster = 1, ex = 3 }
    , traits    = [Male, EnumaElish, King]
    , death     = 65
    , align     = [Lawful, Balanced]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 4)]
                  [(Piece Berserker, 8), (OctupletCrystals, 5)]
                  [(Monument Berserker, 4), (EvilBone, 10), (PhoenixFeather, 6)]
                  [(Monument Berserker, 8), (EvilBone, 20), (MeteorHorseshoe, 8)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 4)]
                  [(GemOf Berserker, 8)]
                  [(MagicGemOf Berserker, 4)]
                  [(MagicGemOf Berserker, 8), (PhoenixFeather, 3)]
                  [(SecretGemOf Berserker, 4), (PhoenixFeather, 6)]
                  [(SecretGemOf Berserker, 8), (OctupletCrystals, 4)]
                  [(VoidsDust, 8), (OctupletCrystals, 7)]
                  [(VoidsDust, 24), (MeteorHorseshoe, 16)]
    }
  , { name      = "Caligula"
    , id        = 54
    , rarity    = 2
    , class     = Berserker
    , attr      = Mankind
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 12
    , stats     = { base =  { atk = 1374,  hp = 1211 }
                  , max =   { atk = 6831, hp = 7303 }
                  , grail = { atk = 9899, hp = 10540 }
                  }
    , skills    = [ { name  = "Sadistic Streak"
                  , rank   = A
                  , icon    = IconSwordUp
                  , cd      = 7
                  , effect = [ Grant Self 3 AttackUp <| Range 10 30
                             , Debuff Self 3 DefenseDown <| Flat 10
                             ]
                  }
                  , { name    = "Imperial Privilege"
                  , rank   = A
                  , icon    = IconHeal
                  , cd      = 7
                  , effect = [ To Self Heal <| Range 1000 3000
                             , Chance 60 << Grant Self 3 AttackUp <| Range 20 40
                             , Chance 60 << Grant Self 3 DefenseUp <| Range 20 40
                             ]
                  }
                  , { name    = "Glory of Past Days"
                  , rank   = B
                  , icon    = IconBusterUp
                  , cd      = 5
                  , effect = [ Grant Self 1 (Performance Buster) <| Range 30 50
                             , To Self DemeritHealth <| Flat 500
                             ]
                  }
                ]
    , passives  = [madness APlus]
    , phantasm  = { name   = "Flucticulus Diana"
                  , desc   = "Devour my Heart, Moonlight"
                  , rank   = C
                  , card   = Arts
                  , kind   = "Anti-Army"
                  , hits   = 0
                  , effect = [ Chances 100 150 <| Debuff Enemies 3 SealSkills Full ]
                  , over   = [ Chances 70 90 <| Debuff Enemies 3 SealNP Full ]
                  , first  = False
                  }
    , gen       = { starWeight = 9, starRate = 5, npAtk = 0.68, npDef = 5 }
    , hits      = { quick = 2, arts = 3, buster = 1, ex = 3 }
    , traits    = [Male, Roman, EnumaElish, King]
    , death     = 56.8
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(Piece Berserker, 3)]
                  [(Piece Berserker, 6), (OctupletCrystals, 4)]
                  [(Monument Berserker, 3), (GhostLantern, 3), (SerpentJewel, 4)]
                  [(Monument Berserker, 6), (GhostLantern, 5), (ForbiddenPage, 6)]
    , skillUp   = Reinforcement
                  [(GemOf Berserker, 3)]
                  [(GemOf Berserker, 6)]
                  [(MagicGemOf Berserker, 3)]
                  [(MagicGemOf Berserker, 6), (SerpentJewel, 2)]
                  [(SecretGemOf Berserker, 3), (SerpentJewel, 4)]
                  [(SecretGemOf Berserker, 6), (OctupletCrystals, 3)]
                  [(MeteorHorseshoe, 3), (OctupletCrystals, 5)]
                  [(MeteorHorseshoe, 9), (ForbiddenPage, 12)]
    }
  ]
