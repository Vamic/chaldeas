module Database.Servant.Extra exposing (extras)

import Database.Base exposing (..)
import Database.Passive exposing (..)
import Database.Servant exposing (..)
import Database.Skill exposing (..)

extras : List Servant
extras = 
  [ { name  =   "Jeanne d'Arc"
    , id        = 59
    , rarity    = 5
    , class     = Ruler
    , attr      = Star
    , deck      = Deck Quick Arts Arts Arts Buster
    , curve     = 10
    , stats     = { base  = { atk = 1482,  hp = 2420 }
                  , max   = { atk = 9593,  hp = 16500 }
                  , grail = { atk = 10501, hp = 18076 }
                  }
    , skills    = [ { name   = "Revelation"
                    , rank   = A
                    , icon   = IconStarTurn
                    , cd     = 8
                    , effect = [ Grant Party 3 StarsPerTurn <| Range 3 9 ]
                    }
                  , { name   = "True Name Revelation"
                    , rank   = B
                    , icon   = IconBeamDown
                    , cd     = 7
                    , effect = [ Debuff Enemy 1 NPDown <| Range 15 30 ]
                    }
                  , { name   = "Divine Judgement"
                    , rank   = A
                    , icon   = IconStun
                    , cd     = 8
                  -- TODO only affects [Servant]s?
                    , effect = [ Chances 70 120 <| Debuff Enemy 1 Stun Full ]
                    }
                  ]
    , passives  = [magicResistance EX]
    , phantasm  = { name   = "Luminosité Eternelle"
                  , desc   = "God is Here With Me"
                  , rank   = AMinus
                  , card   = Arts
                  , kind   = "Barrier"
                  , hits   = 0
                  , effect = [ Grant Party 3 DefenseUp <| Range 5 25
                             , Grant Party 1 Invincibility Full
                             , Debuff Self 2 Stun Full
                             ]
                  , over   = [ Grant Party 2 HealPerTurn <| Range 1000 3000 ]
                  , first  = False
                  }
    , gen       = { starWeight = 99, starRate = 10.1, npAtk = 0.76, npDef = 3 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Female, EnumaElish, Saberface]
    , death     = 21
    , align     = [Lawful, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  (pairWith 5 Piece    [Saber, Archer, Lancer])
                  (pairWith 5 Piece    [Rider, Caster, Assassin, Berserker])
                  (pairWith 5 Monument [Saber, Archer, Lancer])
                  (pairWith 5 Monument [Rider, Caster, Assassin, Berserker])
    , skillUp   = Reinforcement
                  (pairWith 5 GemOf       [Saber, Archer, Lancer])
                  (pairWith 5 GemOf       [Rider, Caster, Assassin, Berserker])
                  (pairWith 5 MagicGemOf  [Saber, Archer, Lancer])
                  (pairWith 5 MagicGemOf  [Rider, Caster, Assassin, Berserker])
                  (pairWith 5 SecretGemOf [Saber, Archer, Lancer])
                  (pairWith 5 SecretGemOf [Rider, Caster, Assassin, Berserker])
                  [(HeartOfTheForeignGod, 5)]
                  [(HeartOfTheForeignGod, 15)]
    }
  , { name      = "Amakusa Shirou"
    , id        = 93
    , rarity    = 5
    , class     = Ruler
    , attr      = Mankind
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 10
    , stats     = { base  = { atk = 1695,  hp = 2069 }
                  , max   = { atk = 10972, hp = 14107 }
                  , grail = { atk = 12011, hp = 15455 }
                  }
    , skills    = [ { name   = "Revelation"
                    , rank   = A
                    , icon   = IconStarTurn
                    , cd     = 8
                    , effect = [ Grant Party 3 StarsPerTurn <| Range 3 9 ]
                    }
                  , { name   = "Baptism Rite"
                    , rank   = BPlus
                    , icon   = IconNobleTurn
                    , cd     = 12
                    , effect = [ Grant Self 5 GaugePerTurn <| Range 10 20
                               , To (EnemyType Undead) GaugeDown <| Flat 1
                               , To (EnemyType Demon) GaugeDown <| Flat 1
                               ]
                    }
                  , { name   = "Divine Judgement"
                    , rank   = C
                    , icon   = IconStun
                    , cd     = 8
                  -- TODO only affects [Servant]s?
                    , effect = [ Chances 50 100 <| Debuff Enemy 1 Stun Full ]
                    }
                  ]
    , passives  = [magicResistance A]
    , phantasm  = { name   = "Twin Arm—Big Crunch"
                  , desc   = "Dual Arm, Zero-Order Convergence"
                  , rank   = APlus
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 1
                  , effect = [ To Enemies RemoveBuffs Full
                             , To Enemies Damage <| Range 300 500
                             ]
                  , over   = [ Debuff Enemies 1 CritChance <| Range 30 70 ]
                  , first  = False
                  }
    , gen       = { starWeight = 100, starRate = 10, npAtk = 0.86, npDef = 3 }
    , hits      = { quick = 3, arts = 2, buster = 1, ex = 7 }
    , traits    = [Male, EnumaElish]
    , death     = 21
    , align     = [Lawful, Good]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  (pairWith 5 Piece    [Lancer, Caster, Assassin])
                  (pairWith 5 Piece    [Saber, Archer, Rider, Berserker])
                  (pairWith 5 Monument [Rider, Caster, Assassin])
                  (pairWith 5 Monument [Saber, Archer, Lancer, Berserker])
    , skillUp   = Reinforcement
                  (pairWith 5 GemOf       [Rider, Caster, Assassin])
                  (pairWith 5 GemOf       [Saber, Lancer, Archer, Berserker])
                  (pairWith 5 MagicGemOf  [Rider, Caster, Assassin])
                  (pairWith 5 MagicGemOf  [Saber, Lancer, Archer, Berserker])
                  (pairWith 5 SecretGemOf [Rider, Caster, Assassin])
                  (pairWith 5 SecretGemOf [Saber, Lancer, Archer, Berserker])
                  [(HeartOfTheForeignGod, 5)]
                  [(HeartOfTheForeignGod, 15)]
    }
  , { name      = "Jeanne d'Arc (Alter)"
    , id        = 106
    , rarity    = 5
    , class     = Avenger
    , attr      = Mankind
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 15
    , stats     = { base  = { atk = 2046,  hp = 1724 }
                  , max   = { atk = 13244, hp = 11761 }
                  , grail = { atk = 14498, hp = 12885 }
                  }
    , skills    = [ { name   = "Self-Modification"
                    , rank   = EX
                    , icon   = IconExclamationUp
                    , cd     = 7
                    , effect = [ Grant Self 3 CritUp <| Range 20 50
                               , Grant Self 3 StarAbsorb <| Range 400 800
                               ]
                    }
                  , { name   = "Dragon Witch"
                    , rank   = EX
                    , icon   = IconSwordUp
                    , cd     = 7
                    , effect = [ Grant Party 3 AttackUp <| Range 10 20
                               , Grant (AlliesType Dragon) 3 AttackUp <| Range 10 20
                               ]
                    }
                  , { name   = "Ephemeral Dream"
                    , rank   = A
                    , icon   = IconBusterUp
                    , cd     = 8
                    , effect = [ Grant Self 1 (Performance Buster) <| Range 30 50
                               , Grant Self 1 Invincibility Full
                               , To Self DemeritHealth <| Flat 1000
                               ]
                    }
                  ]
    , passives  = [avenger B, oblivionCorrection A, selfRestoreMagic APlus]
    , phantasm  = { name   = "La Grondement Du Haine"
                  , desc   = "Howl Loudly, My Resentment"
                  , rank   = APlus
                  , card   = Buster
                  , kind   = "Anti-Army"
                  , hits   = 10
                  , effect = [ To Enemy Damage <| Range 600 1000
                             , Times 1 <| Debuff Enemy 0 BuffBlock Full
                             ]
                  , over   = [ Debuff Enemy 5 Curse <| Range 500 2500 ]
                  , first  = False
                  }
    , gen       = { starWeight = 29, starRate = 6, npAtk = 0.83, npDef = 5 }
    , hits      = { quick = 3, arts = 2, buster = 4, ex = 7 }
    , traits    = [Female, EnumaElish, Saberface]
    , death     = 5.7
    , align     = [Chaotic, Evil]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  [(ProofOfHero, 10), (VoidsDust, 10)]
                  [(OctupletCrystals, 10), (EternalGear, 10)]
                  [(PhoenixFeather, 10), (ClawOfChaos, 10)]
                  [(TearstoneOfBlood, 10), (HeartOfTheForeignGod, 10)]
    , skillUp   = Reinforcement
                  [(DragonFang, 10)]
                  [(EvilBone, 10)]
                  [(SeedOfYggdrasil, 12)]
                  [(ForbiddenPage, 12)]
                  [(SerpentJewel, 12)]
                  [(GhostLantern, 15)]
                  [(WarhorsesYoungHorn, 15)]
                  [(HeartOfTheForeignGod, 15)]
    }
  , { name      = "Edmond Dantes"
    , id        = 96
    , rarity    = 5
    , class     = Avenger
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Buster Buster
    , curve     = 15
    , stats     = { base  = { atk = 1953,  hp = 1785 }
                  , max   = { atk = 12641, hp = 12177 }
                  , grail = { atk = 13838, hp = 13340 }
                  }
    , skills    = [ { name   = "Iron Determination"
                    , rank   = EX
                    , icon   = IconShieldBreak
                    , cd     = 8
                    , effect = [ Grant Self 1 IgnoreInvinc Full
                               , Grant Self 1 AttackUp <| Range 30 50
                               , Grant Self 3 DebuffResist <| Range 14 32
                               ]
                    }
                  , { name   = "Golden Rule"
                    , rank   = A
                    , icon   = IconNobleUp
                    , cd     = 8
                    , effect = [ Grant Self 3 NPGen <| Range 20 50 ]
                    }
                  , { name   = "Wisdom of Crisis"
                    , rank   = A
                    , icon   = IconDarkMagic
                    , cd     = 8
                    , effect = [ To Enemy GaugeDown <| Flat 1
                               , To Self RemoveDebuffs Full
                               , To Party GainStars <| Range 10 20
                               ]
                    }
                  ]
    , passives  = [avenger A, oblivionCorrection B, selfRestoreMagic D]
    , phantasm  = { name   = "Enfer Château d'If"
                  , desc   = "Tyger, Tyger, Burning Bright"
                  , rank   = A
                  , card   = Quick
                  , kind   = "Anti-Army"
                  , hits   = 8
                  , effect = [ To Enemies Damage <| Range 600 1000 ]
                  , over   = [ Debuff Enemies 3 DefenseDown <|  Range 20 40
                             , Debuff Enemies 5 Curse <| Range 500 1500
                             ]
                  , first  = False
                  }
    , gen       = { starWeight = 30, starRate = 5.9, npAtk = 0.62, npDef = 6 }
    , hits      = { quick = 4, arts = 2, buster = 3, ex = 5 }
    , traits    = [Male, EnumaElish]
    , death     = 7
    , align     = [Chaotic, Evil]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  [(EvilBone, 10), (OctupletCrystals, 10)]
                  [(SerpentJewel, 10), (ForbiddenPage, 10)]
                  [(VoidsDust, 10), (EternalGear, 10)]
                  [(PhoenixFeather, 10), (HeartOfTheForeignGod, 10)]
    , skillUp   = Reinforcement
                  [(ProofOfHero, 10)]
                  [(SeedOfYggdrasil, 10)]
                  [(DragonFang, 12)]
                  [(GhostLantern, 12)]
                  [(MeteorHorseshoe, 12)]
                  [(HomunculusBaby, 15)]
                  [(ClawOfChaos, 15)]
                  [(DragonsReverseScale, 15)]
    }
  , { name      = "Martha (Ruler)"
    , id        = 135
    , rarity    = 4
    , class     = Ruler
    , attr      = Mankind
    , deck      = Deck Quick Arts Buster Buster Buster
    , curve     = 4
    , stats     = { base  = { atk = 1591,  hp = 1800 }
                  , max   = { atk = 9546,  hp = 11250 }
                  , grail = { atk = 11558, hp = 13640 }
                  }
    , skills    = [ { name   = "Saint of the Shore"
                    , rank   = BPlus
                    , icon   = IconNoble
                    , cd     = 8
                    , effect = [ To Self GaugeUp <| Range 20 30
                               , When "on Waterside or Beach field" << Grant Self 3 AttackUp <| Range 10 20
                               ]
                    }
                  , { name   = "Natural Body (Sea)"
                    , rank   = A
                    , icon   = IconFlex
                    , cd     = 7
                    , effect = [ Grant Self 0 DebuffResist Full
                               , To Self Heal <| Range 1000 3000
                               ]
                    }
                , { name   = "Jacob's Limbs"
                  , rank   = B
                  , icon   = IconDamageUp
                  , cd     = 7
                  , effect = [ Grant Self 1 (Special AttackUp <| VsTrait Demon) <| Range 50 100
                             , Grant Self 1 (Special AttackUp <| VsTrait Divine) <| Range 50 100
                             , Grant Self 1 (Special AttackUp <| VsTrait Undead) <| Range 50 100
                             ]
                  }
                  ]
    , passives  = [magicResistance EX]
    , phantasm  = { name   = "Tarasque"
                  , desc   = "O Tragic Drake Who Knew Naught of Love"
                  , rank   = A
                  , card   = Buster
                  , kind   = "Anti-Unit/Anti-Dragon"
                  , hits   = 10
                  , effect = [ To Enemy Damage <| Range 600 1000 ]
                  , over   = [ Debuff Enemy 1 DefenseDown <| Range 10 50 ]
                  , first  = True
                  }
    , gen       = { starWeight = 102, starRate = 10, npAtk = 0.76, npDef = 3 }
    , hits      = { quick = 4, arts = 3, buster = 1, ex = 5 }
    , traits    = [Female, EnumaElish]
    , death     = 21
    , align     = [Lawful, Good]
    , limited   = True
    , free      = False
    , ascendUp  = Ascension
                  (pairWith 4 Piece [Saber, Rider, Berserker])
                  (pairWith 4 Piece [Archer, Lancer, Caster, Assassin])
                  (pairWith 4 Monument [Saber, Rider, Berserker])
                  (pairWith 4 Monument [Archer, Lancer, Caster, Assassin])
    , skillUp   = Reinforcement
                  (pairWith 4 GemOf [Saber, Rider, Berserker])
                  (pairWith 4 GemOf [Archer, Lancer, Caster, Assassin])
                  (pairWith 4 MagicGemOf [Saber, Rider, Berserker])
                  (pairWith 4 MagicGemOf [Archer, Lancer, Caster, Assassin])
                  (pairWith 4 SecretGemOf [Saber, Rider, Berserker])
                  (pairWith 4 SecretGemOf [Archer, Lancer, Caster, Assassin])
                  [(TearstoneOfBlood, 6)]
                  [(DragonsReverseScale, 12)]
    }
  , { name      = "Mash Kyrielight"
    , id        = 1
    , rarity    = 3
    , class     = Shielder
    , attr      = Earth
    , deck      = Deck Quick Arts Arts Buster Buster
    , curve     = 11
    , stats     = { base  = { atk = 1261,  hp = 1854 }
                  , max   = { atk = 6791,  hp = 10302 }
                  , grail = { atk = 10575, hp = 15619 }
                  }
    , skills    = [ { name   = "Honorable Wall of Snowflakes"
                    , rank   = Unknown
                    , icon   = IconShieldUp
                    , cd     = 7
                    , effect = [ Grant Party 3 DefenseUp <| Range 15 20
                               , Times 1 << Grant Party 0 DamageDown <| Flat 2000
                               ]
                    }
                  , { name   = "Obscurant Wall of Chalk"
                    , rank   = Unknown
                    , icon   = IconShield
                    , cd     = 9
                    , effect = [ Grant Ally 1 Invincibility Full
                               , To Ally GaugeUp <| Range 10 20
                               ]
                    }
                , { name   = "Shield of Rousing Resolution"
                  , rank   = Unknown
                  , icon   = IconCrosshairUp
                  , cd     = 8
                  , effect = [ Grant Self 1 Taunt Full
                             , Grant Self 1 NPGen <| Range 200 400
                             ]
                  }
                ]
    , passives  = [magicResistance A, riding C]
    , phantasm  = { name   = "Lord Camelot"
                  , desc   = "Castle of the Distant Utopia"
                  , rank   = BPlusPlus
                  , card   = Arts
                  , kind   = "Anti-Evil"
                  , hits   = 0
                  , effect = [ Grant Party 3 DamageDown <| Range 100 1000
                             , Grant Others 3 AttackUp <| Flat 30
                             ]
                  , over   = [ Grant Party 3 DefenseUp <| Range 30 50 ]
                  , first  = False
                  }
    , gen       = { starWeight = 99, starRate = 9.9, npAtk = 0.84, npDef = 3 }
    , hits      = { quick = 2, arts = 2, buster = 1, ex = 3 }
    , traits    = [Female, Riding, EnumaElish]
    , death     = 24.5
    , align     = [Lawful, Good]
    , limited   = False
    , free      = False
    , ascendUp  = Clear
                  "Septem"
                  "London pt. 4, Arrow II"
                  "Camelot pt. 15, Arrow I"
                  "Babylonia"
    , skillUp   = Reinforcement
                  [(ProofOfHero, 5)]
                  [(DragonFang, 5)]
                  [(SeedOfYggdrasil, 5)]
                  [(OctupletCrystals, 5)]
                  [(VoidsDust, 5)]
                  [(EternalGear, 5)]
                  [(PhoenixFeather, 5)]
                  [(DragonsReverseScale, 5)]
    }
  , { name      = "Angra Mainyu"
    , id        = 107
    , rarity    = 0
    , class     = Avenger
    , attr      = Mankind
    , deck      = Deck Quick Quick Arts Arts Buster
    , curve     = 2
    , stats     = { base  = { atk = 1008, hp = 1502 }
                  , max   = { atk = 5683, hp = 7981 }
                  , grail = { atk = 8235, hp = 11518 }
                  }
    , skills    = [ { name  = "Zarich"
                    , rank   = C
                    , icon    = IconExclamationDown
                    , cd      = 8
                    , effect = [ Debuff Enemy 3 CritChance <| Range 30 50 ]
                    }
                  , { name   = "Tawrich"
                    , rank   = C
                    , icon   = IconDarkMagic
                    , cd     = 8
                    , effect = [ To Enemy GaugeDown <| Flat 1
                               , Debuff Enemy 3 AttackDown <| Range 10 30
                               ]
                    }
                  , { name   = "Annihilation Wish"
                    , rank   = A
                    , icon   = IconQuickUp
                    , cd     = 10
                    , effect = [ When "turn 1" <| Grant Self 1 (Performance Quick) <| Range 20 40
                               , When "turn 2" <| Grant Self 1 (Performance Quick) <| Range 40 80
                               , When "turn 3" <| Grant Self 1 (Performance Quick) <| Range 60 120
                               , When "turn 4" <| Grant Self 1 (Performance Quick) <| Range 80 160
                               , When "turn 5" <| Grant Self 1 (Performance Quick) <| Range 100 200
                               , When "turn 6" <| To Self DemeritKill Full
                               ]
                    }
                  ]
    , passives  = [avenger A, oblivionCorrection A, selfRestoreMagic E]
    , phantasm  = { name   = "Verg Avesta"
                  , desc   = "Falsely Transcribed Creation"
                  , rank   = CMinus
                  , card   = Arts
                  , kind   = "Anti-Unit"
                  , hits   = 1
                  , effect = [ Debuff Self 0 Stun Full
                             , To Enemy Avenge <| Range 200 300 
                             ]
                  , over   = [ To Self Heal <| Range 1000 5000 ]
                  , first  = False
                  }
    , gen       = { starWeight = 29, starRate = 6, npAtk = 0.79, npDef = 5 }
    , hits      = { quick = 3, arts = 2, buster = 1, ex = 4 }
    , traits    = [Male, EnumaElish]
    , death     = 9
    , align     = [Chaotic, Evil]
    , limited   = False
    , free      = False
    , ascendUp  = Ascension
                  [(ProofOfHero, 10), (EvilBone, 10)]
                  [(VoidsDust, 10), (BlackBeastGrease, 10)]
                  [(EternalGear, 10), (TearstoneOfBlood, 10)]
                  [(SpiritRoot, 10), (DragonsReverseScale, 10)]
    , skillUp   = Reinforcement
                  [(ProofOfHero, 10)]
                  [(VoidsDust, 10)]
                  [(EvilBone, 12)]
                  [(HomunculusBaby, 12)]
                  [(GhostLantern, 12)]
                  [(EternalGear, 15)]
                  [(TearstoneOfBlood, 15)]
                  [(SpiritRoot, 15)]
    }
  ]
