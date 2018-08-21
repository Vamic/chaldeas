module Database.Servant.Avenger where

import Operators
import Database.Model

avengers ∷ Array Servant
avengers = Servant ↤
[ { name:     "Jeanne d'Arc (Alter)"
  , rarity:   5
  , class:    Avenger
  , attr:     Mankind
  , deck:     Deck Quick Arts Arts Buster Buster
  , stats:    { base:  { atk: 2046,  hp: 1724 }
              , max:   { atk: 13244, hp: 11761 }
              , grail: { atk: 14498, hp: 12885 }
              }
  , ratings:  { damage:5, np:4, critical:5, utility:5, support:3, durability:3 }
  , actives:  [ { name:   "Self-Modification EX"
                , icon:   IconExclamationUp
                , cd:     5
                , effect: [ Grant Self 3 CritUp 50.0 
                          , Grant Self 3 StarAbsorb 800.0
                          ]
                }
              , { name:   "Dragon Witch EX"
                , icon:   IconSwordUp
                , cd:     5
                , effect: [ Grant Allies 3 AttackUp 20.0 
                           -- TODO Increases attack of Dragon allies by 20.0 for 3 turns.
                          ]
                }
              , { name:   "Ephemeral Dream A"
                , icon:   IconBusterUp
                , cd:     6
                , effect: [ Grant Self 1 BusterUp 50.0 
                          , Grant Self 1 Invincibility 100.0
                          , To Self DemeritLose 1000.0
                          ]
                }
              ]
  , passives: [avenger:B, oblivionCorrection:A, selfRestoreMagic:APlus]
  , phantasm: { name:   "La Grondement Du Haine"
              , desc:   "How Loudly, My Resentment"
              , rank:   APlus
              , card:   Buster
              , kind:   "Anti-Army"
              , hits:   10
              , effect: [ To Enemy Damage 1000.0 
                        , Debuff Enemy 0 BuffBlock 1.0
                        ]
              , over:   [ Debuff Enemy 5 Curse 500.0 ]
              }
  , gen:      { starAbsorb: 29, starGen: 6.0, npPerHit: 0.83, npAttacked: 5 }
  , hits:     { a: 2, b: 4, q: 3, ex: 7 }
  , traits:   [Female, EnumaElish, Saberface]
  , death:    5.7
  , align:    Chaotic:Evil
  }
]
