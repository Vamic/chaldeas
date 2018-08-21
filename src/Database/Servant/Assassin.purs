module Database.Servant.Assassin where

import Operators
import Database.Model

assassins ∷ Array Servant
assassins = Servant ↤
[ { name:     "Jack the Ripper"
  , rarity:   5
  , class:    Assassin
  , attr:     Earth
  , deck:     Deck Quick Quick Quick Arts Buster
  , stats:    { base:  { atk: 1786,  hp: 1862 }
              , max:   { atk: 11557, hp: 12696 }
              , grail: { atk: 12651, hp: 13909 }
              }
  , ratings:  { damage:4, np:5, critical:5, utility:4, support:2, durability:3 }
  , actives:  [ { name:   "Murderer on a Misty Night A"
                , icon:   IconDodge
                , cd:     6
                , effect: [ Grant Self 1 Evasion 0.0 
                          , Grant Self 1 QuickUp 50.0
                          ]
                }
              , { name:   "Information Erasure B"
                , icon:   IconCircuits
                , cd:     5
                , effect: [ To Enemy RemoveBuffs 100.0 
                          , Debuff Enemy 3 CritDown 30.0
                          ]
                }
              , { name:   "Surgery E"
                , icon:   IconHeal
                , cd:     4
                , effect: [ To Ally Heal 2500.0 ]
                }
              ]
  , passives: [presenceConcealment:APlus]
  , phantasm: { name:   "Maria the Ripper"
              , desc:   "The Holy Mother of Dismemberment"
              , rank:   DPlus
              , card:   Quick
              , kind:   "Anti-Personnel"
              , hits:   4
              , effect: [ To Enemy DamageThruDef 2200.0 ]
              , over:   [ Grant Self 1 (DamageUpVs Female) 50.0 ]
              }
  , gen:      { starAbsorb: 153, starGen: 7.9, npPerHit: 0.34, npAttacked: 3 }
  , hits:     { a: 5, b: 5, q: 5, ex: 8 }
  , traits:   [Male, Divine, EnumaElish, HeavenOrEarth]
  , death:    31.5
  , align:    Chaotic:Good
  }
]
