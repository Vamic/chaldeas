module Database.Passive where

import Prelude
import Operators
import Data.Maybe
import Data.Tuple
import Database.Base
import Database.Icon
import Database.Skill

avenger ∷ PassiveBase
avenger = passiveBase "Avenger" IconAvenger
  [ Give Others DebuffResist [ A:10.0, B:8.0,  C:6.0 ]
  , Give Self NPFromDamage   [ A:20.0, B:18.0, C:16.0 ]
  ]

connectionRoot ∷ PassiveBase
connectionRoot = passiveBase "Connection to the Root" IconDamageUp
  [ Give Self (Boost Quick)  [ A:6.0, C:4.0 ]
  , Give Self (Boost Arts)   [ A:6.0, C:4.0 ]
  , Give Self (Boost Buster) [ A:6.0, C:4.0 ]
  ]

coreOfGoddess ∷ PassiveBase
coreOfGoddess = passiveBase "Core of the Goddess" IconGoddess
  [ Give Self DamageUp     [ EX:300.0, A:250.0, B:225.0, C:200.0 ]
  , Give Self DebuffResist [ EX:30.0,  A:25.0,  B:22.5,  C:20.0 ]
  ]

cosmoReactor ∷ PassiveBase
cosmoReactor = passiveBase "Cosmo Reactor" IconStarHaloUp
  [ Give Self StarUp [ A:10.0, B:8.0 ] ]

divinity ∷ PassiveBase
divinity = passiveBase "Divine" IconSun
  [ Give Self DamageUp
    [ APlusPlus:230.0, A:200.0, BPlus:185.0, B:175.0, BMinus:170.0
    , C:150.0, D:125.0, E:100.0, EMinus:95.0
    ]
  ]

independentAction ∷ PassiveBase
independentAction = passiveBase "Independent Action" IconDash
  [ Give Self CritUp [ EX:12.0, APlus:11.0, A:10.0, B:8.0, C:6.0 ] ]

independentManifestation ∷ PassiveBase
independentManifestation = passiveBase "Independent Manifestation" IconDash
  [ Give Self CritUp       [ C:6.0, E:2.0 ]
  , Give Self MentalResist [ C:6.0, E:2.0 ]
  , Give Self KillResist   [ C:6.0, E:2.0 ]
  ]

itemConstruction ∷ PassiveBase
itemConstruction = passiveBase "Item Construction" IconPotion
  [ Give Self DebuffSuccess
    [ EX:12.0, A:10.0, BPlus:9.0, B:8.0, C:6.0, D:4.0 ]
  ]

itemConstructionFalse ∷ PassiveBase
itemConstructionFalse = passiveBase "Item Construction (False)" IconPotion
  [ Give Self DebuffSuccess [ A:10.0 ] ]

madness ∷ PassiveBase
madness = passiveBase "Madness Enhancement" IconTeeth
  [ Give Self (Boost Buster)
    [ EX:12.0, APlus:11.0, A:10.0, B:8.0, C:6.0, DPlus:5.0, D:4.0, EPlus:3.0
    , E:2.0, EMinus:1.0
    ]
  ]

magicResistance ∷ PassiveBase
magicResistance = passiveBase "Magic Resistance" IconDiamonds
  [ Give Self DebuffResist
    [ EX:25.0, APlus:21.0, A:20.0, BPlus:18.0, BPlus:18.0, B:17.5, CPlus:15.5
    , C:15.0, DPlus:13.0, D:12.5, E:10.0
    ]
  ]

oblivionCorrection ∷ PassiveBase
oblivionCorrection = passiveBase "Oblivion Correction" IconEclipse
  [ Give Self CritUp [ A:10.0, B:8.0, C:6.0 ] ]

presenceConcealment ∷ PassiveBase
presenceConcealment = passiveBase "Presence Concealment" IconMask
  [ Give Self StarUp
    [ APlus:10.5, A:10.0, B:8.0, CPlus:6.5, C:6.0, CMinus:5.5, D:4.0, E:2.0 ]
  ]

riding ∷ PassiveBase
riding = passiveBase "Riding" IconHorse
  [ Give Self (Boost Quick)
    [ EX:12.0, APlusPlus:11.5, APlus:11.0, A:10.0, B:8.0, CPlus:7.0, C:6.0
    , E:2.0
    ]
  ]

selfRestoreMagic ∷ PassiveBase
selfRestoreMagic = passiveBase "Self-Restoration (Magical Energy)" IconNiffin
  [ Give Self GaugePerTurn [ APlus:4.0, A:3.8, B:3.5, C:3.3, D:3.0, D:2.0 ] ]

surfing ∷ PassiveBase
surfing = passiveBase "Surfing" IconArtsUp
  [ Give Self (Boost Arts) [ A:5.0 ]
  , Give Self StarUp [ A:5.0 ]
  ]

territoryCreation ∷ PassiveBase
territoryCreation = passiveBase "Territory Creation" IconMagicCircle
  [ Give Self (Boost Arts)
    [ EX:12.0, APlusPlus:11.5, APlus:11.0, A:10.0, B:8.0, CPlus:7.0, C:6.0
    , D:4.0
    ]
  ]

unlimitedManaSupply ∷ PassiveBase
unlimitedManaSupply = passiveBase "Unlimited Mana Supply" IconNobleTurn
  [ Give Self GaugePerTurn [ C:3.0 ] ]

-----------
-- INTERNAL
-----------

data PassiveEffect = Give Target BuffEffect (Array (Tuple Rank Number))

type Passive = { name   ∷ String
               , rank   ∷ Rank
               , icon   ∷ Icon
               , effect ∷ Array ActiveEffect
               }

type PassiveBase = Rank -> Passive

passiveBase ∷ String -> Icon -> Array PassiveEffect -> PassiveBase
passiveBase name icon effects rank = { name
                                     , rank
                                     , icon
                                     , effect: activate <$> effects
                                     }
  where
    activate (Give target buff ranks) = Grant target 0 buff
                                      ∘ fromMaybe Placeholder
                                      $ Flat <$> lookup rank ranks

