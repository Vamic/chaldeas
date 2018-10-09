module Database.Passive exposing (..)

import List.Extra as List

import Database.Base   exposing (..)
import Database.Skill  exposing (..)

avenger = passive "Avenger" IconAvenger
  [ Give Others DebuffResist [ (A, 10), (B, 8), (C, 6) ]
  , Give Self NPFromDamage   [ (A, 20), (B, 18), (C, 16) ]
  ]

connectionRoot = passive "Connection to the Root" IconDamageUp
  [ Give Self (Performance Quick)  [ (A, 6), (C, 4) ]
  , Give Self (Performance Arts)   [ (A, 6), (C, 4) ]
  , Give Self (Performance Buster) [ (A, 6), (C, 4) ]
  ]

coreOfGoddess = passive "Core of the Goddess" IconGoddess
  [ Give Self DamageUp     [ (EX, 300), (A, 250), (B, 225), (C, 200) ]
  , Give Self DebuffResist [ (EX, 30),  (A, 25),  (B, 22.5),  (C, 20) ]
  ]

cosmoReactor = passive "Cosmo Reactor" IconStarHaloUp
  [ Give Self StarUp [ (A, 10), (B, 8) ] ]

divinity = passive "Divine" IconSun
  [ Give Self DamageUp
    [ (APlusPlus, 230), (A, 200), (BPlus, 185), (B, 175), (BMinus, 170)
    , (C, 150), (D, 125), (E, 100), (EMinus, 95)
    ]
  ]

independentAction = passive "Independent Action" IconDash
  [ Give Self CritUp 
    [ (EX, 12), (APlus, 11), (A, 10), (B, 8), (C, 6) ] 
  ]

independentManifestation = passive "Independent Manifestation" IconDash
  [ Give Self CritUp       [ (C, 6), (E, 2) ]
  , Give Self MentalResist [ (C, 6), (E, 2) ]
  , Give Self KillResist   [ (C, 6), (E, 2) ]
  ]

itemConstruction = passive "Item Construction" IconPotion
  [ Give Self DebuffSuccess 
    [ (EX, 12), (A, 10), (BPlus, 9), (B, 8), (C, 6), (D, 4) ]
  ]

itemConstructionFalse = passive "Item Construction (False)" IconPotion
  [ Give Self DebuffSuccess [ (A, 10) ] ]

madness = passive "Madness Enhancement" IconTeeth
  [ Give Self (Performance Buster)
    [ (EX, 12), (APlus, 11), (A, 10), (B, 8), (C, 6), (DPlus, 5), (D, 4)
    , (EPlus, 3), (E, 2), (EMinus, 1)
    ]
  ]

magicResistance = passive "Magic Resistance" IconDiamonds
  [ Give Self DebuffResist
    [ (EX, 25), (APlus, 21), (A, 20), (BPlus, 18), (BPlus, 18), (B, 17.5)
    , (CPlus, 15.5), (C, 15), (DPlus, 13), (D, 12.5), (E, 10)
    ]
  ]

oblivionCorrection = passive "Oblivion Correction" IconEclipse
  [ Give Self CritUp [ (A, 10), (B, 8), (C, 6) ] ]

presenceConcealment = passive "Presence Concealment" IconMask
  [ Give Self StarUp
    [ (APlus, 10.5), (A, 10), (B, 8), (CPlus, 6.5), (C, 6), (CMinus, 5.5)
    , (D, 4), (E, 2)
    ]
  ]

riding = passive "Riding" IconHorse
  [ Give Self (Performance Quick)
    [ (EX, 12), (APlusPlus, 11.5), (APlus, 11), (A, 10), (B, 8), (CPlus, 7)
    , (C, 6), (E, 2)
    ]
  ]

selfRestoreMagic = passive "Self-Restoration (Magical Energy)" IconNiffin
  [ Give Self GaugePerTurn
    [ (APlus, 4), (A, 3.8), (B, 3.5), (C, 3.3), (D, 3), (D, 2) ]
  ]

surfing = passive "Surfing" IconArtsUp
  [ Give Self (Performance Arts) [ (A, 5) ]
  , Give Self StarUp [ (A, 5) ]
  ]

territoryCreation = passive "Territory Creation" IconMagicCircle
  [ Give Self (Performance Arts)
    [ (EX, 12), (APlusPlus, 11.5), (APlus, 11), (A, 10), (B, 8), (CPlus, 7)
    , (C, 6), (D, 4)
    ]
  ]

unlimitedManaSupply = passive "Unlimited Mana Supply" IconNobleTurn
  [ Give Self GaugePerTurn [ (C, 3) ] ]

type PassiveEffect = Give Target BuffEffect (List (Rank, Float))

lookup : a -> List (a, b) -> Maybe b
lookup a = List.find (Tuple.first >> (==) a) >> Maybe.map Tuple.second

passive : String -> Icon -> List PassiveEffect -> Rank -> Skill
passive name icon effects rank = 
  let
    skill (Give targ buff ranks) = 
        lookup rank ranks
        |> Maybe.map Flat
        >> Maybe.withDefault Placeholder
        >> Grant targ 0 buff
  in
    { name   = name 
    , rank   = rank
    , icon   = icon
    , cd     = 0
    , effect = List.map skill effects
    }
