-- | This module defines passive skills. Unlike active skills, passives with
-- | the same name and rank will always have identical effects. In other words,
-- | a `Passive` is a function of type `Rank -> Skill`: it accepts a Rank
-- | (such as `EX` or `APlus`) and returns a 'Skill'. 
-- | Passive effects use the `BuffEffects` defined in `Database.Skill`.

-- The easiest way to define a `Passive` is to use the `passive` helper
-- function, which accepts a name, an icon, and a mapping of `Skill` effects
-- to `Rank`s. As usual, mapping is represented in `Tuple` pairs. 
-- For example, since 
-- Avenger A provides 10% Debuff Resistance to the rest of the party,
-- Avenger B provides 8% Debuff Resistance, and Avenger C provides 6%, 
-- the `avenger` function specifies this mapping as 
-- `[(A, 10), (B, 8), (C, 6)]``. Or rather, in PureScript's particular syntax,
-- `[A^ 10.0, B^ 8.0, C^ 6.0]``.
-- ('^' is an infix synonym for the `Tuple` constructor provided by
-- the `Operators` module.)

module Database.Passive where

import StandardLibrary
import Data.Tuple as Tuple

import Database.Base
import Database.Skill

avenger :: Passive
avenger = passive "Avenger" IconAvenger
  [ Give Others DebuffResist [ A^10.0, B^ 8.0,  C^6.0 ]
  , Give Self NPFromDamage   [ A^20.0, B^18.0, C^16.0 ]
  ]

connectionRoot :: Passive
connectionRoot = passive "Connection to the Root" IconDamageUp
  [ Give Self (Performance Quick)  [ A^6.0, C^4.0 ]
  , Give Self (Performance Arts)   [ A^6.0, C^4.0 ]
  , Give Self (Performance Buster) [ A^6.0, C^4.0 ]
  ]

coreOfGoddess :: Passive
coreOfGoddess = passive "Core of the Goddess" IconGoddess
  [ Give Self DamageUp     [ EX^300.0, A^250.0, B^225.0, C^200.0 ]
  , Give Self DebuffResist [ EX^30.0,  A^25.0,  B^22.5,  C^20.0 ]
  ]

cosmoReactor :: Passive
cosmoReactor = passive "Cosmo Reactor" IconStarHaloUp
  [ Give Self StarUp [ A^10.0, B^8.0 ] ]

divinity :: Passive
divinity = passive "Divine" IconSun
  [ Give Self DamageUp
    [ APlusPlus^230.0, A^200.0, BPlus^185.0, B^175.0, BMinus^170.0
    , C^150.0, D^125.0, E^100.0, EMinus^95.0
    ]
  ]

independentAction :: Passive
independentAction = passive "Independent Action" IconDash
  [ Give Self CritUp [ EX^12.0, APlus^11.0, A^10.0, B^8.0, C^6.0 ] ]

independentManifestation :: Passive
independentManifestation = passive "Independent Manifestation" IconDash
  [ Give Self CritUp       [ C^6.0, E^2.0 ]
  , Give Self MentalResist [ C^6.0, E^2.0 ]
  , Give Self KillResist   [ C^6.0, E^2.0 ]
  ]

itemConstruction :: Passive
itemConstruction = passive "Item Construction" IconPotion
  [ Give Self DebuffSuccess
    [ EX^12.0, A^10.0, BPlus^9.0, B^8.0, C^6.0, D^4.0 ]
  ]

itemConstructionFalse :: Passive
itemConstructionFalse = passive "Item Construction (False)" IconPotion
  [ Give Self DebuffSuccess [ A^10.0 ] ]

madness :: Passive
madness = passive "Madness Enhancement" IconTeeth
  [ Give Self (Performance Buster)
    [ EX^12.0, APlus^11.0, A^10.0, B^8.0, C^6.0, DPlus^5.0, D^4.0, EPlus^3.0
    , E^2.0, EMinus^1.0
    ]
  ]

magicResistance :: Passive
magicResistance = passive "Magic Resistance" IconDiamonds
  [ Give Self DebuffResist
    [ EX^25.0, APlus^21.0, A^20.0, BPlus^18.0, BPlus^18.0, B^17.5, CPlus^15.5
    , C^15.0, DPlus^13.0, D^12.5, E^10.0
    ]
  ]

oblivionCorrection :: Passive
oblivionCorrection = passive "Oblivion Correction" IconEclipse
  [ Give Self CritUp [ A^10.0, B^8.0, C^6.0 ] ]

presenceConcealment :: Passive
presenceConcealment = passive "Presence Concealment" IconMask
  [ Give Self StarUp
    [ APlus^10.5, A^10.0, B^8.0, CPlus^6.5, C^6.0, CMinus^5.5, D^4.0, E^2.0 ]
  ]

riding :: Passive
riding = passive "Riding" IconHorse
  [ Give Self (Performance Quick)
    [ EX^12.0, APlusPlus^11.5, APlus^11.0, A^10.0, B^8.0, CPlus^7.0, C^6.0
    , E^2.0
    ]
  ]

selfRestoreMagic :: Passive
selfRestoreMagic = passive "Self-Restoration (Magical Energy)" IconNiffin
  [ Give Self GaugePerTurn [ APlus^4.0, A^3.8, B^3.5, C^3.3, D^3.0, D^2.0 ] ]

surfing :: Passive
surfing = passive "Surfing" IconArtsUp
  [ Give Self (Performance Arts) [ A^5.0 ]
  , Give Self StarUp [ A^5.0 ]
  ]

territoryCreation :: Passive
territoryCreation = passive "Territory Creation" IconMagicCircle
  [ Give Self (Performance Arts)
    [ EX^12.0, APlusPlus^11.5, APlus^11.0, A^10.0, B^8.0, CPlus^7.0, C^6.0
    , D^4.0
    ]
  ]

unlimitedManaSupply :: Passive
unlimitedManaSupply = passive "Unlimited Mana Supply" IconNobleTurn
  [ Give Self GaugePerTurn [ C^3.0 ] ]

-----------
-- INTERNAL
-----------

data PassiveEffect = Give Target BuffEffect (Array (Tuple Rank Number))

type Passive = Rank -> Skill

passive :: String -> Icon -> Array PassiveEffect -> Passive
passive name icon effects rank = { name
                                     , rank
                                     , icon
                                     , cd:     0
                                     , effect: skill <$> effects
                                     }
  where
    skill (Give targ buff ranks) = Grant targ 0 buff <<< fromMaybe Placeholder $ 
                                   Flat <$> Tuple.lookup rank ranks

