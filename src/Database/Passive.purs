module Database.Passive where

import Prelude
import Operators

import Data.Map    (Map, lookup)
import Data.Maybe  (fromMaybe)
import Data.String (joinWith)
import Data.Tuple  (Tuple(..))

import Database.Skill
import Database.Icon

data PassiveEffect = Give Target BuffEffect (Map Rank Number)

showPassiveEffect ∷ Rank → PassiveEffect → String
showPassiveEffect rank (Give target buff ranks)
    = showBuff target (fromMaybe (-1.0) $ lookup rank ranks) buff

type Passive = { name   ∷ String
               , icon   ∷ Icon
               , effect ∷ Array PassiveEffect
               }
showPassive ∷ Tuple Passive Rank → String
showPassive (Tuple {effect} rank) 
    = joinWith "\n" $ (_ ⧺ ".") ∘ showPassiveEffect rank ↤ effect

altreactor =
  { name:   "Altreactor" 
  , icon:   IconHoodUp
  , effect: [ Give Self DebuffResist $m[A:20.0] ]
  } ∷ Passive

atTheBoundary =
  { name:   "At The Boundary"
  , icon:   IconFire
  , effect: [ Give Self KillImmunity $m[A:100.0]
            , Give Self CharmResist  $m[A:100.0]
            , Give Self KillChance   $m[A:5.0]
            ]
  } ∷ Passive

avenger =
  { name:   "Avenger"
  , icon:   IconAvenger
  , effect: [ Give Others DebuffResist $m[A:10.0, B:8.0,  C:6.0]
            , Give Self NPFromDamage   $m[A:20.0, B:18.0, C:16.0]
            ]
  } ∷ Passive

beastsAuthority =
  { name:   "Beast's Authority"
  , icon:   IconExclamationUp
  , effect: [ Give Self CritUp $m[D:8.0] ]
  } ∷ Passive

beastForm =
  { name:   "Beast Form"
  , icon:   IconTeeth
  , effect: [ Give Self BusterUp $m[B:8.0]
            , Give Self StarGen     $m[B:8.0]
            ]
  } ∷ Passive

connectionToTheRoot =
  { name:   "Connection to the Root"
  , icon:   IconDamageUp
  , effect: [ Give Self QuickUp  $m[A:6.0, C:4.0]
            , Give Self ArtsUp   $m[A:6.0, C:4.0]
            , Give Self BusterUp $m[A:6.0, C:4.0]
            ]
  } ∷ Passive

contractWithSamovilas =
  { name:   "Contract with Samovilas"
  , icon:   IconHoodUp
  , effect: [ Give Self DebuffResist  $m[A:10.0]
            , Give Self DebuffSuccess $m[A:10.0]
            ]
  } ∷ Passive

cosmoReactor =
  { name:   "Cosmo Reactor"
  , icon:   IconStarUp
  , effect: [ Give Self StarGen $m[A:10.0, B:8.0] ]
  } ∷ Passive

declineOfCivilization =
  { name:   "Decline of Civilization"
  , icon:   IconExclamationUp
  , effect: [ Give Self CritUp $m[EX:10.0] ]
  } ∷ Passive

doubleClass =
  { name:   "Double Class"
  , icon:   IconMissing
  , effect: []
  } ∷ Passive

divinity =
  { name:   "Divinity"
  , icon:   IconSun
  , effect: [ Give Self DamageUp $m[ APlusPlus:230.0, A:200.0, BPlus:185.0
                                      , B:175.0, BMinus:170.0, C:150.0, D:125.0
                                      , E:100.0, EMinus:95.0
                                      ] ]
  } ∷ Passive

existenceOutsideTheDomain =
  { name:   "Existence Outside the Domain"
  , icon:   IconSpotlight
  , effect: [ Give Party StarsPerTurn $m[EX:2.0,  B:2.0, D:2.0] 
            , Give Self  DebuffResist $m[EX:12.0, B:8.0, D:4.0]
            ]
  } ∷ Passive

goddessEssence =
  { name:   "Goddess' Essence"
  , icon:   IconGoddess
  , effect: [ Give Self DamageUp  $m[EX:300.0, A:250.0, B:225.0, C:200.0] 
            , Give Self DebuffResist $m[EX:30.0,  A:25.0,  B:22.5,  C:20.0]
            ]
  } ∷ Passive

homunculus = 
  { name:   "Homunculus"
  , icon:   IconArtsUp
  , effect: [ Give Self ArtsUp    $m[CPlus:6.5] 
            , Give Self DebuffResist $m[CPlus:6.5]
            ]
  } ∷ Passive

highServant =
  { name:   "High Servant"
  , icon:   IconMissing
  , effect: []
  } ∷ Passive

independentAction =
  { name:   "Independent Action"
  , icon:   IconDash
  , effect: [ Give Self CritUp $m[ EX:12.0, APlus:11.0, A:10.0, B:8.0
                                     , C:6.0 
                                     ] ]
  } ∷ Passive

independentActionCeleb =
  { name:   "Independent Action (Celeb)"
  , icon:   IconDash
  , effect: [ Give Self CritUp $m[EX:10.0] 
            --- TODO Charges own NP gauge by 3% on Waterside and Beach Field. 
            ]
  } ∷ Passive

independentManifestation =
  { name:   "Independent Manifestation"
  , icon:   IconDash
  , effect: [ Give Self CritUp   $m[C:6.0, E:2.0]
            , Give Self MentalResist $m[C:6.0, E:2.0]
            , Give Self KillResist   $m[C:6.0, E:2.0]
            ]
  } ∷ Passive

insanity = 
  { name:   "Insanity"
  , icon:   IconBusterUp
  , effect: [ Give Self BusterUp $m[C:8.0] ]
  } ∷ Passive

itemConstruction =
  { name:   "Item Construction"
  , icon:   IconPotion
  , effect: [ Give Self DebuffSuccess $m[ EX:12.0, A:10.0, BPlus:9.0, B:8.0
                                        , C:6.0, D:4.0
                                        ] ]
  } ∷ Passive

itemConstructionFalse = 
  { name:   "Item Construction (False)"
  , icon:   IconPotion
  , effect: [ Give Self DebuffSuccess $m[A:10.0] ]
  } ∷ Passive

itemConstructionOdd =
  { name:   "Item Construction (Odd)"
  , icon:   IconPotion
  , effect: [ Give Self HealingReceived $m[EX:10.0] ]
  } ∷ Passive

logosEater = 
  { name:   "Logos Eater"
  , icon:   IconShieldUp
  , effect: [] -- TODO Increases own defense against Humanoid enemies by 16%. 
  } ∷ Passive

madness =
  { name:   "Madness Enhancement"
  , icon:   IconTeeth
  , effect: [ Give Self BusterUp $m[ EX:12.0, APlus:11.0, A:10.0, B:8.0
                                      , C:6.0, DPlus:5.0, D:4.0, EPlus:3.0
                                      , E:2.0, EMinus:1.0
                                      ] ]   
  } ∷ Passive

magicResistance = 
  { name:   "Magic Resistance"
  , icon:   IconDiamonds
  , effect: [ Give Self DebuffResist $m[ EX:25.0, APlus:21.0, A:20.0
                                       , BPlus:18.0, BPlus:18.0, B:17.5
                                       , CPlus:15.5, C:15.0, DPlus:13.0, D:12.5
                                       , E:10.0
                                       ] ]
  } ∷ Passive

mixedBlood =
  { name:   "Mixed Blood"
  , icon:   IconNobleTurn
  , effect: [ Give Self GaugePerTurn $m[EX:5.0] ]
  } ∷ Passive

negaSaver =
  { name:   "Nega Saver"
  , icon:   IconDamageUp
  , effect: [] -- TODO Deals x1.5 damage against Ruler class enemies. 
  } ∷ Passive

oblivionCorrection =
  { name:   "Oblivion Correction"
  , icon:   IconEclipse
  , effect: [ Give Self CritUp $m[A:10.0, B:8.0, C:6.0] ]
  } ∷ Passive

presenceConcealment =
  { name:   "Presence Concealment"
  , icon:   IconMask
  , effect: [ Give Self StarGen $m[ APlus:10.5, A:10.0, B:8.0, CPlus:6.5, C:6.0
                                  , CMinus:5.5, D:4.0, E:2.0
                                  ] ]
  } ∷ Passive

presenceConcealmentShade =
  { name:   "Presence Concealment (Shade)"
  , icon:   IconMask
  , effect: [ Give Self StarGen       $m[B:8.0] 
            , Give Self DemeritDebuff  $m[B:10.0]
            ]
  } ∷ Passive

riding = 
  { name:   "Riding"
  , icon:   IconHorse
  , effect: [ Give Self QuickUp $m[ EX:12.0, APlusPlus:11.5, APlus:11.0
                                     , A:10.0, B:8.0, CPlus:7.0, C:6.0, E:2.0
                                     ] ]
  } ∷ Passive

ruffian = 
  { name:   "Ruffian"
  , icon:   IconExclamationUp
  , effect: [ Give Self CritUp $m[A:5.0] 
            , Give Self QuickUp $m[A:5.0]
            ]
  } ∷ Passive

selfRestoreMagic =
  { name:   "Self-Restoration (Magical Energy)"
  , icon:   IconNiffin
  , effect: [ Give Self GaugePerTurn $m[ APlus:4.0, A:3.8, B:3.5, C:3.3, D:3.0
                                    , D:2.0 
                                    ] ]
  } ∷ Passive

surfing = 
  { name:   "Surfing"
  , icon:   IconArtsUp
  , effect: [ Give Self ArtsUp $m[A:5.0]
            , Give Self StarGen   $m[A:5.0]
            ]
  } ∷ Passive

territoryCreation = 
  { name:   "Territory Creation"
  , icon:   IconMagicCircle
  , effect: [ Give Self ArtsUp $m[ EX:12.0, APlusPlus:11.5, APlus:11.0
                                    , A:10.0, B:8.0, CPlus:7.0, C:6.0, D:4.0
                                    ] ]
  } ∷ Passive

theOneWhoSwallowsTheEarth = 
  { name:   "The One Who Swallows the Earth"
  , icon:   IconHoodUp
  , effect: [ Give Self BurnImmunity $m[EX:100.0] ]
  } ∷ Passive
  
unlimitedPranaSupply =
  { name:   "Unlimited Prana Supply"
  , icon:   IconNobleTurn
  , effect: [ Give Self GaugePerTurn $m[C:3.0] ]
  } ∷ Passive
