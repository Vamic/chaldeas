module Database.Passive where

import Prelude
import Operators

import Data.Array  (singleton)
import Data.Map    (Map, lookup)
import Data.Maybe  (fromMaybe)

import Database.Trait
import Database.Skill
import Database.Icon

data PassiveEffect = Give Target BuffEffect (Map Rank Number)

type Passive = { name   ∷ String
               , rank   ∷ Rank
               , icon   ∷ Icon
               , effect ∷ Array ActiveEffect
               }

{-
showPassiveEffect ∷ Rank → PassiveEffect → String
showPassiveEffect rank (Give target buff ranks)
    = showBuff target (fromMaybe (-1.0) $ lookup rank ranks) buff
showPassive ∷ Tuple Passive Rank → String
showPassive (Tuple {effect} rank) 
    = joinWith "\n" $ (_ ⧺ ".") ∘ showPassiveEffect rank ↤ effect
-}

activate ∷ Rank → PassiveEffect → ActiveEffect
activate rank (Give target buff ranks) 
    = Grant target 0 buff ∘ fromMaybe (-1.0) $ lookup rank ranks

type PassiveBase = Rank → Passive

passiveBase ∷ String → Icon → Array PassiveEffect → PassiveBase
passiveBase name icon effects rank = { name
                                     , rank
                                     , icon
                                     , effect: activate rank ↤ effects
                                     }

altReactor ∷ PassiveBase
altReactor = passiveBase "AltReactor" IconHoodUp
  [ Give Self DebuffResist $m[A:20.0] ]

atTheBoundary ∷ PassiveBase
atTheBoundary = passiveBase "At The Boundary" IconFire
  [ Give Self KillImmunity $m[A:0.0]
  , Give Self CharmResist  $m[A:100.0]
  , Give Self KillChance   $m[A:5.0]
  ]

avenger ∷ PassiveBase
avenger = passiveBase "Avenger" IconAvenger
  [ Give Others DebuffResist $m[A:10.0, B:8.0,  C:6.0]
  , Give Self NPFromDamage   $m[A:20.0, B:18.0, C:16.0]
  ] 

beastsAuthority ∷ PassiveBase
beastsAuthority = passiveBase "Beast's Authority" IconExclamationUp
  [ Give Self CritUp $m[D:8.0] ]

beastForm ∷ PassiveBase
beastForm = passiveBase "Beast Form" IconTeeth
  [ Give Self BusterUp $m[B:8.0]
  , Give Self StarUp   $m[B:8.0]
  ]

connectionRoot ∷ PassiveBase
connectionRoot = passiveBase "Connection to the Root" IconDamageUp
  [ Give Self QuickUp  $m[A:6.0, C:4.0]
  , Give Self ArtsUp   $m[A:6.0, C:4.0]
  , Give Self BusterUp $m[A:6.0, C:4.0]
  ]

contractWithSamovilas ∷ PassiveBase
contractWithSamovilas = passiveBase "Contract with Samovilas" IconHoodUp
  [ Give Self DebuffResist  $m[A:10.0]
  , Give Self DebuffSuccess $m[A:10.0]
  ]

coreOfGoddess ∷ PassiveBase
coreOfGoddess = passiveBase "Core of the Goddess" IconGoddess
  [ Give Self DamageUp  $m[EX:300.0, A:250.0, B:225.0, C:200.0] 
  , Give Self DebuffResist $m[EX:30.0,  A:25.0,  B:22.5,  C:20.0]
  ]

cosmoReactor ∷ PassiveBase
cosmoReactor = passiveBase "Cosmo Reactor" IconStarHaloUp
  [ Give Self StarUp $m[A:10.0, B:8.0] ]

declineOfCivilization ∷ PassiveBase
declineOfCivilization = passiveBase "Decline of Civilization" IconExclamationUp
  [ Give Self CritUp $m[EX:10.0] ]

doubleClass ∷ PassiveBase
doubleClass = passiveBase "Double Class" IconMissing []

divinity ∷ PassiveBase
divinity = passiveBase "Divine" IconSun 
  ∘ singleton ∘ Give Self DamageUp 
  $m[ APlusPlus:230.0, A:200.0, BPlus:185.0, B:175.0, BMinus:170.0
    , C:150.0, D:125.0, E:100.0, EMinus:95.0
    ]

existOutsideDomain ∷ PassiveBase
existOutsideDomain = passiveBase "Existence Outside the Domain" IconSpotlight
  [ Give Party StarsPerTurn $m[EX:2.0,  B:2.0, D:2.0] 
  , Give Self  DebuffResist $m[EX:12.0, B:8.0, D:4.0]
  ]

homunculus ∷ PassiveBase
homunculus = passiveBase "Homunculus" IconArtsUp
  [ Give Self ArtsUp    $m[CPlus:6.5] 
  , Give Self DebuffResist $m[CPlus:6.5]
  ]

highServant ∷ PassiveBase
highServant = passiveBase "High Servant" IconMissing []

independentAction ∷ PassiveBase
independentAction = passiveBase "Independent Action" IconDash
  [ Give Self CritUp $m[EX:12.0, APlus:11.0, A:10.0, B:8.0, C:6.0] ] 

independentActionCeleb ∷ PassiveBase
independentActionCeleb = passiveBase "Independent Action (Celeb)" IconDash
  [ Give Self CritUp $m[EX:10.0] ]
  --- TODO Charges own NP gauge by 3% on Waterside and Beach Field. 

independentManifestation ∷ PassiveBase
independentManifestation = passiveBase "Independent Manifestation" IconDash
  [ Give Self CritUp       $m[C:6.0, E:2.0]
  , Give Self MentalResist $m[C:6.0, E:2.0]
  , Give Self KillResist   $m[C:6.0, E:2.0]
  ]

insanity ∷ PassiveBase
insanity = passiveBase "Insanity" IconBusterUp
  [ Give Self BusterUp $m[C:8.0] ]

itemConstruction ∷ PassiveBase
itemConstruction = passiveBase "Item Construction" IconPotion 
  ∘ singleton ∘ Give Self DebuffSuccess 
  $m[ EX:12.0, A:10.0, BPlus:9.0, B:8.0, C:6.0, D:4.0 ] 

itemConstructionFalse ∷ PassiveBase
itemConstructionFalse = passiveBase "Item Construction (False)" IconPotion
  [ Give Self DebuffSuccess $m[A:10.0] ]

itemConstructionOdd ∷ PassiveBase
itemConstructionOdd = passiveBase "Item Construction (Odd)" IconPotion
  [ Give Self HealingReceived $m[EX:10.0] ]

logosEater ∷ PassiveBase
logosEater = passiveBase "Logos Eater" IconShieldUp 
  [ Give Self (DefenseUpVs Humanoid) $m[C:16.0] ]

madness ∷ PassiveBase
madness = passiveBase "Madness Enhancement" IconTeeth 
  ∘ singleton ∘ Give Self BusterUp
  $m[ EX:12.0, APlus:11.0, A:10.0, B:8.0, C:6.0, DPlus:5.0, D:4.0, EPlus:3.0
    , E:2.0, EMinus:1.0 
    ]

magicResistance ∷ PassiveBase
magicResistance = passiveBase "Magic Resistance" IconDiamonds 
  ∘ singleton ∘ Give Self DebuffResist
  $m[ EX:25.0, APlus:21.0, A:20.0, BPlus:18.0, BPlus:18.0, B:17.5, CPlus:15.5
    , C:15.0, DPlus:13.0, D:12.5, E:10.0 
    ]

mixedBlood ∷ PassiveBase
mixedBlood = passiveBase "Mixed Blood" IconNobleTurn
  [ Give Self GaugePerTurn $m[EX:5.0] ]

negaSaver ∷ PassiveBase
negaSaver = passiveBase "Nega Saver" IconDamageUp 
  [ Give Self (DamageAffinity Ruler) $m[A:1500.0] ]

oblivionCorrection ∷ PassiveBase
oblivionCorrection = passiveBase "Oblivion Correction" IconEclipse
  [ Give Self CritUp $m[A:10.0, B:8.0, C:6.0] ]

presenceConcealment ∷ PassiveBase
presenceConcealment = passiveBase "Presence Concealment" IconMask 
  ∘ singleton ∘ Give Self StarUp
  $m[ APlus:10.5, A:10.0, B:8.0, CPlus:6.5, C:6.0, CMinus:5.5, D:4.0, E:2.0 ]

presenceConcealmentShade ∷ PassiveBase
presenceConcealmentShade = passiveBase "Presence Concealment (Shade)" IconMask
  [ Give Self StarUp $m[B:8.0] ]
            --, Give Self DebuffResist $m[B:(-10.0)] TODO Demerit

riding ∷ PassiveBase
riding = passiveBase "Riding" IconHorse 
  ∘ singleton ∘ Give Self QuickUp
  $m[ EX:12.0, APlusPlus:11.5, APlus:11.0, A:10.0, B:8.0, CPlus:7.0, C:6.0
    , E:2.0
    ]

ruffian ∷ PassiveBase    
ruffian = passiveBase "Ruffian" IconExclamationUp
  [ Give Self CritUp $m[A:5.0] 
  , Give Self QuickUp $m[A:5.0]
  ]

selfRestoreMagic ∷ PassiveBase
selfRestoreMagic = passiveBase "Self-Restoration (Magical Energy)" IconNiffin
  ∘ singleton ∘ Give Self GaugePerTurn
  $m[ APlus:4.0, A:3.8, B:3.5, C:3.3, D:3.0, D:2.0 ]

surfing ∷ PassiveBase
surfing = passiveBase "Surfing" IconArtsUp 
  [ Give Self ArtsUp $m[A:5.0]
  , Give Self StarUp $m[A:5.0]
  ]

territoryCreation ∷ PassiveBase
territoryCreation = passiveBase "Territory Creation" IconMagicCircle
  ∘ singleton ∘ Give Self ArtsUp
  $m[ EX:12.0, APlusPlus:11.5, APlus:11.0, A:10.0, B:8.0, CPlus:7.0, C:6.0
    , D:4.0
    ]

theOneWhoSwallowsEarth ∷ PassiveBase
theOneWhoSwallowsEarth = passiveBase "The One Who Swallows the Earth" IconHoodUp
  [ Give Self BurnImmunity $m[EX:0.0] ]

unlimitedManaSupply ∷ PassiveBase  
unlimitedManaSupply = passiveBase "Unlimited Mana Supply" IconNobleTurn
  [ Give Self GaugePerTurn $m[C:3.0] ]
