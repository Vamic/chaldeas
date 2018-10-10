module Site.Base exposing 
  ( Section(..), enumSection
  , FilterTab(..), enumFilterTab, ordFilterTab
  , exclusive
  )

import StandardLibrary exposing (..)
import Database.Skill exposing (..)

type Section
    =  SectionBrowse 
    | SectionSettings 
    | SectionSortBy 
    | SectionInclude 
    | SectionFilter

enumSection : List Section
enumSection =
    [ SectionBrowse 
    , SectionSettings
    , SectionSortBy
    , SectionInclude
    , SectionFilter
    ]

type FilterTab
    = FilterEventBonus
    | FilterAvailability
    | FilterAlignment
    | FilterTrait
    | FilterPassiveSkill
    | FilterMaterial
    | FilterBonus | FilterAction | FilterDebuff
    | FilterBuff BuffCategory
    | FilterDamage
    -- Exclusive
    | FilterSource
    | FilterPhantasm | FilterCard
    | FilterClass
    | FilterDeck
    | FilterAttribute
    | FilterRarity

enumFilterTab : List FilterTab
enumFilterTab =
    [ FilterEventBonus
    , FilterAvailability
    , FilterAlignment
    , FilterTrait
    , FilterPassiveSkill
    , FilterMaterial
    , FilterBonus,  FilterAction,  FilterDebuff
    ] ++ List.map FilterBuff enumBuffCategory ++
    [ FilterDamage
    -- Exclusive
    , FilterSource
    , FilterPhantasm,  FilterCard
    , FilterClass
    , FilterDeck
    , FilterAttribute
    , FilterRarity
    ]

type alias OrdFilterTab = Int

ordFilterTab : FilterTab -> OrdFilterTab
ordFilterTab = enumToOrd enumFilterTab

exclusive : FilterTab -> Bool
exclusive = on (<=) ordFilterTab FilterSource
