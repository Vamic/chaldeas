module Site.Algebra exposing (..)

import Browser.Navigation as Navigation
import Json.Decode        as Json
import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html)
import Time

import StandardLibrary     exposing (..)
import Database.Base       exposing (..)
import Database.Skill      exposing (..)
import Database.Servant    exposing (..)
import MyServant           exposing (..)
import Persist.Preferences exposing (..)
import Persist.Flags       exposing (..)
import Printing            exposing (..)
import Site.ToImage        exposing (..)
import Sorting             exposing (..)

type alias Component model msg =
    { init          : Value -> Navigation.Key -> model
    , view          : model -> Html msg
    , update        : msg -> model -> (model, Cmd msg)
    }

type SiteMsg inFilters inFocus toAlternate
    = ToSection (Maybe Section)
    | Focus     (Maybe inFocus)
    | ClearAll
    | Check     FilterTab Bool
    | FilterBy  (List (Filter inFilters))
    | Toggle    (Filter inFilters)
    | MatchAny  Bool
    | SetSort   SortBy
    | SetPref   Preference Bool
    | Ascend    inFocus Int
    | OnTeam    Bool inFocus
    | MineOnly  Bool
    | Switch    (Maybe toAlternate)
    | DoNothing

type alias SiteModel inFilters inFocus extra = 
    { error      : Maybe String
    , today      : Date
    , navKey     : Navigation.Key
    , root       : String
    , allFilters : FilterList inFilters
    , section    : Maybe Section
    , filters    : List (Filter inFilters)
    , exclude    : List (Filter inFilters)
    , matchAny   : Bool
    , focus      : Maybe inFocus
    , sortBy     : SortBy
    , prefs      : Preferences
    , sorted     : List (String, inFocus)
    , listing    : List (String, inFocus)
    , team       : Dict OrdServant MyServant
    , extra      : extra
    }

siteInit : (Date -> FilterList inFilters) -> Value -> Navigation.Key -> extra
        -> SiteModel inFilters inFocus extra
siteInit getFilters val navKey extra = 
  let
    (error, {today, preferences, team}) = 
        case Json.decodeValue decodeFlags val of
          Ok flags -> (Nothing, flags)
          Err err  -> 
            (Just <| Json.errorToString err
            , { today       = 0 |> Time.millisToPosix >> Date.today
              , preferences = noPreferences
              , team        = Dict.empty
              }
            )
  in
    { error      = error
    , today      = today
    , navKey     = navKey
    , root       = ""
    , allFilters = getFilters today
    , section    = Nothing
    , filters    = []
    , exclude    = []
    , matchAny   = True
    , focus      = Nothing
    , sortBy     = Rarity
    , prefs      = preferences
    , sorted     = []
    , listing    = []
    , team       = team
    , extra      = extra
    }

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

showSection : Section -> String
showSection = Debug.toString >> String.dropLeft 7 >> unCamel

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

showFilterTab : FilterTab -> String
showFilterTab a = case a of
    FilterPhantasm -> "NP Type"
    FilterCard     -> "NP Card"
    FilterBuff c   -> "Buff (" ++ showBuffCategory c ++ ")"
    _              -> a |> Debug.toString >> String.dropLeft 6 >> unCamel

exclusive : FilterTab -> Bool
exclusive = on (<=) ordFilterTab FilterSource

type alias Filter a =
    { icon  : Maybe ImagePath
    , tab   : FilterTab 
    , name  : String
    , match : Bool -> a -> Bool
    }

eqFilter : Filter a -> Filter a -> Bool
eqFilter x y = x.tab == y.tab && x.name == y.name

type alias OrdFilter = String

ordFilter : Filter a -> OrdFilter
ordFilter x = 
    showFilterTab x.tab ++ 
    if x.tab == FilterRarity then
      String.fromInt <| 10 - String.length x.name
    else if String.startsWith "+" x.name then
      case String.split " " x.name of
        []       -> x.name
        w :: ws  -> String.join " " ws ++ w
    else
      x.name

compareFilter : Filter a -> Filter a -> Order
compareFilter x y = case compareWith x y <| .tab >> ordFilterTab of
    EQ -> compareWith x y ordFilter
    a  -> a

type alias FilterList a = List { tab : FilterTab, filters : List (Filter a) }
