module Site.Algebra exposing (..)

import Browser.Navigation as Navigation
import Date exposing (Date)
import Html exposing (Html)

import StandardLibrary     exposing (..)
import Class.ToImage exposing (ImagePath)
import Persist.Preferences exposing (..)
import Persist.Flags       exposing (..)
import Site.Base           exposing (..)
import Sorting             exposing (..)

import Class.Show as Show

type alias Component model msg =
    { init          : Flags -> Navigation.Key -> model
    , view          : model -> Html msg
    , update        : msg -> model -> (model, Cmd msg)
    }

type SiteMsg filt focus
    = ToSection (Maybe Section)
    | Focus     (Maybe focus)
    | ClearAll
    | Check     FilterTab Bool
    | FilterBy  (List (Filter filt))
    | Toggle    (Filter filt)
    | MatchAny  Bool
    | SetSort   SortBy
    | SetPref   Preference Bool
    | Ascend    focus Int
    | OnMine    Bool focus
    | DoNothing

type alias SiteModel filt focus extra =
    { today      : Date
    , navKey     : Navigation.Key
    , root       : String
    , allFilters : FilterList filt
    , section    : Maybe Section
    , filters    : List (Filter filt)
    , exclude    : List (Filter filt)
    , matchAny   : Bool
    , focus      : Maybe focus
    , sortBy     : SortBy
    , prefs      : Preferences
    , sorted     : List (String, focus)
    , listing    : List (String, focus)
    , extra      : extra
    }

siteInit : (Date -> FilterList filt) -> Flags -> Navigation.Key -> extra
        -> SiteModel filt focus extra
siteInit getFilters flags navKey extra =
    { today      = flags.today
    , navKey     = navKey
    , root       = ""
    , allFilters = getFilters flags.today
    , section    = Nothing
    , filters    = []
    , exclude    = []
    , matchAny   = True
    , focus      = Nothing
    , sortBy     = Rarity
    , prefs      = flags.preferences
    , sorted     = []
    , listing    = []
    , extra      = extra
    }

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
    Show.filterTab x.tab ++
    if x.tab == FilterRarity then
      String.fromInt <| 10 - String.length x.name
    else if String.startsWith "+" x.name then
      case String.split " " x.name of
        []       -> x.name
        w :: ws  -> "+" ++ String.join " " ws
                    ++ String.fromInt (String.length w) ++ w
    else
      x.name

compareFilter : Filter a -> Filter a -> Order
compareFilter =
    compareThen (.tab >> ordFilterTab) <|
    compareThen ordFilter
    alwaysEq

type alias FilterList a = List { tab : FilterTab, filters : List (Filter a) }
