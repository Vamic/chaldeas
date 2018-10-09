module Site.Filtering exposing 
  ( ScheduledFilter, getScheduled
  , updateListing
  , collectFilters
  , matchFilter, namedBonus, skillFilter
  )

import List.Extra as List
import Date exposing (Date)
import Time

import StandardLibrary     exposing (..)
import Database.Has        exposing (..)
import Database.Skill      exposing (..)
import Persist.Preferences exposing (..)
import Site.Algebra        exposing (..)
import Site.ToImage        exposing (..)

type alias ScheduledFilter a = 
    { start  : Date 
    , end    : Date
    , filter : Filter a
    }

getScheduled : List (ScheduledFilter a) -> Date -> List (Filter a)
getScheduled xs today = 
  let
    scheduled x = Date.on (<=) x.start today && Date.on (<=) today x.end
  in
    List.map .filter <| List.filter scheduled xs

updateListing : (b -> a) -> SiteModel a b c -> SiteModel a b c
updateListing f st = 
  let
    noSelf = prefer st.prefs ExcludeSelf
    matches x filter = filter.match noSelf x
    match x = 
        ( List.isEmpty st.exclude 
        || List.all (not << matches x) st.exclude
        ) 
        &&
        ( List.isEmpty st.filters 
        || (if st.matchAny then List.any else List.all) (matches x) st.filters
        )
  in
    { st
    | listing = List.filter (Tuple.second >> f >> match) st.sorted
    }

collectFilters : (Date -> FilterTab -> List (Filter a)) -> Date -> FilterList a
collectFilters f today = flip List.map enumFilterTab <| \tab ->
    { tab = tab, filters = reduceFilters <| f today tab }

reduceFilters : List (Filter a) -> List (Filter a)
reduceFilters = 
  let
    go (x, xs) = { x | match = \a b -> List.any (\f -> f.match a b) <| x :: xs }
  in
    List.sortWith compareFilter 
    >> List.groupWhile eqFilter
    >> List.map go

matchFilter : Maybe (ToImage b) -> Has a b -> FilterTab -> b -> Filter a
matchFilter toImage {show, has} tab x =
    { icon  = case toImage of
                Nothing -> Nothing
                Just f  -> Just <| f x
    , tab   = tab
    , name  = show x
    , match = \b -> List.member x << has b
    }

namedBonus : FilterTab -> String -> List String -> Filter { a | name : String }
namedBonus tab name names =
    { icon  = Nothing 
    , tab   = tab
    , name  = name
    , match = always <| .name >> flip List.member names
    }

skillFilter : SkillEffect -> (a -> List SkillEffect) -> Maybe (Filter a)
skillFilter a getEffects = case a of
  Grant _ _ buff _ -> Just <| matchFilter 
      (Just toImageBuffEffect) 
      (hasBuffEffect getEffects)
      (FilterBuff <| buffCategory buff)
      buff
  Debuff _ _ debuff _ -> Just <| matchFilter 
      (Just toImageDebuffEffect) 
      (hasDebuffEffect getEffects)
      FilterDebuff
      debuff
  To _ action _ -> Just <| matchFilter 
      Nothing
      (hasInstantEffect getEffects)
      FilterAction
      action
  Bonus bonus _ _ -> Just <| matchFilter 
      Nothing
      (hasBonusEffect getEffects)
      FilterBonus
      bonus
  Chance _ b    -> skillFilter b getEffects
  Chances _ _ b -> skillFilter b getEffects
  When _ b      -> skillFilter b getEffects
  Times _ b     -> skillFilter b getEffects
  ToMax _ b     -> skillFilter b getEffects
