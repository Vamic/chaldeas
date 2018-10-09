module Site.Rendering exposing (siteView)

import Browser exposing (Document)

import Html            as H
import Html.Events     as E
import Html.Attributes as P

import StandardLibrary     exposing (..)
import Persist.Preferences exposing (..)
import Sorting             exposing (..)
import Site.Algebra        exposing (..)
import Site.Common         exposing (..)
import Site.Filtering      exposing (..)
import Site.ToImage        exposing (..)

type alias Html a b c = H.Html (SiteMsg a b c)

render : Section -> SiteModel a b d -> List SortBy
      -> List (Html a b c) -> List (Html a b c)
render a st sorts nav = case a of
  SectionBrowse -> 
    h_ 1 "Browse" :: nav ++
    [ h_ 1 "Links" 
    , H.a [P.href "https://www.reddit.com/message/compose/?to=pareidolist"]
        [H.text "Send Feedback/Suggestions"]
      , H.a [P.href "https://github.com/jnbooth/chaldeas"] [H.text "GitHub"]
      , H.a [P.href "https://grandorder.wiki"] [H.text "GrandOrder Wiki"]  
    ]
  SectionSettings ->
    [ h_ 1 "Settings"
    , H.form [] << flip List.map (unfoldPreferences st.prefs) <| \(k, v) ->
        H.p [E.onClick << SetPref k <| not v] <| 
        checkbox_ Nothing (showPreference k) v
    ]
  SectionSortBy ->
    [ h_ 1 "Sort by"
    , H.form [] << flip List.map sorts <| \sort ->
        H.p [E.onClick <| SetSort sort] <|
        radio_ (showSortBy sort) (st.sortBy == sort)
    ]
  SectionInclude ->
    List.filter (.tab >> exclusive) st.allFilters 
    |> List.concatMap (filterSection st)
    >> (::) (h_ 1 "Include")
  SectionFilter ->
    [ h_ 1 "Filter" 
    , H.form [] <|
      [ H.table [] 
        [ H.tr []
          [ text_ H.th "Match" 
          , H.td [E.onClick <| MatchAny False] <| radio_ "All" (not st.matchAny)
          , H.td [E.onClick <| MatchAny True]  <| radio_ "Any" st.matchAny
          ]
         ] 
      , flip H.button [H.text "Reset All"] <| 
        if List.isEmpty st.filters && List.isEmpty st.exclude then
          [P.disabled True]
        else
          [E.onClick ClearAll]
      ] ++
        ( List.filter (.tab >> exclusive >> not) st.allFilters 
          |> List.concatMap (filterSection st)
        )
    ]

siteView : SiteModel a b d -> List SortBy
    -> List (Html a b c) -> Html a b c -> List (Html a b c)
siteView st sorts nav content = case st.section of
  Just x -> 
      [ H.div [P.id "bg"] [] 
      , H.aside [] <|
        button_ "X" True (ToSection Nothing) :: render x st sorts nav
      ]
  Nothing -> 
    let
      renderSection x = render x st sorts nav
      showError = case st.error of
          Nothing -> identity
          Just err -> (::) <| H.div [P.id "error"] [H.text err]
    in
      showError
      [ H.div [P.id "bg"] [] 
      , H.footer [] << flip List.map enumSection <| \section ->
          button_ (showSection section) True << ToSection <| Just section
      , H.aside [] <|
        [ h_ 1 "Links" 
        , H.a [P.href "https://www.reddit.com/message/compose/?to=pareidolist"]
          [H.text "Send Feedback/Suggestions"]
        , H.a [P.href "https://github.com/jnbooth/chaldeas"] [H.text "GitHub"]
        , H.a [P.href "https://grandorder.wiki"] [H.text "GrandOrder Wiki"]
        ] 
        ++ renderSection SectionSettings
        ++ renderSection SectionSortBy
        ++ renderSection SectionInclude
      , content
      , H.aside [] <| h_ 1 "Browse" :: nav ++ renderSection SectionFilter
      ]

filterSection : SiteModel a b d 
             -> { tab : FilterTab, filters : List (Filter a) } 
             -> List (Html a b c)
filterSection st {tab, filters} = case filters of
  [] -> []
  _  -> 
    let
      checked = List.length <| List.filter (.tab >> (==) tab) st.exclude
      addAll = doIf (exclusive tab && List.length filters > 3) <| (++)
          [ button_ "All" (checked /= 0) <| Check tab True
          , button_ "None" (checked /= List.length filters) <| Check tab False
          ]
      filterEl filter = 
          H.p [E.onClick <| Toggle filter] << 
          checkbox_ (Maybe.map imageEl filter.icon) filter.name <|
          if exclusive filter.tab then
            not <| List.any (eqFilter filter) st.exclude
          else
            List.any (eqFilter filter) st.filters
    in
      (::) (h_ 3 <| showFilterTab tab) 
      << addAll << List.singleton << H.form []
      <| List.map filterEl filters
