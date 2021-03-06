module Site.Update exposing (siteUpdate)

import List.Extra as List

import StandardLibrary     exposing (..)
import Site.Algebra        exposing (..)
import Site.Base           exposing (..)
import Site.Common         exposing (..)
import Site.Filtering      exposing (..)
import Persist.Flags       exposing (..)
import Persist.Preferences exposing (..)

siteUpdate : (focus -> filt)
          -> (filt -> String)
          -> (SiteModel filt focus extra -> SiteModel filt focus extra)
          -> Preferences
          -> SiteMsg filt focus
          -> SiteModel filt focus extra
          -> (SiteModel filt focus extra, Cmd (SiteMsg filt focus))
siteUpdate transform show reSort prefs msg st =
  let
    relist        = updateListing prefs transform
    goUp x        = (x, scrollToTop "content")
    toggleIn x xs =
        if List.any (eqFilter x) xs then
          removeWith eqFilter x xs
        else
          x :: xs
  in case msg of
    ToSection section ->
        pure { st | section = section }
    ClearAll ->
        goUp <| relist { st | exclude = [], filters = [] }
    Check t True ->
        goUp <| relist
        { st | exclude = List.filter (.tab >> (/=) t) st.exclude }
    Check t False ->
      let
        filters =
            List.find (.tab >> (==) t) st.allFilters
            |> Maybe.map .filters
            >> Maybe.withDefault []
      in
        goUp <| relist
        { st | exclude = List.uniqueBy ordFilter <| filters ++ st.exclude }
    SetSort sortBy ->
        goUp << relist <| reSort { st | sortBy = sortBy }
    MatchAny matchAny ->
        goUp <| relist { st | matchAny = matchAny }
    Focus focus ->
        ( { st | focus = focus }
        , setFocus st.navKey st.root <| Maybe.map (transform >> show) focus
        )
    FilterBy filters ->
      let
        resetPath (x, y) = (x, Cmd.batch [y, setPath st.navKey [st.root]])
      in
        resetPath << goUp << relist <|
        if List.any (.tab >> exclusive) filters then
          { st
          | exclude = filters
          , filters = []
          , focus   = Nothing
          }
        else
          { st
          | exclude = []
          , filters = filters
          , focus   = Nothing
          }
    SetPref k v -> pure << relist <| reSort st
    Toggle filter ->
        goUp << relist <|
        if exclusive filter.tab then
            { st | exclude = toggleIn filter st.exclude }
        else
            { st | filters = toggleIn filter st.filters }
    _ -> pure st
