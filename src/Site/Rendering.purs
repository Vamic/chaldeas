-- | Renders the site.
module Site.Rendering (site) where

import StandardLibrary
import Data.Array              as Array
import Halogen.HTML            as H
import Halogen.HTML.Properties as P

import Halogen.HTML (HTML)

import Sorting
import Site.Algebra
import Site.Common
import Site.Filtering
import Site.Preferences
import Site.ToImage

render :: ∀ a b c d f. Section -> SiteState a b d -> Array SortBy 
       -> FilterList a
       -> Array (HTML f (SiteQuery a b c Unit))
       -> Array (HTML f (SiteQuery a b c Unit))
render SectionBrowse _ _ _ nav = 
    cons (_h 1 "Browse") nav <> 
    [ _h 1 "Links"
    , H.a [P.href "https://www.reddit.com/message/compose/?to=pareidolist"]
        [H.text "Send Feedback/Suggestions"]
      , H.a [P.href "https://github.com/jnbooth/chaldeas"] [H.text "GitHub"]
      , H.a [P.href "https://grandorder.wiki"] [H.text "GrandOrder Wiki"]  
    ]
render SectionSettings st _ _ _ = 
    [ _h 1 "Settings"
    , H.form_ $ unfoldPreferences st.prefs <#> \(k : v) ->
        H.p [_click <<< SetPref k $ not v] $ _checkbox Nothing (show k) v
    ]
render SectionSortBy st sorts _ _ =
    [ _h 1 "Sort by"
    , H.form_ $ sorts <#> \sort ->
        H.p [_click $ SetSort sort] $ _radio (show sort) (st.sortBy == sort)
    ]
render SectionInclude st _ allFilters _ = 
    cons (_h 1 "Include") $
    (filter (exclusive <<< _.tab) allFilters >>= filterSection st)
render SectionFilter st _ allFilters _ = 
    [ _h 1 "Filter"
    , H.form_
      [ H.table_
        [ H.tr_
          [ _th "Match"
          , H.td [_click $ MatchAny false] $ _radio "All" (not st.matchAny)
          , H.td [_click $ MatchAny true]  $ _radio "Any"      st.matchAny
          ]
        ]
      , H.button clearAll [ H.text "Reset All" ]
      ]
    ] <>
      (filter (not exclusive <<< _.tab) allFilters >>= filterSection st)
  where
    clearAll
      | null st.filters && null st.exclude = [ P.enabled false ]
      | otherwise = [ P.enabled true, _click ClearAll ]


site :: ∀ a b c d f. SiteState a b d -> Array SortBy -> FilterList a
     -> Array (HTML f (SiteQuery a b c Unit))
     -> Array (HTML f (SiteQuery a b c Unit))
     -> Array (HTML f (SiteQuery a b c Unit))
site st@{section: Just x} sorts allFilters nav _ = 
    [ H.div [_i "bg"] []
    , H.aside_ <<< cons (_button "X" true $ ToSection Nothing) $ 
      render x st sorts allFilters nav 
    ]
site st@{section: Nothing} sorts allFilters nav content =
    [ H.div [_i "bg"] []
    , H.aside_ $
      [ _h 1 "Links"
      , H.a [P.href "https://www.reddit.com/message/compose/?to=pareidolist"]
        [H.text "Send Feedback/Suggestions"]
      , H.a [P.href "https://github.com/jnbooth/chaldeas"] [H.text "GitHub"]
      , H.a [P.href "https://grandorder.wiki"] [H.text "GrandOrder Wiki"]
      ] <> renderSection SectionSettings
      <> renderSection SectionSortBy
      <> renderSection SectionInclude 
    , H.section_ content
    , H.aside_ $ cons (_h 1 "Browse") nav <> renderSection SectionFilter
    , H.footer_ $ enumArray <#> \section -> 
        _button (show section) true <<< ToSection $ Just section
    ]
  where
    renderSection x = render x st sorts allFilters nav


filterSection :: ∀ a b c e f. 
                 {exclude :: Array (Filter a), filters :: Array (Filter a) | e}
              -> {tab :: FilterTab, filters :: Array (Filter a)}
              -> Array (HTML f (SiteQuery a b c Unit))
filterSection _ {filters: []} = []
filterSection st {tab, filters} =
    cons (_h 3 $ show tab) <<<
    ( (exclusive tab && length filters > 3) ?
        let checked = length $ filter (eq tab <<< getTab) st.exclude
        in append
            [ _button "All" (checked /= 0) $ Check tab true
            , _button "None" (checked /= length filters) $ Check tab false
            ]
    ) <<< Array.singleton <<< H.form_ $ filters <#> \f'@(Filter f) ->
    H.p [_click $ Toggle f' ] <<<
    _checkbox (toImage <$> f.icon) f.name $ checker f'
  where
    checker :: Filter a -> Boolean
    checker f'@(Filter f)
        | exclusive f.tab = f' `notElem` st.exclude
        | otherwise       = f' `elem`    st.filters
