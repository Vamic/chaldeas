module Site.Eval (siteEval) where

import StandardLibrary
import Routing.Hash as Hash

import Halogen (ComponentDSL, modify_, get, raise)

import Printing
import Site.Algebra
import Site.Filtering
import Site.Preferences

siteEval :: ∀ inFilters inFocus toAlternate e m. Show inFocus => MonadEffect m
         => String -> (inFocus -> inFilters) 
         -> (FilterTab -> Array (Filter inFilters))
         -> (SiteState inFilters inFocus e -> SiteState inFilters inFocus e)
         -> SiteQuery inFilters inFocus toAlternate
         ~> ComponentDSL (SiteState inFilters inFocus e)
                         (SiteQuery inFilters inFocus toAlternate)
                         (SiteMessage inFilters toAlternate) m
siteEval title transform getFilters reSort = case _ of
    Switch    switchTo a -> a <$ do
        {exclude, filters, sortBy} <- get
        raise $ SiteMessage sortBy (exclude <> filters) switchTo
    ToSection section  a -> a <$ modify_ _{ section = section }
    ClearAll           a -> a <$ modif _{ exclude = mempty, filters = mempty }
    Check t  true      a -> a <$ do
        modif <<< modExclude <<< filter $ notEq t <<< getTab
    Check t  false    a -> a <$
        modif (modExclude $ nub <<< append (getFilters t))
    SetSort   sortBy   a -> a <$ modif (reSort <<< _{ sortBy = sortBy })
    MatchAny  matchAny a -> a <$ modif _{ matchAny = matchAny }
    Focus     focus    a -> a <$ do
        liftEffect $ hash focus
        modify_ _{ focus = focus }
    FilterBy filts  a -> a <$ do
        liftEffect $ hash Nothing
        modif if any (exclusive <<< getTab) filts
          then _{ exclude = filts
                , filters = []
                , focus   = Nothing
                }
          else _{ exclude = []
                , filters = filts
                , focus   = Nothing
                }
    SetPref   k v      a -> a <$ do
        liftEffect $ writePreference k v
        modif $ reSort <<< modPrefs (setPreference k v)
    Toggle     filt     a
      | exclusive $ getTab filt -> a <$ modif (modExclude $ toggleIn filt)
      | otherwise               -> a <$ modif (modFilters $ toggleIn filt)
    Ascend _ _ a         -> pure a
    OnTeam _ _ a         -> pure a
    MineOnly _ a         -> pure a
    DoNothing a          -> pure a
  where
    modif = modify_ <<< compose (updateListing transform)
    modFilters f st = st{ filters = f st.filters }
    modPrefs   f st = st{ prefs   = f st.prefs }
    modExclude f st = st{ exclude = f st.exclude }
    toggleIn x xs
      | x `elem` xs = delete x xs
      | otherwise   = cons x xs
    hash Nothing  = Hash.setHash title
    hash (Just x) = Hash.setHash <<< urlName $ show x
