module Site.Eval (siteEval) where

import StandardLibrary
import Routing.Hash as Hash

import Halogen (ComponentDSL, modify_, get, raise)

import Printing
import Sorting
import Site.Algebra
import Site.Filtering
import Site.Preferences

siteEval :: ∀ a b c d m. Show b => MonadEffect m 
         => String -> (b -> a) -> (FilterTab -> Array (Filter a)) 
         -> (SortBy -> Array { label :: String, obj :: b })
         -> SiteQuery a b c
         ~> ComponentDSL (SiteState a b d) 
                         (SiteQuery a b c) 
                         (SiteMessage a c) m
siteEval title transform getFilters getSort = case _ of
    Switch    switchTo a -> a <$ do
        {exclude, filters} <- get
        raise $ SiteMessage (exclude <> filters) switchTo
    ClearAll           a -> a <$ modif _{ exclude = [], filters = [] }
    Check t  true      a -> a <$ do
        modif <<< modExclude <<< filter $ notEq t <<< getTab
    Check t  false    a -> a <$
        modif (modExclude $ nub <<< append (getFilters t))
    SetSort   sortBy   a -> a <$ modif _{ sortBy = sortBy
                                        , sorted = getSort sortBy
                                        }
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
        modif <<< modPrefs $ setPreference k v
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
      | otherwise   = x : xs
    hash Nothing  = Hash.setHash title
    hash (Just x) = Hash.setHash <<< urlName $ show x
