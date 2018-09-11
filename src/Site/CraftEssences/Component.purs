module Site.CraftEssences.Component (Query, Message(..), comp) where

import Prelude
import Operators (enumArray, (?))

import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Data.Map                as M
import Data.String             as S

import Data.Date
import Data.Tuple
import Halogen (Component, ComponentDSL, ComponentHTML, component, get, liftEffect, modify_, raise)
import Data.Array
import Data.Maybe
import Effect.Class
import Halogen.HTML (HTML)
import Routing.Hash

import Database
import Site.Common
import Site.Filtering
import Site.Preferences
import Site.CraftEssences.Filters
import Site.CraftEssences.Sorting

type Input = Unit
data Message = Message (Array (Filter CraftEssence)) (Maybe Servant)
data Query a
    = Switch    (Maybe Servant) a
    | Focus     (Maybe CraftEssence) a
    | ClearAll  a
    | FilterBy  (Array (Filter CraftEssence)) a
    | Toggle    (Filter CraftEssence) a
    | MatchAny  Boolean a
    | SetSort   SortBy a
    | SetPref   Preference Boolean a

type State = { filters  :: Array (Filter CraftEssence)
             , exclude  :: Array (Filter CraftEssence)
             , matchAny :: Boolean
             , focus    :: Maybe CraftEssence
             , sortBy   :: SortBy
             , prefs    :: Preferences
             , listing  :: Array (Tuple String CraftEssence)
             , sorted   :: Array (Tuple String CraftEssence)
             }

comp :: ∀ m. MonadEffect m => Array (Filter CraftEssence) -> Maybe CraftEssence 
     -> Preferences -> Date -> Component HTML Query Unit Message m
comp initialFilt initialFocus initialPrefs today = component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  allFilters :: FilterList CraftEssence
  allFilters = collectFilters getFilters today

  initialState :: Input -> State
  initialState = const $ updateListing { filters
                                       , exclude
                                       , matchAny: true
                                       , focus:    initialFocus
                                       , sortBy:   Rarity
                                       , prefs:    initialPrefs
                                       , sorted:   initialSort
                                       , listing:  initialSort
                                       }
    where 
      initialSort = getSort Rarity
      {yes: exclude, no: filters} = partition (exclusive <<< getTab) initialFilt

  render :: State -> ComponentHTML Query
  render st = modal st.prefs st.focus
      [ H.aside_ $
        [ _h 1 "Settings"
        , H.form_ $ M.toUnfoldableUnordered st.prefs <#> \(Tuple k v) -> 
          H.p [_click <<< SetPref k $ not v] $ _checkbox (show k) v
        , _h 1 "Sort by"
        , H.form_ $ enumArray <#> \sort -> 
          H.p [_click $ SetSort sort] $ _radio (show sort) (st.sortBy == sort)
        , _h 1 "Include"
        ] <> (filter (exclusive <<< fst) allFilters >>= filterSection)
      , H.section_ <<< (if st.sortBy == Rarity then identity else reverse) $
        portrait false artorify <$> st.listing
      , H.aside_ $
        [ _h 1 "Browse"
        , _strong "Craft Essences"
        , _a "Servants" $ Switch Nothing
        , _h 1 "Filter"
        , H.form_
          [ H.table_ 
            [ H.tr_
              [ _th "Match"
              , H.td [_click $ MatchAny false] $ _radio "All" (not st.matchAny)
              , H.td [_click $ MatchAny true]  $ _radio "Any"      st.matchAny
              ]
            ]
          , H.button clearAll $ _txt "Reset All" 
          ]
        ] <> (filter (not exclusive <<< fst) allFilters >>= filterSection)
      ]
    where
      artorify = getPreference st.prefs Artorify
      noSelf   = getPreference st.prefs ExcludeSelf
      clearAll
        | null st.filters && null st.exclude = [ P.enabled false ]
        | otherwise = [ P.enabled true, _click ClearAll ]
      filterSection (Tuple _ []) = []
      filterSection (Tuple tab filts) = 
          [ _h 3 $ show tab
          , H.form_ $ filts <#> \filt -> 
            H.p [_click $ Toggle filt ] <<< _checkbox (show filt) $
            if exclusive tab 
            then filt `notElem` st.exclude 
            else filt `elem` st.filters
          ]

  eval :: Query ~> ComponentDSL State Query Message m
  eval = case _ of
      Switch    switchTo a -> a <$ do
          {exclude, filters} <- get
          raise $ Message (exclude <> filters) switchTo
      ClearAll           a -> a <$ modif _{ exclude = [], filters = [] }
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
          liftEffect $ setPreference k v
          modif <<< modPrefs $ M.insert k v
      Toggle     filt     a
        | exclusive $ getTab filt -> a <$ modif (modExclude $ toggleIn filt)
        | otherwise               -> a <$ modif (modFilters $ toggleIn filt)
      where
      modif = modify_ <<< compose updateListing
      modFilters f st = st{ filters = f st.filters }
      modPrefs   f st = st{ prefs   = f st.prefs }
      modExclude f st = st{ exclude = f st.exclude }
      toggleIn x xs
        | x `elem` xs = delete x xs
        | otherwise   = cons x xs
      hash Nothing = setHash ""
      hash (Just ce) = setHash <<< urlName $ show ce

portrait :: ∀ a. Boolean -> Boolean -> Tuple String CraftEssence
         -> HTML a (Query Unit)
portrait big artorify (Tuple lab ce'@(CraftEssence ce)) = H.div meta
    [ _img $ "img/CraftEssence/" <> fileName ce.name <> ".png"
    , H.header_ <<< (lab /= "") ? append [_span $ noBreakName big lab, H.br_] $
      [ _span <<< noBreakName big <<< artorify ? doArtorify $ ce.name ]
    , H.footer_ [_span <<< S.joinWith "  " $ replicate ce.rarity "★"]
    ]
  where 
    meta       = not big ? (cons <<< _click <<< Focus $ Just ce') $
                 [_c $ "portrait stars" <> show ce.rarity]
    doArtorify = S.replaceAll (S.Pattern "Altria") (S.Replacement "Artoria")

modal :: ∀ a. Preferences -> Maybe CraftEssence
      -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
modal prefs Nothing = H.div [_c $ mode prefs] <<< append
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
modal prefs
(Just ce'@(CraftEssence ce@{stats:{base, max}})) = H.div 
    [_c $ "fade " <> mode prefs] <<< append
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ portrait true (getPreference prefs Artorify) $ Tuple "" ce'
      , _table ["", "ATK", "HP"]
        [ H.tr_ [ _th "Base",  _td $ print' base.atk,  _td $ print' base.hp ]
        , H.tr_ [ _th "Max",   _td $ print' max.atk,   _td $ print' max.hp ]
        ]
      , _img $ "img/Skill/" <> show ce.icon <> ".png"
      , _h 2 "Effects"
      ] <> (if base == max then [] else
      [ H.section_ $ effectEl <<< mapAmount (\a _ -> Flat a) <$> ce.effect
      , _h 2 "Max Limit Break"
      ]) <>
      [ H.section_ $ effectEl <<< mapAmount (\_ b -> Flat b) <$> ce.effect ]
    ]

effectEl :: ∀ a. ActiveEffect -> HTML a (Query Unit)
effectEl ef
  | demerit ef = H.p [_c "demerit"] <<< _txt $ show ef
  | otherwise  = H.p (maybe [] meta $ activeFilter ef) <<< _txt $ show ef
  where
    meta filt = [_c "link", _click $ FilterBy [filt] ]
