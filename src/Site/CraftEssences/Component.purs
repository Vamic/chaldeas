-- | The user interface for Craft Essences.
-- This module is only for functions that render Craft Essences to HTML.
-- Everything else goes in `Database.CraftEssence`.
module Site.CraftEssences.Component (Query, Message(..), comp) where

import StandardLibrary
import Halogen.HTML            as H
import Routing.Hash            as Hash
import Halogen.HTML.Properties as P
import Data.String             as String

import Data.Date (Date)
import Halogen (Component, ComponentDSL, ComponentHTML, component, get, modify_, raise)
import Halogen.HTML (HTML)

import Database
import Site.Common
import Site.ToImage
import Site.Filtering
import Site.Preferences
import Site.CraftEssences.Filters
import Site.CraftEssences.Sorting
import Printing

type Input = Unit
data Message = Message (Array (Filter CraftEssence)) (Maybe Servant)
data Query a
    = Switch    (Maybe Servant) a
    | Focus     (Maybe CraftEssence) a
    | ClearAll  a
    | Check     FilterTab Boolean a
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
             , sorted   :: Array { label :: String, obj :: CraftEssence }
             , listing  :: Array { label :: String, obj :: CraftEssence }
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
  initialState = const $ updateListing identity
      { filters
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
      {yes: exclude, no: filters} = partition (exclusive <<< getTab)
                                    initialFilt

  render :: State -> ComponentHTML Query
  render st = modal st.prefs st.focus
      [ H.aside_ $
        [ _h 1 "Settings"
        , H.form_ $ unfoldPreferences st.prefs <#> \(k ^ v) ->
          H.p [_click <<< SetPref k $ not v] $ _checkbox Nothing (show k) v
        , _h 1 "Sort by"
        , H.form_ $ enumArray <#> \sort ->
          H.p [_click $ SetSort sort] $ _radio (show sort) (st.sortBy == sort)
        , _h 1 "Include"
        ] <> (filter (exclusive <<< _.tab) allFilters >>= filtersEl)
      , H.section_ <<< maybeReverse $
        portrait false st.prefs <$> st.listing
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
        ] <> (filter (not exclusive <<< _.tab) allFilters >>= filtersEl)
      ]
    where
      maybeReverse = case st.sortBy of
          Rarity -> identity
          _      -> reverse
      clearAll
        | null st.filters && null st.exclude = [ P.enabled false ]
        | otherwise = [ P.enabled true, _click ClearAll ]
      filtersEl { tab, filters } = filterSection Check Toggle st tab filters

  eval :: Query ~> ComponentDSL State Query Message m
  eval = case _ of
      Switch    switchTo a -> a <$ do
          {exclude, filters} <- get
          raise $ Message (exclude <> filters) switchTo
      ClearAll           a -> a <$ modif _{ exclude = [], filters = [] }
      Check t  true      a -> a <$ do
          modif <<< modExclude <<< filter $ notEq t <<< getTab
      Check t  false    a -> a <$
          modif (modExclude $ nub <<< append (getFilters today t))
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
      where
      modif = modify_ <<< compose (updateListing identity)
      modFilters f st = st{ filters = f st.filters }
      modPrefs   f st = st{ prefs   = f st.prefs }
      modExclude f st = st{ exclude = f st.exclude }
      toggleIn x xs
        | x `elem` xs = delete x xs
        | otherwise   = x : xs
      hash Nothing   = Hash.setHash "CraftEssences"
      hash (Just ce) = Hash.setHash <<< urlName $ show ce

portrait :: ∀ a. Boolean -> Preferences
         -> { label :: String, obj :: CraftEssence } -> HTML a (Query Unit)
portrait big prefs {label, obj: ce'@(CraftEssence ce)}
  | not big && prefer prefs Thumbnails =
      H.div [_c "thumb", _click <<< Focus $ Just ce']
      [ toThumbnail ce' ]
  | otherwise =
      H.div meta
      [ toImage ce'
      , H.header_ <<< (label /= "") ? append
        [_span $ noBreakName big label, H.br_] $
        [ _span <<< noBreakName big $ artorify ce.name ]
      , H.footer_ [_span <<< String.joinWith "  " $ replicate ce.rarity "★"]
      ]
  where
    artorify   = prefer prefs Artorify ?
                 String.replaceAll (Pattern "Altria") (Replacement "Artoria")
    meta       = not big ? (cons <<< _click <<< Focus $ Just ce') $
                 [_c $ "portrait stars" <> show ce.rarity]
    doArtorify = String.replaceAll (Pattern "Altria") (Replacement "Artoria")

modal :: ∀ a. Preferences -> Maybe CraftEssence
      -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
modal prefs Nothing = H.div [_c $ mode prefs] <<< append
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
modal prefs
(Just ce'@(CraftEssence ce@{stats:{base, max}})) = H.div
    [_c $ "fade " <> mode prefs] <<< append
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ portrait true prefs {label: "", obj: ce'}
      , _table ["", "ATK", "HP"]
        [ H.tr_ [ _th "Base",  _td $ places' base.atk,  _td $ places' base.hp ]
        , H.tr_ [ _th "Max",   _td $ places' max.atk,   _td $ places' max.hp ]
        ]
      , toImage ce.icon
      , _h 2 "Effects"
      ] <> (if base == max then [] else
      [ H.section_ $ effectEl <<< mapAmount (\x _ -> Flat x) <$> ce.effect
      , _h 2 "Max Limit Break"
      ]) <>
      [ H.section_ $ effectEl <<< mapAmount (\_ y -> Flat y) <$> ce.effect ]
    ]

effectEl :: ∀ a. SkillEffect -> HTML a (Query Unit)
effectEl ef
  | demerit ef = H.p [_c "demerit"] <<< _txt $ show ef
  | otherwise  = H.p (maybe [] meta $ skillFilter ef) <<< _txt $ show ef
  where
    meta filt = [_c "link", _click $ FilterBy [filt] ]
