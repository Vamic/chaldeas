module Site.CraftEssences.Component (Query, comp) where

import Prelude
import Operators (enumArray, (?))

import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Data.Map                as M
import Data.String             as S

import Control.Alternative
import Control.Bind
import Data.Tuple
import Halogen (Component, ComponentDSL, ComponentHTML, component, liftEffect, modify_, raise)
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
type Message = Maybe Servant
data Query a
    = Switch    (Maybe Servant) a
    | Focus     (Maybe CraftEssence) a
    | ClearAll  a
    | FilterBy  (Array (Filter CraftEssence)) a
    | Toggle    (Filter CraftEssence) a
    | MatchAny  Boolean a
    | SetSort   SortBy a
    | SetPref   Preference Boolean a

type State = { filters  ∷ Array (Filter CraftEssence)
             , exclude  ∷ Array (Filter CraftEssence)
             , matchAny ∷ Boolean
             , focus    ∷ Maybe CraftEssence
             , sortBy   ∷ SortBy
             , prefs    ∷ Preferences
             , listing  ∷ Array (Tuple String CraftEssence)
             , sorted   ∷ Array (Tuple String CraftEssence)
             }

comp ∷ ∀ m. MonadEffect m => Maybe CraftEssence -> Preferences
            -> Component HTML Query Unit Message m
comp initialFocus initialPrefs = component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState ∷ Input -> State
  initialState = const { filters:  []
                       , exclude:  []
                       , matchAny: true
                       , focus:    initialFocus
                       , sortBy:   Rarity
                       , prefs:    initialPrefs
                       , sorted:   initialSort
                       , listing:  initialSort
                       }
    where initialSort = getSort Rarity

  render ∷ State -> ComponentHTML Query
  render {exclude, filters, focus, listing, matchAny, prefs, sortBy}
      = modal (pref ShowTables) artorify focus
        [ H.aside_ $
          [ _h 1 "Settings"
          , H.form_ $ M.toUnfoldable prefs <#> \(Tuple k v)
             -> H.p [_click <<< SetPref k $ not v]
                $ _checkbox (show k) v
          , _h 1 "Sort by"
          , H.form_ $ enumArray <#> \sort
             -> H.p [_click $ SetSort sort]
                $ _radio (show sort) (sortBy == sort)
          , _h 1 "Include"
          ] <> (filter exclusive enumArray >>= filterSection)
        , H.section_
          <<< (if sortBy == Rarity then identity else reverse)
          $ portrait false artorify <$> listing
        , H.aside_ $
          [ _h 1 "Browse"
          , H.a [_click $ Switch Nothing, P.href "#"] $ _txt "Servants"
          , H.a_ <<< singleton <<< H.strong_ $ _txt "Craft Essences"
          , _h 1 "Filter"
          , H.form_
            [ H.table_ <<< singleton $ H.tr_
              [ _th "Match"
              , H.td [_click $ MatchAny false] $ _radio "All" (not matchAny)
              , H.td [_click $ MatchAny true]  $ _radio "Any"      matchAny
              ]
            , H.button clearAll $ _txt "Reset All"
            ]
          ] <> (filter (not exclusive) enumArray >>= filterSection)
        ]
    where
      pref     = getPreference prefs
      artorify = pref Artorify
      noSelf   = pref ExcludeSelf
      clearAll
        | null filters && null exclude = [ P.enabled false ]
        | otherwise                    = [ P.enabled true, _click ClearAll ]
      filterSection tab = case getFilters tab of
        []    -> []
        filts -> [ _h 3 $ show tab
                 , H.form_ $ filts <#> \filt
                     -> H.p [_click $ Toggle filt ]
                       <<< _checkbox (show filt)
                       $ if exclusive tab then filt `notElem` exclude
                         else filt `elem` filters
                 ]

  eval ∷ Query ~> ComponentDSL State Query Message m
  eval = case _ of
      Switch    switchTo a -> a <$ raise switchTo
      ClearAll           a -> a <$ modif _{ filters = [], exclude = [] }
      SetSort   sortBy   a -> a <$ modif _{ sortBy = sortBy
                                          , sorted = getSort sortBy
                                          }
      MatchAny  matchAny a -> a <$ modif _{ matchAny = matchAny }
      Focus     focus    a -> a <$ do
          liftEffect $ hash focus
          modify_ _{ focus = focus }
      SetPref   k v      a -> a <$ do
          liftEffect $ setPreference k v
          modif <<< modPrefs $ M.insert k v
      Toggle     filt     a
        | excludes filt -> a <$ modif (modExclude $ toggleIn filt)
        | otherwise     -> a <$ modif (modFilters $ toggleIn filt)
      FilterBy   filts    a
        | any excludes filts -> a <$ modif _{ exclude = filts
                                            , filters = []
                                            , focus   = Nothing
                                            }
        | otherwise          -> a <$ modif _{ exclude = []
                                            , filters = filts
                                            , focus   = Nothing
                                            }
      where
      modif = modify_ <<< compose updateListing
      modFilters f state@{filters} = state{ filters = f filters }
      modPrefs   f state@{prefs}   = state{ prefs   = f prefs }
      modExclude f state@{exclude} = state{ exclude = f exclude }
      toggleIn x xs
        | x `elem` xs = delete x xs
        | otherwise   = cons x xs
      hash Nothing = setHash ""
      hash (Just ce) = setHash <<< urlName $ show ce

portrait ∷ ∀ a. Boolean -> Boolean -> Tuple String CraftEssence
           -> HTML a (Query Unit)
portrait big artorify (Tuple lab ce'@(CraftEssence ce))
    = H.div meta
      [ _img $ "img/CraftEssence/" <> fileName ce.name <> ".png"
      , H.header_
        <<< (lab /= "") ? append [_span $ noBreakName lab, H.br_]
        $ [ _span <<< noBreakName <<< artorify ? doArtorify $ ce.name ]
      , H.footer_
        <<< singleton <<< _span <<< S.joinWith "  " $ replicate ce.rarity "★"
      ]
  where meta       = not big ? (cons <<< _click <<< Focus $ Just ce')
                   $ [_c $ "portrait stars" <> show ce.rarity]
        doArtorify = S.replaceAll (S.Pattern "Altria") (S.Replacement "Artoria")

modal ∷ ∀ a. Boolean -> Boolean -> Maybe CraftEssence
        -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
modal _ _ Nothing = H.div [_i "layout"] <<< append
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
modal showTables artorify
(Just ce'@(CraftEssence ce@{stats:{base, max}}))
  = H.div [_i "layout", _c "fade"] <<< append
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ portrait true artorify $ Tuple "" ce'
      , _table ["", "ATK", "HP"]
        [ H.tr_ [ _th "Base",  _td $ print' base.atk,  _td $ print' base.hp ]
        , H.tr_ [ _th "Max",   _td $ print' max.atk,   _td $ print' max.hp ]
        ]
      , _h 2 "Effects"
      ] <> (if base == max then [] else
      [ H.section_ $ effectEl <$> flatten toMin ce.effect
      , _h 2 "Max Limit Break"
      ]) <>
      [ H.section_ $ effectEl <$> flatten toMax ce.effect ]
    ]

effectEl ∷ ∀ a. ActiveEffect -> HTML a (Query Unit)
effectEl ef
  | demerit ef = H.p [_c "demerit"] <<< _txt $ show ef
  | otherwise  = H.p (maybe [] meta $ activeFilter ef) <<< _txt $ show ef
  where
    meta filt = [_c "link", _click $ FilterBy [filt] ]

flatten ∷ ∀ f. Alternative f => Bind f => (Amount -> Number) -> f ActiveEffect -> f ActiveEffect
flatten f = bindFlipped go
  where
    f' a
      | a /= Full && f a == 0.0 = empty
      | otherwise = pure <<< Flat $ f a
    go (Grant a b c d) = Grant a b c <$> f' d
    go (Debuff a b c d) = Debuff a b c <$> f' d
    go (To a b c ) = To a b <$> f' c
    go (Bonus a b) = Bonus a <$> f' b
    go (Chance a b) = Chance a <$> go b
    go (Chances a b c) = Chances a b <$> go c
    go (When a b) = When a <$> go b
    go (Times a b) = Times a <$> go b
    go (ToMax a b) = ToMax (Flat $ f a) <$> go b
