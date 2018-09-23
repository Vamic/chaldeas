-- | The user interface for Craft Essences.
-- This module is only for functions that render Craft Essences to HTML.
-- Everything else goes in `Database.CraftEssence`.
module Site.CraftEssences.Component (Query, Message(..), comp) where

import StandardLibrary
import Halogen.HTML as H
import Data.String  as String

import Data.Date (Date)
import Halogen (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML (HTML)

import Database
import Printing
import Site.Algebra
import Site.Common
import Site.CraftEssences.Filters
import Site.CraftEssences.Sorting
import Site.Eval
import Site.Filtering
import Site.Preferences
import Site.ToImage
import Sorting

type Message = SiteMessage CraftEssence Servant
type Query = SiteQuery CraftEssence CraftEssence Servant
type State = SiteState CraftEssence CraftEssence ()

-- | Halogen component.
comp :: ∀ m. MonadEffect m => Array (Filter CraftEssence) -> Maybe CraftEssence
     -> Preferences -> Date -> Component HTML Query Unit Message m
comp initialFilt initialFocus initialPrefs today = component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  allFilters :: FilterList CraftEssence
  allFilters = collectFilters getFilters today

  initialState :: State
  initialState =  updateListing identity
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
  render st = modal st.prefs st.focus <<<
              outline st [Rarity, ID, ATK, HP] allFilters nav $
              portrait false st.prefs <$> st.listing
    where
      nav = [ _strong "Craft Essences", _a "Servants" $ Switch Nothing ]

  eval :: Query ~> ComponentDSL State Query Message m
  eval = siteEval "CraftEssences" identity (getFilters today) getSort

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
        [ H.tr_ [ _th "Base",  _td $ commas base.atk,  _td $ commas base.hp ]
        , H.tr_ [ _th "Max",   _td $ commas max.atk,   _td $ commas max.hp ]
        ]
      , toImage ce.icon
      , _h 2 "Effects"
      ] <> mlbEl <>
      [ effectsEl \_ y -> Flat y ]
    ]
  where
    mlbEl
      | base == max = []
      | otherwise   = [ effectsEl \x _ -> Flat x, _h 2 "Max Limit Break" ]
    effectsEl f = H.section_ <<< append bondMsg $
                  effectEl <<< mapAmount f <$> ce.effect
    bondLink bond = _click <<< Switch $ find (eq bond <<< show) servants
    bondMsg = case ce.bond of
                  Nothing   -> []
                  Just bond -> [ H.em_
                                 [ H.text "If equipped by "
                                 , H.a [_c "link", bondLink bond] [H.text bond]
                                 , H.text ": "
                                 ]
                               ]

effectEl :: ∀ a. SkillEffect -> HTML a (Query Unit)
effectEl ef
  | demerit ef = H.p [_c "demerit"] [H.text $ show ef]
  | otherwise  = H.p (maybe [] meta $ skillFilter ef) [H.text $ show ef]
  where
    meta filt = [_c "link", _click $ FilterBy [filt] ]
