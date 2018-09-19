-- | The user interface for Craft Essences.
-- This module is only for functions that render Craft Essences to HTML.
-- Everything else goes in `Database.CraftEssence`.
module Site.CraftEssences.Component (Query, Message(..), comp) where

import StandardLibrary
import Halogen.HTML            as H
import Data.String             as String

import Data.Date (Date)
import Halogen (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML (HTML)

import Database
import Sorting
import Site.Algebra
import Site.Common
import Site.Eval
import Site.ToImage
import Site.Filtering
import Site.Preferences
import Site.CraftEssences.Filters
import Site.CraftEssences.Sorting

type Message = SiteMessage CraftEssence Servant
type Query = SiteQuery CraftEssence CraftEssence Servant
type State = SiteState CraftEssence CraftEssence ()

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

  initialState :: Unit -> State
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
