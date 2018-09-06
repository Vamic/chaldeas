module Site.Component where

import Prelude
import Data.Either
import Data.Foldable
import Data.Functor.Coproduct.Nested
import Data.Maybe
import Effect.Class
import Halogen (Component, ParentDSL, ParentHTML, parentComponent, liftEffect, modify_)
import Halogen.HTML (HTML)

import Halogen.Component.ChildPath as CP
import Halogen.HTML as H
import Halogen.HTML.Events as E

import Site.Servants.Component as Servants
import Site.CraftEssences.Component as CraftEssences

import Site.Common
import Site.Preferences
import Site.Filtering
import Database

type State = { browseCe ∷ Boolean
             , withHash ∷ String
             , prefs    ∷ Preferences
             , mServant ∷ Maybe Servant
             , mCe      ∷ Maybe CraftEssence
             , fServant ∷ Array (Filter Servant)
             , fCe      ∷ Array (Filter CraftEssence)
             }

data Query a
    = BrowseServants CraftEssences.Message a
    | BrowseCraftEssences Servants.Message a

type ChildQuery = Coproduct2 Servants.Query CraftEssences.Query

type ChildSlot = Either Unit (Either Unit Unit)

comp ∷ ∀ m. MonadEffect m => String -> Preferences
       -> Component HTML Query Unit Void m
comp initialHash initialPrefs = parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { withHash: initialHash, prefs: initialPrefs
                 , browseCe: isJust mCe
                 , fServant: []
                 , fCe:      []
                 , mServant
                 , mCe
                 }
    where
      fromHash ∷ ∀ f a. Foldable f => Show a => f a -> Maybe a
      fromHash = find (eq initialHash <<< urlName <<< show)
      mCe      = fromHash craftEssences
      mServant = fromHash servants

  render :: State -> ParentHTML Query ChildQuery ChildSlot m
  render {browseCe, withHash, prefs, fServant, fCe, mServant, mCe}
    | browseCe  = H.slot' CP.cp2 unit (CraftEssences.comp fCe mCe prefs) 
                  unit $ E.input BrowseServants
    | otherwise = H.slot' CP.cp1 unit (Servants.comp fServant mServant prefs) 
                  unit $ E.input BrowseCraftEssences

  eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
  eval (BrowseServants (CraftEssences.Message fCe mServant) next) = next <$ do
      prefs <- liftEffect $ getPreferences
      modify_ _{ browseCe = false
               , withHash = ""
               , prefs    = prefs
               , fCe      = fCe
               , mServant = mServant
               }
  eval (BrowseCraftEssences (Servants.Message fServant mCe) next) = next <$ do
      prefs <- liftEffect $ getPreferences
      modify_ _{ browseCe = true
               , withHash = ""
               , prefs    = prefs
               , fServant = fServant
               , mCe      = mCe
               }
