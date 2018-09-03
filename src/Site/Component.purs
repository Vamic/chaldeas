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
import Database

type State = { browseCe ∷ Boolean
             , withHash ∷ String
             , prefs    ∷ Preferences
             , mServant ∷ Maybe Servant
             , mCe      ∷ Maybe CraftEssence
             }

data Query a
    = BrowseServants (Maybe Servant) a
    | BrowseCraftEssences (Maybe CraftEssence) a

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
                 , mServant
                 , mCe
                 }
    where
      fromHash ∷ ∀ f a. Foldable f => Show a => f a -> Maybe a
      fromHash = find (eq initialHash <<< urlName <<< show)
      mCe      = fromHash craftEssences
      mServant = fromHash servants

  render :: State -> ParentHTML Query ChildQuery ChildSlot m
  render {browseCe, withHash, prefs, mServant, mCe}
    | browseCe  = H.slot' CP.cp2 unit (CraftEssences.comp mCe prefs) unit
                  $ E.input BrowseServants
    | otherwise = H.slot' CP.cp1 unit (Servants.comp mServant prefs) unit
                  $ E.input BrowseCraftEssences

  eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
  eval (BrowseServants mServant next) = next <$ do
      prefs <- liftEffect $ getPreferences
      modify_ _{ browseCe = false
               , withHash = ""
               , prefs    = prefs
               , mServant = mServant
               }
  eval (BrowseCraftEssences mCe next) = next <$ do
      prefs <- liftEffect $ getPreferences
      modify_ _{ browseCe = true
               , withHash = ""
               , prefs    = prefs
               , mCe      = mCe
               }
