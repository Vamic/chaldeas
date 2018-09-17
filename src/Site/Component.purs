-- | A simple Halogen parent component of `Site.Servants.Component` and 
-- | `Site.CraftEssences.Component`.
module Site.Component where

import StandardLibrary
import Halogen.Component.ChildPath as CP
import Halogen.HTML.Events         as E
import Halogen.HTML                as H
import Routing.Hash                as Hash


import Data.Date (Date)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Halogen (Component, ParentDSL, ParentHTML, parentComponent, modify_)
import Halogen.HTML (HTML)


import Site.Servants.Component      as Servants
import Site.CraftEssences.Component as CraftEssences
import Site.Common
import Site.Preferences
import Site.Filtering
import Database
import Database.MyServant
import Printing

type State = { today    :: Date
             , browseCe :: Boolean
             , withHash :: String
             , prefs    :: Preferences
             , team     :: Map Servant MyServant
             , mServant :: Maybe Servant
             , mCe      :: Maybe CraftEssence
             , fServant :: Array (Filter Servant)
             , fCe      :: Array (Filter CraftEssence)
             }

data Query a
    = BrowseServants CraftEssences.Message a
    | BrowseCraftEssences Servants.Message a

type ChildQuery = Coproduct2 Servants.Query CraftEssences.Query

type ChildSlot = Either Unit (Either Unit Unit)

comp :: ∀ m. MonadEffect m => String -> Preferences -> Date 
     -> Map Servant MyServant -> Component HTML Query Unit Void m
comp initialHash initialPrefs initialToday initialTeam = parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { today:    initialToday
                 , withHash: initialHash
                 , prefs:    initialPrefs
                 , team:     initialTeam
                 , browseCe: isJust mCe
                 , fServant: []
                 , fCe:      []
                 , mServant
                 , mCe
                 }
    where
      fromHash :: ∀ f a. Foldable f => Show a => f a -> Maybe a
      fromHash = find (eq initialHash <<< urlName <<< show)
      mCe      = fromHash craftEssences
      mServant = fromHash servants

  render :: State -> ParentHTML Query ChildQuery ChildSlot m
  render {browseCe, withHash, prefs, fServant, fCe, mServant, mCe, team, today}
    | browseCe  = H.slot' CP.cp2 unit 
                  (CraftEssences.comp fCe mCe prefs today) 
                  unit $ E.input BrowseServants
    | otherwise = H.slot' CP.cp1 unit 
                  (Servants.comp fServant mServant prefs today team) 
                  unit $ E.input BrowseCraftEssences

  eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
  eval (BrowseServants (CraftEssences.Message fCe mServant) next) = next <$ do
      liftEffect $ Hash.setHash ""
      prefs <- liftEffect getPreferences
      today <- liftEffect getDate
      team  <- liftEffect getTeam
      modify_ _{ browseCe = false
               , withHash = ""
               , prefs    = prefs
               , team     = team
               , today    = today
               , fCe      = fCe
               , mServant = mServant
               }
  eval (BrowseCraftEssences (Servants.Message fServant mCe) next) = next <$ do
      liftEffect $ Hash.setHash ""
      prefs <- liftEffect getPreferences
      today <- liftEffect getDate
      modify_ _{ browseCe = true
               , withHash = ""
               , prefs    = prefs
               , today    = today
               , fServant = fServant
               , mCe      = mCe
               }
