-- | A simple Halogen parent component of `Site.Servants.Component` and
-- | `Site.CraftEssences.Component`.
module Site.Component (Query, comp) where

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
import Site.Algebra
import Site.Preferences
import Database
import Site.Filtering
import Sorting
import MyServant
import Printing

type ChildState a = { sortBy  :: SortBy 
                    , filters :: Array (Filter a)
                    , focus   :: Maybe a
                    }

initChild :: ∀ a. Maybe a -> ChildState a
initChild = { filters: mempty, sortBy: Rarity, focus: _ }

type State = { browseCe  :: Boolean
             , withHash  :: String
             , prefs     :: Preferences
             , team      :: Map Servant MyServant
             , ceSt      :: ChildState CraftEssence
             , servantSt :: ChildState Servant
             }

data Query a
    = BrowseServants CraftEssences.Message a
    | BrowseCraftEssences Servants.Message a

type ChildQuery = Coproduct2 Servants.Query CraftEssences.Query

type ChildSlot = Either Unit (Either Unit Unit)

-- | Halogen component.
comp :: ∀ m. MonadEffect m => String -> Preferences -> Date
     -> Map Servant MyServant -> Component HTML Query Unit Void m
comp initialHash initialPrefs today initialTeam = parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { withHash:  initialHash
                 , prefs:     initialPrefs
                 , team:      initialTeam
                 , browseCe:  initialHash == "CraftEssences" || isJust mCe
                 , servantSt: initChild mServant
                 , ceSt:      initChild mCe
                 }
    where
      fromHash :: ∀ f a. Foldable f => Show a => f a -> Maybe a
      fromHash = find (eq initialHash <<< urlName <<< show)
      mCe      = fromHash craftEssences
      mServant = fromHash servants

  ceState :: State -> CraftEssences.State -> CraftEssences.State
  ceState st = updateListing identity <<<
      _{ sortBy  = st.ceSt.sortBy
       , filters = st.ceSt.filters
       , focus   = st.ceSt.focus
       , prefs   = st.prefs 
       }

  servantState :: State -> Servants.State -> Servants.State
  servantState st = updateListing getBase <<<
      _{ sortBy   = st.servantSt.sortBy 
       , filters  = st.servantSt.filters
       , focus    = owned st.team <$> st.servantSt.focus

       , prefs    = st.prefs 
       , mineOnly = st.withHash == "MyServants"
       , team     = st.team
       , myServs  = owned st.team <$> servants
       }

  ceComp = CraftEssences.comp today
  servantComp = Servants.comp today

  render :: State -> ParentHTML Query ChildQuery ChildSlot m
  render st@{browseCe}
    | browseCe  = H.slot' CP.cp2 unit ceComp (ceState st) $ 
                  E.input BrowseServants
    | otherwise = H.slot' CP.cp1 unit servantComp (servantState st) $ 
                  E.input BrowseCraftEssences

  eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
  eval (BrowseServants (SiteMessage sortBy filters focus) a) = a <$ do
      liftEffect $ Hash.setHash "Servants"
      prefs <- liftEffect getPreferences
      team  <- liftEffect getTeam
      modify_ _{ browseCe = false
               , withHash = "Servants"
               , prefs    = prefs
               , team     = team
               , ceSt      { sortBy = sortBy, filters = filters }
               , servantSt { focus = focus }
               }
  eval (BrowseCraftEssences (SiteMessage sortBy filters focus) a) = a <$ do
      liftEffect $ Hash.setHash "CraftEssences"
      prefs <- liftEffect getPreferences
      modify_ _{ browseCe = true
               , withHash = "CraftEssences"
               , prefs    = prefs
               , ceSt      { focus = focus }
               , servantSt { sortBy = sortBy, filters = filters }
               }
