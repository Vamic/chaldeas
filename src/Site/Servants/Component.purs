module Site.Servants.Component (Query, comp) where

import Prelude
import Operators

import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Data.Map                as M
import Data.String             as S

import Data.Tuple
import Halogen (Component, ComponentDSL, ComponentHTML, component, liftEffect, modify_, raise)
import Data.Array
import Data.Int
import Data.Maybe
import Effect.Class
import Halogen.HTML (HTML)
import Routing.Hash

import Database
import Site.Common
import Site.Preferences
import Site.Filtering
import Site.Servants.Filters
import Site.Servants.Sorting

type Input = Unit
type Message = Maybe CraftEssence
data Query a
    = Switch    (Maybe CraftEssence) a
    | Focus     (Maybe Servant) a
    | ClearAll  a
    | Check     FilterTab Boolean a
    | FilterBy  (Array (Filter Servant)) a
    | Toggle    (Filter Servant) a
    | MatchAny  Boolean a
    | SetSort   SortBy a
    | SetPref   Preference Boolean a
    | Ascend    Int a

type State = { filters  ∷ Array (Filter Servant)
             , exclude  ∷ Array (Filter Servant)
             , matchAny ∷ Boolean
             , focus    ∷ Maybe Servant
             , sortBy   ∷ SortBy
             , prefs    ∷ Preferences
             , ascend   ∷ Int
             , listing  ∷ Array (Tuple String Servant)
             , sorted   ∷ Array (Tuple String Servant)
             }

comp ∷ ∀ m. MonadEffect m => Maybe Servant -> Preferences
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
                       , ascend:   1
                       , sorted:   initialSort
                       , listing:  initialSort
                       }
    where initialSort = getSort Rarity

  render ∷ State -> ComponentHTML Query
  render {ascend, exclude, filters, focus, listing, matchAny, prefs, sortBy}
      = modal prefs ascend focus
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
          $ portrait false (pref Thumbnails) (pref Artorify) baseAscend 
            <$> listing
        , H.aside_ $
          [ _h 1 "Browse"
          , H.a_ <<< singleton <<< H.strong_ $ _txt "Servants"
          , H.a [_click $ Switch Nothing, P.href "#"] $ _txt "Craft Essences"
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
      pref = getPreference prefs
      baseAscend
        | pref MaxAscension = 4
        | otherwise         = 1
      clearAll
        | null filters && null exclude = [ P.enabled false ]
        | otherwise                    = [ P.enabled true, _click ClearAll ]
      filterSection tab = case getFilters tab of
        []    -> []
        filts -> cons (_h 3 $ show tab)
                 <<< ((exclusive tab && length filts > 3) ? 
                 let checked = length $ filter (eq tab <<< getTab) exclude in
                 append 
                   [ _button "All" (checked /= 0) $ Check tab true
                   , _button "None" (checked /= length filts) $ Check tab false
                  ])
                 <<< singleton <<< H.form_ $ filts <#> \filt
                     -> H.p [_click $ Toggle filt ]
                      <<< _checkbox (show filt)
                       $ if exclusive tab then filt `notElem` exclude
                         else filt `elem` filters

  eval ∷ Query ~> ComponentDSL State Query Message m
  eval = case _ of
      Switch   switch a -> a <$ raise switch
      ClearAll        a -> a <$ modif _{ filters = [], exclude = [] }
      SetSort  sortBy a -> a <$ modif _{ sortBy = sortBy
                                       , sorted = getSort sortBy
                                       }
      Ascend   ascend a -> a <$ modify_ _{ ascend = ascend }
      MatchAny match  a -> a <$ modif _{ matchAny = match }
      Check t  true   a -> a <$ modif (modExclude $ filter (notEq t <<< getTab))
      Check t  false  a -> a <$ modif (modExclude $ nub <<< append (getFilters t))
      Focus    focus  a -> a <$ do
          liftEffect $ hash focus
          modify_ _{ focus = focus, ascend = 1 }
      SetPref  k v    a -> a <$ do
          liftEffect $ setPreference k v
          modif (modPrefs $ M.insert k v)
      Toggle   filt   a
        | exclusive $ getTab filt -> a <$ modif (modExclude $ toggleIn filt)
        | otherwise               -> a <$ modif (modFilters $ toggleIn filt)
      FilterBy filts  a
        | any (exclusive <<< getTab) filts -> a <$ modif _{ exclude = filts
                                                          , filters = []
                                                          , focus   = Nothing
                                                          }
        | otherwise                        -> a <$ modif _{ exclude = []
                                                          , filters = filts
                                                          , focus   = Nothing
                                                          }
    where
      modif = modify_ <<< compose updateListing
      modFilters f state@{filters} = state{ filters = f filters }
      modExclude f state@{exclude} = state{ exclude = f exclude }
      modPrefs   f state@{prefs}   = state{ prefs   = f prefs }
      toggleIn x xs
        | x `elem` xs = delete x xs
        | otherwise   = cons x xs
      hash Nothing = setHash ""
      hash (Just s) = setHash <<< urlName $ show s

portrait ∷ ∀ a. Boolean -> Boolean -> Boolean -> Int -> Tuple String Servant
           -> HTML a (Query Unit)
portrait big thumbnails artorify ascension (Tuple lab s'@(Servant s))
  | thumbnails && not big = H.div [_c "thumb", _click <<< Focus $ Just s']
    [ _img $ "img/Servant/" <> fileName s.name <> " Thumbnail.png" ]
  | otherwise = H.div meta
      [ _img $ "img/Servant/" <> fileName s.name <> ascend <> ".png"
      , H.div_ [ _img $ "img/Class/" <> show s.class <> ".png"]
      , H.header_
        <<< (lab /= "") ? append [_span lab, H.br_]
        $ [ _span <<< noBreakName <<< artorify ? doArtorify $ s.name ]
      , H.footer_
        <<< ((big && ascension > 1) ? cons prevAscend)
        <<< ((big && ascension < 4) ? (_ `snoc` nextAscend))
        <<< singleton <<< _span <<< S.joinWith "  " $ replicate s.rarity "★"
      ]
  where 
    meta       = not big ? (cons <<< _click <<< Focus $ Just s')
               $ [_c $ "portrait stars" <> show s.rarity]
    doArtorify = S.replaceAll (S.Pattern "Altria") (S.Replacement "Artoria")
    prevAscend = H.a [_click <<< Ascend $ ascension - 1] $ _txt "<"
    nextAscend = H.a [_click <<< Ascend $ ascension + 1] $ _txt ">"
    ascend
      | ascension <= 1 = ""
      | otherwise      = " " <> show ascension

modal ∷ ∀ a. Preferences -> Int -> Maybe Servant
        -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
modal prefs _ Nothing = H.div [_i "layout", _c $ mode prefs] <<< append
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
modal prefs ascend
(Just s'@(Servant s@{align: Tuple alignA alignB, stats:{base, max, grail}}))
  = H.div [_i "layout", _c $ "fade " <> mode prefs] <<< append
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ portrait true (pref Thumbnails) (pref Artorify) ascend $ Tuple "" s'
      , _table ["", "ATK", "HP"]
        [ H.tr_ [ _th "Base",  _td $ print' base.atk,  _td $ print' base.hp ]
        , H.tr_ [ _th "Max",   _td $ print' max.atk,   _td $ print' max.hp ]
        , H.tr_ [ _th "Grail", _td $ print' grail.atk, _td $ print' grail.hp ]
        ]
      , _table ["", "Q", "A", "B", "EX", "NP"]

        [ H.tr_
          [ _th "Hits"
          , _td $ show s.hits.quick
          , _td $ show s.hits.arts
          , _td $ show s.hits.buster
          , _td $ show s.hits.ex
          , _td $ show s.phantasm.hits
          ]
        ]
      , H.table_
        [ _tr "Class"       [ link FilterClass s.class ]
        , _tr "Deck"        [ link FilterDeck s.deck ]
        , _tr "NP Type"     [ link FilterPhantasm <<< fromMaybe Support
                            $ find (\t -> has t false s')
                              [SingleTarget, MultiTarget]
                            ]
        , _tr "Alignment"   $ [ link FilterAlignment alignA
                              , H.text " "
                              , link FilterAlignment alignB
                              ]
        , _tr "Attribute"   $ [ link FilterAttribute s.attr ]
        , _tr "Star Weight" <<< _txt $ show s.gen.starWeight
        , _tr "Star Rate"   <<< _txt $ show s.gen.starRate <> "%"
        , _tr "NP/Hit"      <<< _txt $ show s.gen.npAtk <> "%"
        , _tr "NP/Defend"   <<< _txt $ show s.gen.npDef <> "%"
        --, _tr "Death Rate"  <<< _txt $ show s.death
        , _tr "Bond CE"     [ bondCe ]
        ]
      , H.h2 [_c "clearhead"] $ _txt "Noble Phantasm"
      , H.table [_c "phantasm"]
        [ _tr "Name" <<< _txt $ s.phantasm.name
        , _tr "Rank" <<< _txt $ show s.phantasm.rank
        , _tr "Card" $ [ link FilterCard s.phantasm.card ]
        , _tr "Class" <<< _txt $ s.phantasm.kind
        , H.tr_
          [ _th "Effects"
          , H.td_ <<< showTables ? (flip snoc)
              ( _table (append "NP" <<< show <$> 1..5)
                $ npRow <$> nub (ranges s.phantasm.effect)
              ) $ effectEl <$> s.phantasm.effect
          ]
        , H.tr_
          [ _th "Overcharge"
          , H.td overMeta <<< showTables ? (flip snoc)
              (_table ((_ <> "%") <<< show <<< (_ * 100) <$> 1..5)
                $ overRow <$> nub (ranges s.phantasm.over)
              )
            $ effectEl <$> s.phantasm.over
          ]
        ]
      , _h 2 "Active Skills"] <> (activeEl showTables <$> s.actives) <>
      [ _h 2 "Passive Skills"] <> (passiveEl <$> s.passives) <>
      [ _h 2 "Traits"
      , H.section_ $ traitEl <$> s.traits
      ]
    ]
  where
    pref = getPreference prefs
    showTables = pref ShowTables
    gotBond = getBond s'
    bondCe = case gotBond of
        Just (CraftEssence ce) -> 
            H.span [_c "link", _click $ Switch gotBond] $ _txt ce.name
        Nothing -> _span "N/A"
    overMeta
      | s.phantasm.first = [_c "activates"]
      | otherwise        = []

activeEl ∷ ∀ a. Boolean -> Active -> HTML a (Query Unit)
activeEl showTables {name, icon, cd, effect} = H.section_
    <<< showTables ? (flip snoc)
      (_table (show <$> 1..10) $ lvlRow <$> nub (ranges effect)) $
    [ _img $ "img/Skill/" <> show icon <> ".png"
    , _h 3 name
    , H.span_
      [  H.strong_ [ H.text "CD: "]
      , H.text $ show cd <> "~" <> show (cd - 2)
      ]
    ] <> (effectEl <$> effect)

passiveEl ∷ ∀ a. Passive -> HTML a (Query Unit)
passiveEl {name, rank, icon, effect} = H.section_ $
    [ _img $ "img/Skill/" <> show icon <> ".png"
    , H.h3_
      [ H.span [_c "link", _click $ FilterBy [passiveFilter name] ] $ _txt name
      , H.text $ " " <> show rank
      ]
    ] <> (_p <<< show <$> effect)

effectEl ∷ ∀ a. ActiveEffect -> HTML a (Query Unit)
effectEl ef
  | demerit ef = H.p [_c "demerit"] <<< _txt $ show ef
  | otherwise  = H.p (maybe [] meta $ activeFilter ef) <<< _txt $ show ef
  where
    meta filt = [_c "link", _click $ FilterBy [filt]]

traitEl ∷ ∀ a. Trait -> HTML a (Query Unit)
traitEl trait = H.span
    [_c "trait link", _click <<< FilterBy $ singleFilter FilterTrait trait]
    <<< _txt $ show trait

npRow ∷ ∀ a b. RangeInfo -> HTML a b
npRow (RangeInfo isPercent a b) = H.tr_
          $ toCell isPercent <<< (_ + a) <<< (_ * over)
         <$> [0.0, 0.5, 0.75, 0.825, 1.0]
  where
    over = b - a

overRow ∷ ∀ a b. RangeInfo -> HTML a b
overRow (RangeInfo isPercent a b) = H.tr_
          $ toCell isPercent <<< (_ + a) <<< (_ * over) <<< toNumber
         <$> 0..4
  where
    over = (b - a) / 4.0

link ∷ ∀ a b. MatchServant a => FilterTab -> a -> HTML b (Query Unit)
link tab a = H.span [_c "link", _click <<< FilterBy $ singleFilter tab a]
            <<< _txt $ show a
