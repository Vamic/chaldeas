module Site.Servants.Component (Query, Message(..), comp) where

import Prelude
import Operators

import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Data.Map                as M
import Data.String             as S

import Data.Tuple
import Halogen (Component, ComponentDSL, ComponentHTML, component, get, liftEffect, modify_, raise)
import Data.Array
import Data.Date
import Data.Int
import Data.Map (Map, empty)
import Data.Maybe
import Effect.Class
import Halogen.HTML (HTML)
import Routing.Hash

import Printing
import Database
import Database.MyServant
import Site.Common
import Site.Preferences
import Site.Filtering
import Site.Servants.Filters
import Site.Servants.Sorting

type Input = Unit
data Message = Message (Array (Filter Servant)) (Maybe CraftEssence)
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
    | OnTeam    Boolean MyServant a
    | MineOnly  Boolean a
    | DoNothing a

type State = { filters  :: Array (Filter Servant)
             , exclude  :: Array (Filter Servant)
             , matchAny :: Boolean
             , mineOnly :: Boolean
             , focus    :: Maybe MyServant
             , sortBy   :: SortBy
             , prefs    :: Preferences
             , ascent   :: Int
             , listing  :: Array (Tuple String Servant)
             , sorted   :: Array (Tuple String Servant)
             , team     :: Map Servant MyServant
             }

comp :: ∀ m. MonadEffect m => Array (Filter Servant) -> Maybe Servant 
     -> Preferences -> Date -> Map Servant MyServant 
     -> Component HTML Query Unit Message m
comp initialFilt initialFocus initialPrefs today initialTeam = component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  allFilters :: FilterList Servant
  allFilters = collectFilters getFilters today

  initialState :: Input -> State
  initialState = const $ updateListing 
      { filters
      , exclude
      , matchAny: true
      , mineOnly: false
      , focus:    owned initialTeam <$> initialFocus
      , sortBy:   Rarity
      , prefs:    initialPrefs
      , ascent:   1
      , sorted:   initialSort
      , listing:  initialSort
      , team:     initialTeam
      }
    where 
      initialSort = getSort empty Rarity
      {yes: exclude, no: filters} = partition (exclusive <<< getTab) initialFilt

  render :: State -> ComponentHTML Query
  render st = modal st.prefs st.ascent st.focus
      [ H.aside_ $
        [ _h 1 "Settings"
        , H.form_ $ M.toUnfoldableUnordered st.prefs <#> \(Tuple k v) -> 
          H.p [_click <<< SetPref k $ not v] $ _checkbox (show k) v
        , _h 1 "Sort by"
        , H.form_ $ enumArray <#> \sort -> 
          H.p [_click $ SetSort sort] $ _radio (show sort) (st.sortBy == sort)
        , _h 1 "Include"
        ] <> (filter (exclusive <<< fst) allFilters' >>= filterSection)
      , H.section_ <<< (if st.sortBy == Rarity then identity else reverse) $
        portrait false (pref Thumbnails) (pref Artorify) baseAscend
        <$> (st.mineOnly ? filter isMine $ st.listing)
      , H.aside_ $
        [ _h 1 "Browse"
        , _a "Craft Essences" $ Switch Nothing
        , if st.mineOnly then _a      "Servants" $ MineOnly false 
                         else _strong "Servants"
        , if st.mineOnly then _strong "My Servants" 
                         else _a      "My Servants" $ MineOnly true
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
        ] <> (filter (not exclusive <<< fst) allFilters' >>= filterSection)
      ]
    where
      pref = getPreference st.prefs
      isMine (Tuple _ s) = any (eq s <<< getBase) st.team
      baseAscend
        | pref MaxAscension = 4
        | otherwise         = 1
      allFilters'
        | st.mineOnly = allFilters <#> \(Tuple tab filts) 
                                    -> Tuple tab $ filter (\(Filter _ _ f) 
                                    -> any (f false <<< getBase) st.team) filts
        | otherwise   = allFilters
      clearAll
        | null st.filters && null st.exclude = [ P.enabled false ]
        | otherwise = [ P.enabled true, _click ClearAll ]
      filterSection (Tuple _ []) = []
      filterSection (Tuple tab filts) = 
          cons (_h 3 $ show tab) <<< 
          ( (exclusive tab && length filts > 3) ? 
              let checked = length $ filter (eq tab <<< getTab) st.exclude
              in append 
                  [ _button "All" (checked /= 0) $ Check tab true
                  , _button "None" (checked /= length filts) $ Check tab false
                  ]
          ) <<< singleton <<< H.form_ $ filts <#> \filt -> 
          H.p [_click $ Toggle filt ] <<< _checkbox (show filt) $ 
          if exclusive tab 
          then filt `notElem` st.exclude
          else filt `elem` st.filters

  eval :: Query ~> ComponentDSL State Query Message m
  eval = case _ of
      DoNothing       a -> pure a
      Ascend   ascent a -> a <$ modify_ _{ ascent = ascent }
      MatchAny match  a -> a <$ modif _{ matchAny = match }
      MineOnly mine   a -> a <$ modif _{ mineOnly = mine }
      Check t  true   a -> a <$ modif (modExclude $ filter (notEq t <<< getTab))
      ClearAll        a -> a <$ modif _{ exclude = [], filters = [] }
      Switch   switch a -> a <$ do
          {exclude, filters} <- get
          raise $ Message (exclude <> filters) switch
      SetSort  sortBy a -> a <$ modif \st -> st{ sortBy = sortBy
                                               , sorted = getSort st.team sortBy
                                               }
      Check t  false  a -> a <$ 
          modif (modExclude $ nub <<< append (getFilters today t))
      Focus    focus  a -> a <$ do
          liftEffect $ hash focus
          modify_ \st -> st{ focus = owned st.team <$> focus, ascent = 1 }
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
      SetPref  k v    a -> a <$ do
          liftEffect $ setPreference k v
          modif (modPrefs $ M.insert k v)
      Toggle   filt   a
        | exclusive $ getTab filt -> a <$ modif (modExclude $ toggleIn filt)
        | otherwise               -> a <$ modif (modFilters $ toggleIn filt)
      OnTeam keep myServant a -> a <$  do
          {team}        <- get
          let myServant' = keep ? recalc $ myServant
              team'      = if keep 
                           then M.insert (getBase myServant) myServant team
                           else M.delete (getBase myServant) team
          liftEffect $ setTeam team'
          modif \st -> st{ team   = team'
                         , sorted = getSort team' st.sortBy
                         , focus  = st.focus *> Just myServant'
                         }
    where
      modif = modify_ <<< compose updateListing
      modFilters f st = st{ filters = f st.filters }
      modExclude f st = st{ exclude = f st.exclude }
      modPrefs   f st = st{ prefs   = f st.prefs }
      toggleIn x xs
        | x `elem` xs = delete x xs
        | otherwise   = cons x xs
      hash Nothing = setHash ""
      hash (Just s) = setHash <<< urlName $ show s

portrait :: ∀ a. Boolean -> Boolean -> Boolean -> Int -> Tuple String Servant
         -> HTML a (Query Unit)
portrait big thumbnails artorify ascension (Tuple lab s'@(Servant s))
  | thumbnails && not big = H.div [_c "thumb", _click <<< Focus $ Just s']
    [ _img $ "img/Servant/" <> fileName s.name <> " Thumbnail.png" ]
  | otherwise = H.div meta
      [ _img $ "img/Servant/" <> fileName s.name <> ascent <> ".png"
      , H.div_ [ _img $ "img/Class/" <> show s.class <> ".png"]
      , H.header_ <<< (lab /= "") ? append [_span lab, H.br_] $
        [ _span <<< noBreakName big <<< artorify ? doArtorify $ s.name ]
      , H.footer_ <<< 
        ((big && ascension > 1) ? cons prevAscend) <<<
        ((big && ascension < 4) ? (_ `snoc` nextAscend)) $
        [_span <<< S.joinWith "  " $ replicate s.rarity "★"]
      ]
  where 
    meta       = not big ? (cons <<< _click <<< Focus $ Just s') $
                 [_c $ "portrait stars" <> show s.rarity]
    doArtorify = S.replaceAll (S.Pattern "Altria") (S.Replacement "Artoria")
    prevAscend = _a "<" <<< Ascend $ ascension - 1
    nextAscend = _a ">" <<< Ascend $ ascension + 1
    ascent
      | ascension <= 1 = ""
      | otherwise      = " " <> show ascension

modal :: ∀ a. Preferences -> Int -> Maybe MyServant
      -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
modal prefs _ Nothing = H.div [_c $ mode prefs] <<< append
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
modal prefs ascent (Just ms')= H.div 
  [_c $ "fade " <> mode prefs] <<< append
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ portrait true (pref Thumbnails) (pref Artorify) ascent $ Tuple "" s'
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
        , _tr "NP Type"     [ link FilterPhantasm <<< fromMaybe Support $
                              find (\t -> has t false s') 
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
        , _tr "Stars/Quick" <<< _txt $ print 2 (starsPerQuick s')
        , _tr "NP/Arts"     <<< _txt $ print 2 (npPerArts s') <> "%"
        ]
      , H.form [_i "myservant"] myServantBox
      , _h 2 "Noble Phantasm"
      , H.table [_i "phantasm"]
        [ _tr "Name" <<< _txt $ s.phantasm.name
        , _tr "Rank" <<< _txt $ show s.phantasm.rank
        , _tr "Card" $ [ link FilterCard s.phantasm.card ]
        , _tr "Class" <<< _txt $ s.phantasm.kind
        , H.tr_
          [ _th "Effects"
          , H.td_ <<< showTables ? flip snoc
              ( _table (append "NP" <<< show <$> 1..5) $
                npRow <$> nub (ranges b.phantasm.effect)
              ) $ effectEl <$> s.phantasm.effect
          ]
        , H.tr_
          [ _th "Overcharge"
          , H.td overMeta <<< showTables ? flip snoc
              ( _table ((_ <> "%") <<< show <<< (_ * 100) <$> 1..5) $
                overRow <$> nub (ranges b.phantasm.over)
              ) $ effectEl <$> s.phantasm.over
          ]
        ]
      , _h 2 "Active Skills"] 
      <> (zipWith (activeEl showTables) s.actives b.actives) <>
      [ _h 2 "Passive Skills"] <> (passiveEl <$> s.passives) <>
      [ _h 2 "Max-Bond Craft Essence"
      , bondEl $ getBond s'
      , _h 2 "Traits"
      , H.section_ $ traitEl <$> s.traits
      ]
    ]
  where
    MyServant ms@{servant:s'@(Servant s), base:(Servant b)} = ms'
    Tuple alignA alignB = s.align
    {base} = s.stats
    {max, grail} = b.stats
    pref = getPreference prefs
    showTables = pref ShowTables
    overMeta
      | s.phantasm.first = [_c "activates"]
      | otherwise        = []
    alter f = OnTeam true <<< MyServant $ f ms
    _mInt = _int DoNothing
    skillBox (Tuple {icon} lvl) i = 
        [ H.td_ [_img $ "img/Skill/" <> show icon <> ".png"]
        , H.td_ $ _mInt 1 10 lvl \val -> 
          alter _{ skills = maybeDo (updateAt i val) ms.skills }
        ]
    myServantBox
      | ms.level == 0 = 
            [ _a "+Add to My Servants" <<< OnTeam true $ newServant s' ]
      | otherwise = 
            [ H.table_
              [ H.tr_
                [ H.td_ [_strong "Level:"]
                , H.td_ $ _mInt 1 100 ms.level   \val -> alter _{level = val}
                , H.td_ [_strong "NP:"]
                , H.td_ $ _mInt 1 5 ms.npLvl     \val -> alter _{npLvl = val}
                , H.td_ [_strong "+ATK:"]
                , H.td_ $ _mInt 0 990 ms.fou.atk \val -> alter _{fou{atk = val}}
                , H.td_ [_strong "+HP:"]
                , H.td_ $ _mInt 0 990 ms.fou.hp  \val -> alter _{fou{hp = val}}
                ]
              , H.tr_ <<< append 
                [ H.td_ [ _a "Delete" <<< OnTeam false $ unowned s' ]
                , H.td_ [_strong "Skills:"]
                ] <<< join $ zipWith skillBox (zip s.actives ms.skills) (0..2)
              ]
            ]

activeEl :: ∀ a. Boolean -> Active -> Active -> HTML a (Query Unit)
activeEl showTables active@{name, icon, cd, effect} base = 
    H.section_ <<< showTables ? flip snoc effectTable $
    [ _img $ "img/Skill/" <> show icon <> ".png"
    , _h 3 name
    , _strong "CD: "
    , H.text <<< (active == base) ? (_ <> "~" <> show (cd - 2)) $ show cd
    ] <> (effectEl <$> effect)
  where
    effectTable = _table (show <$> 1..10) $ lvlRow <$> nub (ranges base.effect)

passiveEl :: ∀ a. Passive -> HTML a (Query Unit)
passiveEl {name, rank, icon, effect} = H.section_ $
    [ _img $ "img/Skill/" <> show icon <> ".png"
    , H.h3_
      [ H.span [_c "link", _click $ FilterBy [passiveFilter name] ] $ _txt name
      , H.text $ " " <> show rank
      ]
    ] <> (_p <<< show <$> effect)

bondEl :: ∀ a. Maybe CraftEssence -> HTML a (Query Unit)
bondEl Nothing = H.section_ $ _txt "N/A"
bondEl ce@(Just (CraftEssence {name, icon, effect})) = H.section_ $
    [ _img $ "img/Skill/" <> show icon <> ".png"
    , H.h3 [_c "link", _click $ Switch ce] $ _txt name
    , H.p_
      [                  _span "★★★★ "
      , _strong "ATK: ", _span "100 "
      , _strong "DEF: ", _span "100"
      ]
    ] <> (_p <<< show <<< nvmEquipped <$> effect)
  where
    nvmEquipped (When _ ef) = ef
    nvmEquipped ef = ef

effectEl :: ∀ a. ActiveEffect -> HTML a (Query Unit)
effectEl ef
  | demerit ef = H.p [_c "demerit"] <<< _txt $ show ef
  | otherwise  = H.p (maybe [] meta $ activeFilter ef) <<< _txt $ show ef
  where
    meta filt = [_c "link", _click $ FilterBy [filt]]

traitEl :: ∀ a. Trait -> HTML a (Query Unit)
traitEl trait = 
    H.a 
    [_c "trait link", _click <<< FilterBy $ singleFilter FilterTrait trait]   
    [H.text $ show trait]

npRow :: ∀ a b. RangeInfo -> HTML a b
npRow (RangeInfo isPercent a b) = 
    H.tr_ $ toCell isPercent <<< (_ + a) <<< (_ * over) 
    <$> [0.0, 0.5, 0.75, 0.825, 1.0]
  where
    over = b - a

overRow :: ∀ a b. RangeInfo -> HTML a b
overRow (RangeInfo isPercent a b) = 
    H.tr_ $ toCell isPercent <<< (_ + a) <<< (_ * over) <<< toNumber <$> (0..4)
  where
    over = (b - a) / 4.0

link :: ∀ a b. MatchServant a => FilterTab -> a -> HTML b (Query Unit)
link tab a = 
    H.a 
    [_c "link", _click <<< FilterBy $ singleFilter tab a]
    [H.text $ show a]
