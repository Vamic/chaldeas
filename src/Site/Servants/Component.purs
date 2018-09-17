-- | The user interface for Servants and My Servants.
-- This module is only for functions that render Servants to HTML.
-- Everything else goes in `Database.Servant` and `MyServant`.
module Site.Servants.Component (Query, Message(..), comp) where

import StandardLibrary
import Data.Array              as Array
import Halogen.HTML            as H
import Routing.Hash            as Hash
import Data.Int                as Int
import Data.Map                as Map
import Halogen.HTML.Properties as P
import Data.String             as String

import Halogen (Component, ComponentDSL, ComponentHTML, component, get, modify_, raise)
import Halogen.HTML (HTML)
import Data.Date (Date)

import Database
import MyServant
import MyServant.Sorting
import Site.Common
import Site.ToImage
import Site.Preferences
import Site.Filtering
import Site.Servants.Filters
import Site.Servants.Sorting
import Printing

type Input = Unit
data Message = Message (Array (Filter Servant)) (Maybe CraftEssence)
data Query a
    = Switch    (Maybe CraftEssence) a
    | Focus     (Maybe MyServant) a
    | ClearAll  a
    | Check     FilterTab Boolean a
    | FilterBy  (Array (Filter Servant)) a
    | Toggle    (Filter Servant) a
    | MatchAny  Boolean a
    | SetSort   SortBy a
    | SetPref   Preference Boolean a
    | Ascend    MyServant Int a
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
             , myServs  :: Array MyServant
             , sorted   :: Array { label :: String, obj :: MyServant }
             , listing  :: Array { label :: String, obj :: MyServant }
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
  initialState = const $ updateListing getBase
      { filters
      , exclude
      , matchAny: true
      , mineOnly: false
      , focus:    owned initialTeam <$> initialFocus
      , sortBy:   Rarity
      , prefs:    initialPrefs
      , ascent:   1
      , myServs:  initialMyServs
      , sorted:   initialSort
      , listing:  initialSort
      , team:     initialTeam
      }
    where
      initialMyServs = owned initialTeam <$> servants
      initialSort = getSort Rarity initialMyServs
      {yes: exclude, no: filters} = partition (exclusive <<< getTab)
                                    initialFilt

  render :: State -> ComponentHTML Query
  render st = modal st.prefs st.ascent st.focus
      [ H.aside_ $
        [ _h 1 "Settings"
        , H.form_ $ unfoldPreferences st.prefs <#> \(k ^ v) ->
          H.p [_click <<< SetPref k $ not v] $ _checkbox Nothing (show k) v
        , _h 1 "Sort by"
        , H.form_ $ enumArray <#> \sort ->
          H.p [_click $ SetSort sort] $ _radio (show sort) (st.sortBy == sort)
        , _h 1 "Include"
        ] <> (filter (exclusive <<< _.tab) allFilters' >>= filtersEl)
      , H.section_ <<< maybeReverse $
        doPortrait <$> (st.mineOnly ? filter isMine $ st.listing)
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
        ] <>
          (filter (not exclusive <<< _.tab) allFilters' >>= filtersEl)
      ]
    where
      maybeReverse = case st.sortBy of
          Rarity -> identity
          _      -> reverse
      doPortrait {label: "", obj: ms}
        | st.mineOnly = portrait false st.prefs baseAscend { label: showStats ms, obj: ms }
      doPortrait info = portrait false st.prefs baseAscend info
      baseAscend
        | prefer st.prefs MaxAscension = 4
        | otherwise                    = 1
      allFilters'
        | st.mineOnly = allFilters <#> \{tab, filters} ->
                        { tab
                        , filters: filter (\(Filter x) ->
                              any (x.match false <<< getBase) st.team) filters
                        }
        | otherwise   = allFilters
      clearAll
        | null st.filters && null st.exclude = [ P.enabled false ]
        | otherwise = [ P.enabled true, _click ClearAll ]
      filtersEl { tab, filters } = filterSection Check Toggle st tab filters

  eval :: Query ~> ComponentDSL State Query Message m
  eval = case _ of
      Ascend (MyServant {level: 0}) ascent a -> a <$
          modify_ _{ ascent = ascent }
      Ascend (MyServant ms) ascent a -> eval $
          OnTeam true (MyServant ms{ascent = ascent}) a
      DoNothing         a -> pure a
      MatchAny matchAny a -> a <$ modif   _{ matchAny = matchAny }
      MineOnly mineOnly a -> a <$ modif   _{ mineOnly = mineOnly }
      ClearAll          a -> a <$ modif   _{ exclude = [], filters = [] }
      Check t  true     a -> a <$ do
          modif <<< modExclude <<< filter $ notEq t <<< getTab
      Check t  false    a -> a <$
          modif (modExclude $ nub <<< append (getFilters today t))
      Switch   switch   a -> a <$ do
          {exclude, filters} <- get
          raise $ Message (exclude <> filters) switch
      SetSort  sortBy   a -> a <$ modif \st ->
          st{ sortBy = sortBy
            , sorted = getSort sortBy st.myServs
            }
      Focus    focus    a -> a <$ do
          liftEffect $ hash focus
          modify_ _{ focus = focus, ascent = 1 }
      FilterBy filts    a -> a <$ do
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
      SetPref  k v      a -> a <$ do
          liftEffect $ writePreference k v
          modif (modPrefs $ setPreference k v)
      Toggle   filt     a
        | exclusive $ getTab filt -> a <$ modif (modExclude $ toggleIn filt)
        | otherwise               -> a <$ modif (modFilters $ toggleIn filt)
      OnTeam keep myServant a -> a <$  do
          {team}        <- get
          let myServant' = keep ? recalc $ myServant
              team'      = if keep
                           then Map.insert (getBase myServant') myServant' team
                           else Map.delete (getBase myServant') team
              myServs    = owned team' <$> servants
          liftEffect $ setTeam team'
          modif \st -> st{ team    = team'
                         , myServs = myServs
                         , sorted  = getSort st.sortBy myServs
                         , focus   = st.focus *> Just myServant'
                         }
    where
      modif = modify_ <<< compose (updateListing getBase)
      modFilters f st = st{ filters = f st.filters }
      modExclude f st = st{ exclude = f st.exclude }
      modPrefs   f st = st{ prefs   = f st.prefs }
      toggleIn x xs
        | x `elem` xs = delete x xs
        | otherwise   = x : xs
      hash Nothing  = Hash.setHash "Servants"
      hash (Just s) = Hash.setHash <<< urlName <<< show $ getBase s

showStats :: MyServant -> String
showStats (MyServant ms) = show ms.level <> "/" <> show (maxLevel ms.servant)
                           <> " " <> String.joinWith "·" (show <$> ms.skills)

isMine :: ∀ a. { obj :: MyServant | a } -> Boolean
isMine  {obj: MyServant {level: 0}} = false
isMine _                            = true

portrait :: ∀ a. Boolean -> Preferences -> Int
         -> { label :: String, obj :: MyServant } -> HTML a (Query Unit)
portrait big prefs baseAscension { label, obj: ms' }
  | not big && prefer prefs Thumbnails =
      H.div [_c "thumb", _click <<< Focus $ Just ms' ]
      [ toThumbnail ms' ]
  | otherwise =
      H.div meta
      [ _img $ "img/Servant/" <> fileName s.name <> ascent <> ".png"
      , H.div_ [ toImage s.class ]
      , H.header_ <<< (label /= "") ? append [_span label, H.br_] $
        [ _span <<< noBreakName big $ artorify s.name ]
      , H.footer_ <<<
        ((big && ascension > 1) ? cons prevAscend) <<<
        ((big && ascension < 4) ? consAfter nextAscend) $
        [_span <<< String.joinWith "  " $ replicate s.rarity "★"]
      ]
  where
    MyServant ms@{servant:Servant s} = ms'
    artorify   = prefer prefs Artorify ?
                 String.replaceAll (Pattern "Altria") (Replacement "Artoria")
    meta       = not big ? (cons <<< _click <<< Focus $ Just ms') $
                 [_c $ "portrait stars" <> show s.rarity]
    ascension = case ms.level of
        0 -> baseAscension
        _ -> ms.ascent
    prevAscend = _a "<" <<< Ascend ms' $ ascension - 1
    nextAscend = _a ">" <<< Ascend ms' $ ascension + 1
    ascent
      | ascension <= 1 = ""
      | otherwise      = " " <> show ascension

modal :: ∀ a. Preferences -> Int -> Maybe MyServant
      -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
modal prefs _ Nothing = H.div [_c $ mode prefs] <<< append
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
modal prefs ascent focus@(Just ms') = H.div
  [_c $ "fade " <> mode prefs] <<< append
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ portrait true prefs ascent { label: "", obj: ms' }
      , _table ["", "ATK", "HP"]
        [ H.tr_ [ _th "Base",  _td $ places' base.atk,  _td $ places' base.hp ]
        , H.tr_ [ _th "Max",   _td $ places' max.atk,   _td $ places' max.hp ]
        , H.tr_ [ _th "Grail", _td $ places' grail.atk, _td $ places' grail.hp ]
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
        , _tr "Attribute"   $ [ link FilterAttribute s.attr ]
        , _tr "Alignment"   $ alignBox s.align
        , _tr "ID"          <<< _txt $ "#" <> show s.id
        , _tr "Star Weight" <<< _txt $ show s.gen.starWeight
        , _tr "Star Rate"   <<< _txt $ show s.gen.starRate <> "%"
        , _tr "NP/Hit"      <<< _txt $ show s.gen.npAtk <> "%"
        , _tr "NP/Defend"   <<< _txt $ show s.gen.npDef <> "%"
        , _tr "Death Rate"  <<< _txt $ show s.death
        --, _tr "Stars/Quick" <<< _txt $ places 2 (starsPer s' Quick)
        --, _tr "NP/Arts"     <<< _txt $ places 2 (npPer s' Arts) <> "%"
        ]
      , H.form [_i "myservant"] myServantBox
      , _h 2 "Noble Phantasm"
      , H.table [_i "phantasm"]
        [ _tr "Name" <<< _txt $ s.phantasm.name
        , _tr "Rank" <<< _txt $ npRank s.phantasm.rank
        , _tr "Card" $ [ link FilterCard s.phantasm.card ]
        , _tr "Class" <<< _txt $ s.phantasm.kind
        , H.tr_
          [ _th "Effects"
          , H.td_ <<< showTables ? consAfter
              ( _table (append "NP" <<< show <$> 1..5) $
                npRow <$> nub (ranges b.phantasm.effect)
              ) $ effectEl <$> s.phantasm.effect
          ]
        , H.tr_
          [ _th "Overcharge"
          , H.td overMeta <<< showTables ? consAfter
              ( _table (flip append "%" <<< show <<< (_ * 100) <$> 1..5) $
                overRow <$> nub (ranges b.phantasm.over)
              ) $ effectEl <$> s.phantasm.over
          ]
        ]
      , _h 2 "Active Skills"]
      <> (zipWith (skillEl showTables) s.skills b.skills) <>
      [ _h 2 "Passive Skills"] <> (passiveEl <$> s.passives) <>
      [ _h 2 "Max-Bond Craft Essence"
      , bondEl $ getBond s'
      , _h 2 "Traits"
      , H.section_ <<< intersperse (H.text ", ") $ link FilterTrait <$> s.traits
      , _h 2 "Calculator"
      , H.table [_i "calc"]
        [ H.tr_
          [ H.td_
            [ _h 3 "NP Generation"
            , H.table_
              [ _tr "Per Arts card" <<< _txt $ calc NPArts
              , _tr "Per full deck" <<< _txt $ calc NPDeck
              ]
            ]
          , H.td_
            [ _h 3 "NP Damage"
            , H.table_
              [ _tr "100% Overcharge" <<< _txt $ calc NPDmg
              , _tr "500% Overcharge" <<< _txt $ calc NPDmgOver
              ]
            ]
          ]
        , H.tr_
          [ H.td_
            [ _h 3 "Star Generation"
            , H.table_
              [ _tr "Per Quick card" <<< _txt $ calc StarQuick
              , _tr "Per full deck"  <<< _txt $ calc StarDeck
              ]
            ]
          , H.td_
            [ _h 3 "NP Special Damage"
            , H.table_
              [ _tr "100% Overcharge" <<< _txt $ calc NPSpec
              , _tr "500% Overcharge" <<< _txt $ calc NPSpecOver
              ]
            ]
          ]
        ]
      ]
    ]
  where
    MyServant ms@{servant:s'@(Servant s), base:(Servant b), sorted} = ms'
    npRank Unknown = "--"
    npRank x       = show x
    {base} = s.stats
    {max, grail} = b.stats
    showTables = prefer prefs ShowTables
    overMeta
      | s.phantasm.first = [_c "activates"]
      | otherwise        = []
    alter f = OnTeam true <<< MyServant $ f ms
    _mInt = _int DoNothing
    alignBox []                 = [H.text "None"]
    alignBox [Neutral, Neutral] = [H.text "True ", link FilterAlignment Neutral]
    alignBox [a, b, c, d]       = [ link FilterAlignment a
                                  , H.text " "
                                  , link FilterAlignment b
                                  , H.text " / "
                                  , link FilterAlignment c
                                  , H.text " "
                                  , link FilterAlignment d
                                  ]
    alignBox xs = xs >>= \x -> [link FilterAlignment x, H.text " "]
    calc sort = formatSort sort <<< fromMaybe (-1.0) $ Map.lookup sort sorted
    skillBox i ({icon} ^ lvl) =
        [ H.td_ [ toImage icon ]
        , H.td_ $ _mInt 1 10 lvl \val ->
          alter _{ skills = maybeDo (Array.updateAt i val) ms.skills }
        ]
    myServantBox = case ms.level of
        0 -> [ _a "+Add to My Servants" <<< OnTeam true $ newServant s' ]
        _ -> [ H.table_
               [ H.tr_
                 [ H.td_ [_strong "Level:"]
                 , H.td_ $ _mInt 1 100 ms.level \val ->
                       alter _{ level = val }
                 , H.td_ [_strong "NP:"]
                 , H.td_ $ _mInt 1 5 ms.npLvl \val ->
                       alter _{ npLvl = val }
                 , H.td_ [_strong "+ATK:"]
                 , H.td_ $ _mInt 0 990 ms.fou.atk \val ->
                       alter _{ fou { atk = val } }
                 , H.td_ [_strong "+HP:"]
                 , H.td_ $ _mInt 0 990 ms.fou.hp \val ->
                       alter _{ fou { hp = val } }
                 ]
               , H.tr_ <<< append
                 [ H.td_ [ _a "Delete" <<< OnTeam false $ unowned s' ]
                 , H.td_ [_strong "Skills:"]
                 ] <<< join <<<
                   zipWith skillBox (0..10) $ zip s.skills ms.skills
               ]
             ]

skillEl :: ∀ a. Boolean -> Skill -> Skill -> HTML a (Query Unit)
skillEl showTables active@{name, icon, cd, rank, effect} base =
    H.section_ <<< showTables ? consAfter effectTable $
    [ toImage icon
    , _h 3 $ name <> show rank
    , _strong "CD: "
    , H.text <<< (active == base) ? (_ <> "~" <> show (cd - 2)) $ show cd
    ] <> (effectEl <$> effect)
  where
    effectTable = _table (show <$> 1..10) $
                  lvlRow <$> nub (ranges base.effect)

passiveEl :: ∀ a. Skill -> HTML a (Query Unit)
passiveEl p@{name, rank, icon, effect} = H.section_ $
    [ toImage icon
    , H.h3_
      [ H.span [_c "link", _click $ FilterBy [passiveFilter p] ] $ _txt name
      , H.text $ " " <> show rank
      ]
    ] <> (_p <<< show <$> effect)

bondEl :: ∀ a. Maybe CraftEssence -> HTML a (Query Unit)
bondEl Nothing = H.section_ $ _txt "N/A"
bondEl ce@(Just (CraftEssence {name, icon, effect})) = H.section_ $
    [ toImage icon
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

effectEl :: ∀ a. SkillEffect -> HTML a (Query Unit)
effectEl ef
  | demerit ef = H.p [_c "demerit"] <<< _txt $ show ef
  | otherwise  = H.p (maybe [] meta $ skillFilter ef) <<< _txt $ show ef
  where
    meta filt = [_c "link", _click $ FilterBy [filt]]

npRow :: ∀ a b. RangeInfo -> HTML a b
npRow (RangeInfo isPercent x y) =
    H.tr_ $ toCell isPercent <<< (_ + x) <<< (_ * over)
    <$> [0.0, 0.5, 0.75, 0.825, 1.0]
  where
    over = y - x

overRow :: ∀ a b. RangeInfo -> HTML a b
overRow (RangeInfo isPercent x y) =
    H.tr_ $ toCell isPercent <<< (_ + x) <<< (_ * over) <<<
    Int.toNumber <$> (0..4)
  where
    over = (y - x) / 4.0

link :: ∀ a b. MatchServant a => FilterTab -> a -> HTML b (Query Unit)
link tab x =
    H.a
    [_c "link", _click <<< FilterBy $ singleFilter tab x]
    [H.text $ show x]
