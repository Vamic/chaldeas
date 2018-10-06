-- | The user interface for Servants and My Servants.
-- This module is only for functions that render Servants to HTML.
-- Everything else goes in `Database.Servant` and `MyServant`.
module Site.Servants.Component (Query, Message(..), State, comp) where

import StandardLibrary
import Data.Array              as Array
import Halogen.HTML            as H
import Routing.Hash            as Hash
import Data.Int                as Int
import Data.Map                as Map
import Data.String             as String

import Halogen (Component, ComponentDSL, ComponentHTML, component, get, modify_)
import Halogen.HTML (HTML)
import Data.Date (Date)

import Sorting
import Database
import MyServant
import MyServant.Leveling
import Site.Algebra
import Site.Common
import Site.Eval
import Site.ToImage
import Site.Preferences
import Site.Filtering
import Site.Rendering
import Site.Servants.Filters
import Site.Servants.Sorting
import Printing

type Message = SiteMessage Servant CraftEssence

type State = SiteState Servant MyServant 
             ( mineOnly :: Boolean
             , ascent   :: Int
             , myServs  :: Array MyServant
             , team     :: Map Servant MyServant
             )

type Query = SiteQuery Servant MyServant CraftEssence

reSort :: State -> State
reSort st = 
    st { sorted = getSort (prefer st.prefs AddSkills) st.sortBy st.myServs }

-- | Halogen component.
comp :: ∀ m. MonadEffect m 
     => Date -> Component HTML Query (State -> State) Message m
comp today = 
    component
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  allFilters :: FilterList Servant
  allFilters = collectFilters getFilters today

  initialState :: (State -> State) -> State
  initialState f = updateListing getBase <<< reSort $ f
      { section:  Nothing
      , filters:  mempty
      , exclude:  mempty
      , matchAny: true
      , mineOnly: false
      , focus:    Nothing
      , sortBy:   Rarity
      , prefs:    mempty
      , ascent:   1
      , myServs:  unowned <$> servants
      , sorted:   mempty
      , listing:  mempty
      , team:     mempty
      }

  render :: State -> ComponentHTML Query
  render st = popup st.prefs st.ascent st.focus $
              site st enumArray allFilters' nav content
    where
      nav = [ _a "Craft Essences" $ Switch Nothing
            , if st.mineOnly then _a      "Servants" $ MineOnly false
                             else _strong "Servants"
            , if st.mineOnly then _strong "My Servants"
                             else _a      "My Servants" $ MineOnly true
            ]
      portraits = case st.sortBy of
          Rarity -> map doPortrait
          _      -> reverse <<< map doPortrait
      content
        | st.mineOnly = portraits mine 
                        <> getMats "Ascension" ascendWishlist 
                        <> getMats "Skill" skillWishlist
          where
            mine = filter isMine st.listing
            getMats label f = case f $ _.obj <$> mine of
                []   -> []
                mats -> [_h 2 $ "Total " <> label <> " Materials Needed"
                        , H.footer [_c "materials"] $ materialEl <$> mats
                        ]
        | otherwise   = portraits st.listing
      doPortrait {label: "", obj: ms}
        | st.mineOnly = portrait false st.prefs baseAscend
                        { label: showStats ms, obj: ms }
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

  eval :: Query ~> ComponentDSL State Query Message m
  eval = case _ of
      Ascend (MyServant {level: 0}) ascent a -> a <$
          modify_ _{ ascent = ascent }
      Ascend (MyServant ms) ascent a -> eval $
          OnTeam true (MyServant ms{ascent = ascent}) a
      MineOnly mineOnly a -> a <$ do
          liftEffect $ hash mineOnly Nothing
          modif _{ mineOnly = mineOnly }
      OnTeam keep myServant a -> a <$  do
          {team}        <- get
          let myServant' = (keep ? recalc) myServant
              team'      = if keep
                           then Map.insert (getBase myServant') myServant' team
                           else Map.delete (getBase myServant') team
              myServs    = owned team' <$> servants
          liftEffect $ setTeam team'
          modif $ reSort <<< \st -> st { team    = team'
                                       , myServs = myServs
                                       , focus   = st.focus *> Just myServant'
                                       }
      Focus    focus    a -> a <$ do
          {mineOnly} <- get
          liftEffect $ hash mineOnly focus
          modify_ _{ focus = focus, ascent = 1 }
      req -> do
          {myServs, prefs} <- get
          siteEval "Servants" getBase (getFilters today) reSort req
    where
      modif = modify_ <<< compose (updateListing getBase)
      hash true  Nothing = Hash.setHash "MyServants"
      hash false Nothing = Hash.setHash "Servants"
      hash _ (Just s)    = Hash.setHash <<< urlName <<< show $ getBase s

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
      , H.header_ $ (label /= "" ? append [_span label, H.br_])
        [ _span <<< noBreakName big $ artorify s.name ]
      , H.footer_ <<<
        (big && ascension > 1 ? cons prevAscend) $
        (big && ascension < 4 ? consAfter nextAscend)
        [_span <<< String.joinWith "  " $ replicate s.rarity "★"]
      ]
  where
    MyServant ms@{servant:Servant s} = ms'
    artorify   = prefer prefs Artorify ?
                 String.replaceAll (Pattern "Altria") (Replacement "Artoria")
    meta       = (not big ? cons <<< _click <<< Focus $ Just ms')
                 [_c $ "portrait stars" <> show s.rarity]
    ascension = case ms.level of
        0 -> baseAscension
        _ -> ms.ascent
    prevAscend = _a "<" <<< Ascend ms' $ ascension - 1
    nextAscend = _a ">" <<< Ascend ms' $ ascension + 1
    ascent
      | ascension <= 1 = ""
      | otherwise      = " " <> show ascension

popup :: ∀ a. Preferences -> Int -> Maybe MyServant
      -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
popup prefs _ Nothing = H.div [_c $ mode prefs] <<< append
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
popup prefs ascent focus@(Just ms') = H.div
  [_c $ "fade " <> mode prefs] <<< append
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ H.div_
        [ portrait true prefs ascent { label: "", obj: ms' }
        , H.div_
          [ _table ["", "ATK", "HP"]
            [ H.tr_ 
              [ _th "Base",  _td $ commas base.atk,  _td $ commas base.hp ]
            , H.tr_ 
              [ _th "Max",   _td $ commas max.atk,   _td $ commas max.hp ]
            , H.tr_ 
              [ _th "Grail", _td $ commas grail.atk, _td $ commas grail.hp ]
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
            , _tr "Attribute"   [ link FilterAttribute s.attr ]
            , _tr "Alignment"   $ alignBox s.align
            , _tr "ID"          [ H.text $ "#" <> show s.id ]
            , _tr "Star Weight" [ H.text $ show s.gen.starWeight ]
            , _tr "Star Rate"   [ H.text $ show s.gen.starRate <> "%" ]
            , _tr "NP/Hit"      [ H.text $ show s.gen.npAtk <> "%" ]
            , _tr "NP/Defend"   [ H.text $ show s.gen.npDef <> "%" ]
            , _tr "Death Rate"  [ H.text $ show s.death ]
            ]
        ]
      ]
      , H.form [_i "myservant"] myServantBox
      , _h 2 "Noble Phantasm"
      , H.table [_i "phantasm"]
        [ _tr "Name" [ H.text s.phantasm.name ]
        , _tr "Rank" [ H.text $ npRank s.phantasm.rank ]
        , _tr "Card" [ link FilterCard s.phantasm.card ]
        , _tr "Class" [ H.text s.phantasm.kind ]
        , H.tr_
          [ _th "Effects"
          , H.td_ <<< 
            showTable (append "NP" <<< show) b.phantasm.effect $ 
            effectEl <$> s.phantasm.effect
          ]
        , H.tr_
          [ _th "Overcharge"
          , H.td_ <<< 
            showTable (flip append "%" <<< show <<< (_ * 100)) b.phantasm.over $ 
            effectEl <$> s.phantasm.over
          ]
        ]
      , _h 2 "Active Skills"]
      <> (zipWith (skillEl showTables) s.skills b.skills) <>
      [ _h 2 "Passive Skills"] <> (passiveEl <$> s.passives) <>
      [ _h 2 "Max-Bond Craft Essence"
      , bondEl $ getBond s'
      , _h 2 "Traits"
      , H.section_ <<< intersperse (H.text ", ") $ link FilterTrait <$> s.traits
      , _h 2 "Ascension"
      , H.table [_c "materials"] $
        flip Array.mapWithIndex (ascendUpEl s.ascendUp) \i el ->
            H.tr_ [_th <<< show $ i + 1, H.td_ $ withCost (ascendCost s' i) el]
      , _h 2 "Skill Reinforcement"
      , H.table [_c "materials"] $
        flip Array.mapWithIndex (skillUpEl s.skillUp) \i el ->
            H.tr_ [_th <<< show $ i + 2, H.td_ $ withCost (skillCost s' i) el]
      , _h 2 "Calculator"
      , H.table [_i "calc"]
        [ H.tr_
          [ H.td_
            [ _h 3 "NP Generation"
            , H.table_
              [ _tr "Per Arts card" [ H.text $ calc NPArts ]
              , _tr "Per full deck" [ H.text $ calc NPDeck ]
              ]
            ]
          , H.td_
            [ _h 3 "NP Damage"
            , H.table_
              [ _tr "100% Overcharge" [ H.text $ calc NPDmg ]
              , _tr "500% Overcharge" [ H.text $ calc NPDmgOver ]
              ]
            ]
          ]
        , H.tr_
          [ H.td_
            [ _h 3 "Star Generation"
            , H.table_
              [ _tr "Per Quick card" [ H.text $ calc StarQuick ]
              , _tr "Per full deck"  [ H.text $ calc StarDeck ]
              ]
            ]
          , H.td_
            [ _h 3 "NP Special Damage"
            , H.table_
              [ _tr "100% Overcharge" [ H.text $ calc NPSpec ]
              , _tr "500% Overcharge" [ H.text $ calc NPSpecOver ]
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
    showTable showCol effects
      | showTables = consAfter $ _table
                     (showCol <$> 1..5) $
                     npRow <$> nub (ranges effects)
      | otherwise  = identity
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
    calcWith
      | prefer prefs AddSkills = fst
      | otherwise              = snd
    calc sort = formatSort sort <<< fromMaybe infinity $ calcWith <$> 
                Map.lookup sort sorted
    skillBox i ({icon} : lvl) =
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

withCost :: ∀ a. Int -> Array (HTML a (Query Unit))
         -> Array (HTML a (Query Unit))
withCost 0    = identity
withCost cost = cons $ materialEl (QP : cost)

materialEl :: ∀ a. (Material : Int) -> HTML a (Query Unit)
materialEl (mat : amt) = H.div_ [ imageLink mat, _span $ "×" <> commas amt ]
  where
    imageLink
      | ignoreMat mat = toImage
      | otherwise     = toImageLink <<< FilterBy $
                        singleFilter FilterMaterial mat

ascendUpEl :: ∀ a. Ascension -> Array (Array (HTML a (Query Unit)))
ascendUpEl (Clear a b c d) = Array.singleton <<< H.text <<< append "Clear "
                             <$> [a, b, c, d]
ascendUpEl (Welfare x) = replicate 4 <<< Array.singleton <<< toImage $
                         ImagePath "Material" x
ascendUpEl (Ascension a b c d) = map materialEl <$> [a, b, c, d]

skillUpEl :: ∀ a. Reinforcement -> Array (Array (HTML a (Query Unit)))
skillUpEl (Reinforcement a b c d e f g h) =
    map materialEl <$> [a, b, c, d, e, f, g, h, [CrystallizedLore: 1]]

skillEl :: ∀ a. Boolean -> Skill -> Skill -> HTML a (Query Unit)
skillEl showTables active@{name, icon, cd, rank, effect} base =
    H.section_ <<< (showTables ? consAfter effectTable) $
    [ toImage icon
    , _h 3 $ name <> show rank
    , _strong "CD: "
    , H.text <<< (active == base ? flip append $ "~" <> show (cd - 2)) $ 
      show cd
    ] <> (effectEl <$> effect)
  where
    effectTable = _table (show <$> 1..10) $
                  lvlRow <$> nub (ranges base.effect)

passiveEl :: ∀ a. Skill -> HTML a (Query Unit)
passiveEl p@{name, rank, icon, effect} = H.section_ $
    [ toImage icon
    , H.h3_
      [ H.span [_c "link", _click $ FilterBy [passiveFilter p] ] [ H.text name ]
      , H.text $ " " <> show rank
      ]
    ] <> (_p <<< show <$> effect)

bondEl :: ∀ a. Maybe CraftEssence -> HTML a (Query Unit)
bondEl Nothing = H.section_ [ H.text "N/A" ]
bondEl ce@(Just (CraftEssence {name, icon, effect})) = H.section_ $
    [ toImage icon
    , H.h3 [_c "link", _click $ Switch ce] [ H.text name ]
    , H.p_
      [                  _span "★★★★ "
      , _strong "ATK: ", _span "100 "
      , _strong "DEF: ", _span "100"
      ]
    ] <> (_p <<< show <$> effect)

effectEl :: ∀ a. SkillEffect -> HTML a (Query Unit)
effectEl ef
  | demerit ef = H.p [_c "demerit"] [ H.text $ show ef ]
  | otherwise  = H.p (maybe [] meta $ skillFilter ef) [ H.text $ show ef ]
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

link :: ∀ a b. Has Servant a => FilterTab -> a -> HTML b (Query Unit)
link tab x =
    H.a
    [_c "link", _click <<< FilterBy $ singleFilter tab x]
    [H.text $ show x]
