module Site.Component (Query, siteComponent) where

import Prelude
import Operators

import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P
import Data.Map                as M
import Data.String             as S

import Data.Tuple (Tuple(..), snd, uncurry)
import Halogen    ( Component, ComponentDSL, ComponentHTML
                  , component, liftEffect, modify_ )
import Data.Array
import Data.Int
import Data.Maybe
import Data.Number.Format
import Effect.Class
import Halogen.HTML
import Halogen.HTML.Properties
import Routing.Hash
import Web.UIEvent.MouseEvent

import Database
import Site.Filters
import Site.Sorting
import Site.Preferences
import Printing

type Input = Unit
type Message = Void
data Query a = Focus (Maybe Servant) a
             | ClearAll  a
             | OneFilter Filter a
             | Toggle    Filter a
             | MatchAny  Boolean a
             | SetSort   SortBy a
             | SetPref   Preference Boolean a
             | Ascend    Int a

type State = { filters  ∷ Array Filter
             , exclude  ∷ Array Filter
             , matchAny ∷ Boolean
             , focus    ∷ Maybe Servant
             , sortBy   ∷ SortBy
             , prefs    ∷ Preferences
             , ascend   ∷ Int
             }

urlName ∷ Servant -> String
urlName = S.replaceAll (S.Pattern " ") (S.Replacement "") ∘ _.name

siteComponent ∷ ∀ m. MonadEffect m => String -> Preferences
            -> Component HTML Query Unit Message m
siteComponent initialHash initialPrefs = component
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
                       , focus:    find ((_ == initialHash) ∘ urlName) servants
                       , sortBy:   Rarity
                       , prefs:    initialPrefs
                       , ascend:   1
                       }
 
  render ∷ State -> ComponentHTML Query
  render {ascend, exclude, filters, focus, matchAny, prefs, sortBy} 
      = modal (pref ShowTables) artorify ascend focus
        [ H.aside_ $
          [ _h 1 "Settings"
          , H.form_ $ M.toUnfoldable prefs <#> \(Tuple k v)
             -> H.p [_click ∘ SetPref k $ not v]
                $ _checkbox (show k) v
          , _h 1 "Sort by"
          , H.form_ $ enumArray <#> \sort 
             -> H.p [_click $ SetSort sort] 
                $ _radio (show sort) (sortBy == sort)
          
          , _h 1 "Include"
          ] <> (filter exclusive enumArray >>= filterSection)
        , H.section [_i "servants"] 
          ∘ (if sortBy == Rarity then identity else reverse)
          $ portrait false artorify baseAscend 
            <$> (filter (match ∘ snd) $ getSort sortBy)
        , H.aside_ $
          [ _h 1 "Filter"
          , H.form_ 
            [ H.table_ ∘ singleton $ H.tr_
              [ _th "Match"
              , H.td [_click $ MatchAny false] $ _radio "All" (not matchAny)
              , H.td [_click $ MatchAny true]  $ _radio "Any"      matchAny
              ] 
            , H.button clearAll $ _txt "Clear All"
            ]
          ] <> (filter (not exclusive) enumArray >>= filterSection)
        ]
    where 
      pref     = getPreference prefs
      artorify = pref Artorify
      noSelf   = pref ExcludeSelf
      baseAscend 
        | pref MaxAscension = 4
        | otherwise         = 1
      clearAll
        | null filters = [ P.enabled false ]
        | otherwise    = [ P.enabled true, _click ClearAll ]
      matchFilter s (Filter _ _ f) = f noSelf s
      match s  = (null exclude || all (matchFilter s) exclude)
              && (null filters || (if matchAny then any else all) 
                                  (matchFilter s) filters)
      filterSection tab = [ _h 3 $ show tab
                          , H.form_ $ getFilters tab <#> \filt
                             -> H.p [_click $ Toggle filt ] 
                                ∘ _checkbox (show filt) 
                                $ if exclusive tab then filt `notElem` exclude
                                  else filt `elem` filters
                           ]
      
  eval ∷ Query ~> ComponentDSL State Query Message m
  eval = case _ of
      ClearAll           a -> a <$ modify_ _{ filters = [] }
      OneFilter filt     a -> a <$ modify_ _{ filters = [filt] }
      SetSort   sortBy   a -> a <$ modify_ _{ sortBy = sortBy }
      Ascend    ascend   a -> a <$ modify_ _{ ascend = ascend }
      MatchAny  matchAny a -> a <$ modify_ _{ matchAny = matchAny }
      Focus     focus    a -> a <$ do
          liftEffect $ hash focus
          modify_ _{ focus = focus, ascend = 1 } 
      SetPref   k v      a -> a <$ do
          liftEffect $ setPreference k v
          modify_ (modPrefs $ M.insert k v)
      Toggle filt@(Filter tab _ _) a 
        | exclusive tab -> a <$ modify_ (modExclude $ toggleIn filt)
        | otherwise     -> a <$ modify_ (modFilters $ toggleIn filt)
    where
      modFilters f state@{filters} = state{ filters = f filters }
      modPrefs   f state@{prefs}   = state{ prefs   = f prefs }
      modExclude f state@{exclude} = state{ exclude = f exclude }
      toggleIn x xs
        | x `elem` xs = delete x xs
        | otherwise   = cons x xs
      hash Nothing = setHash ""
      hash (Just s) = setHash $ urlName s

noBreakName ∷ String -> String
noBreakName = unBreak ∘ S.split (S.Pattern "(")
  where
    unBreak [a, b] = a <> "(" 
                       <> S.replaceAll (S.Pattern " ") (S.Replacement " ") b
    unBreak xs = S.joinWith "(" xs

deck ∷ ∀ a b. Deck -> Array (HTML a b)
deck (Deck a b c d e) = card ∘ show <$> [a, b, c, d, e]
  where 
    card x = H.span [_c x] ∘ _txt $ S.take 1 x 

portrait ∷ ∀ a. Boolean -> Boolean -> Int -> Tuple String Servant 
           -> HTML a (Query Unit)
portrait big artorify ascension (Tuple lab s)
    = H.div meta
      [ _img $ "img/Servant/" <> s.name <> ascend <> ".png"
      , H.div_ [ _img $ "img/Class/" <> show s.class <> ".png"]
      , H.aside_ $ deck s.deck
      , H.header_ 
        ∘ (lab /= "") ? append [_span lab, H.br_]
        $ [ _span ∘ noBreakName ∘ artorify ? doArtorify $ s.name ]
      , H.footer_ 
        ∘ ((big && ascension > 1) ? cons prevAscend)
        ∘ ((big && ascension < 4) ? (_ `snoc` nextAscend))
        ∘ singleton ∘ _span ∘ S.joinWith "  " $ replicate s.rarity "★" 
      ]
  where meta       = not big ? (cons ∘ _click ∘ Focus $ Just s) 
                   $ [_c $ "servant stars" <> show s.rarity]
        doArtorify = S.replaceAll (S.Pattern "Altria") (S.Replacement "Artoria")
        prevAscend = H.a [_click ∘ Ascend $ ascension - 1] $ _txt "<"
        nextAscend = H.a [_click ∘ Ascend $ ascension + 1] $ _txt ">"
        ascend
          | ascension <= 1 = ""
          | otherwise      = " " <> show ascension

print' ∷ Int -> String
print' = print 0 ∘ toNumber

modal ∷ ∀ a. Boolean -> Boolean -> Int -> Maybe Servant 
        -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
modal _ _ _ Nothing = H.div [_i "layout"] ∘ append 
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
modal showTables artorify ascend 
(Just s@{gen, hits, stats:{base, max, grail}, phantasm})
  = H.div [_i "layout", _c "fade"] ∘ append 
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ portrait true artorify ascend $ Tuple "" s
      , _table ["", "ATK", "HP"]
        [ H.tr_ [ _th "Base",  _td $ print' base.atk,  _td $ print' base.hp ]
        , H.tr_ [ _th "Max",   _td $ print' max.atk,   _td $ print' max.hp ]
        , H.tr_ [ _th "Grail", _td $ print' grail.atk, _td $ print' grail.hp ]
        ]
      , _table ["", "Q", "A", "B", "EX", "NP"]
      
        [ H.tr_
          [ _th "Hits"
          , _td $ show hits.quick
          , _td $ show hits.arts
          , _td $ show hits.buster
          , _td $ show hits.ex
          , _td $ show phantasm.hits
          ] 
        ] 
      , H.table_ 
        [ _tr "Class"       $ show s.class 
        , _tr "Deck"        $ show s.deck
        , _tr "NP Type"     ∘ show ∘ fromMaybe Support 
                            $ find (\t -> has t false s) 
                              [SingleTarget, MultiTarget]
        , _tr "Alignment"   $ showAlignment s.align
        , _tr "Attribute"   $ show s.attr
        , _tr "Star Weight" $ show gen.starWeight
        , _tr "Star Rate"   $ show gen.starRate <> "%"
        , _tr "NP/Hit"      $ show gen.npAtk <> "%"
        , _tr "NP/Defend"   $ show gen.npDef <> "%"
        , _tr "Death Rate"  $ show s.death <> "%"
        ]
      , H.h2 [_c "npheading"] $ _txt "Noble Phantasm"
      , H.table [_c "phantasm"]
        [ _tr "Name" $ phantasm.name
        , _tr "Rank" $ show phantasm.rank
        , _tr "Card" $ show phantasm.card
        , _tr "Class" $ phantasm.kind
        , H.tr_ 
          [ _th "Effects"
          , H.td_ ∘ showTables ? (flip snoc) 
              ( _table (("NP" <> _) ∘ show <$> 1..5) 
                $ npRow <$> nub (ranges phantasm.effect)
              ) $ _p ∘ show <$> phantasm.effect
          ]
        , H.tr_ 
          [ _th "Overcharge"
          , H.td_ ∘ showTables ? (flip snoc)
              (_table ((_ <> "%") ∘ show ∘ (_ * 100) <$> 1..5)
                $ overRow <$> nub (ranges phantasm.over)
              )
            $ _p ∘ uncurry showOver <$> overs
          ]
        ]
      , _h 2 "Active Skills"] <> (activeEl showTables <$> s.actives) <>
      [ _h 2 "Passive Skills"] <> (passiveEl <$> s.passives) <>
      [ _h 2 "Traits"
      , H.section_ $ traitEl <$> s.traits
      ]
    ]
  where 
    overs = zip phantasm.over ∘ cons phantasm.first $ replicate 10 false
    showOver eff true = show eff <> " [Activates first.]"
    showOver eff false = show eff

activeEl ∷ ∀ a b. Boolean -> Active -> HTML a b
activeEl showTables {name, icon, cd, effect} = H.section_ 
    ∘ showTables ? (flip snoc) 
      (_table (show <$> 1..10) $ lvlRow <$> nub (ranges effect)) $
    [ _img $ "img/Skill/" <> show icon <> ".png"
    , _h 3 name
    , H.span_ 
      [  H.strong_ [ H.text "CD: "]
      , H.text $ show cd <> "~" <> show (cd - 2)
      ]
    ] <> (_p ∘ show <$> effect) 

passiveEl ∷ ∀ a b. Passive -> HTML a b
passiveEl {name, rank, icon, effect} = H.section_ $
    [ _img $ "img/Skill/" <> show icon <> ".png" 
    , _h 3 $ name <> " " <> show rank
    ] <> (_p ∘ show <$> effect)

traitEl ∷ ∀ a b. Trait -> HTML a b
traitEl trait = H.span [_c "trait"] ∘ _txt $ show trait

toCell ∷ ∀ a b. Boolean -> Number -> HTML a b
toCell isPercent = _td ∘ (isPercent ? (_ <> "%")) ∘ toString ∘ roundTo 2

lvlRow ∷ ∀ a b. RangeInfo -> HTML a b
lvlRow (RangeInfo isPercent a b) = H.tr_
           $ toCell isPercent ∘ (_ + a) ∘ (_ * step) ∘ toNumber 
          <$> (0..8) `snoc` 10
  where
    step = (b - a) / 10.0

npRow ∷ ∀ a b. RangeInfo -> HTML a b
npRow (RangeInfo isPercent a b) = H.tr_
          $ toCell isPercent ∘ (_ + a) ∘ (_ * over)
         <$> [0.0, 0.5, 0.75, 0.825, 1.0]
  where
    over = b - a

overRow ∷ ∀ a b. RangeInfo -> HTML a b
overRow (RangeInfo isPercent a b) = H.tr_
          $ toCell isPercent ∘ (_ + a) ∘ (_ * over) ∘ toNumber 
         <$> 0..4
  where
    over = (b - a) / 4.0

----------------
-- ABBREVIATIONS
----------------

_i   ∷ ∀ a b. String -> IProp (id ∷ String | b) a 
_i   = P.id_
_c   ∷ ∀ a b. String -> IProp (class ∷ String | b) a
_c   = P.class_ ∘ ClassName
_src ∷ ∀ a b. String -> IProp (src ∷ String | b) a
_src = P.src
_style ∷ ∀ a b. String -> IProp (style ∷ String | b) a
_style = P.attr (H.AttrName "style")

_txt ∷ ∀ a b. String -> Array (HTML a b)
_txt = (flip cons) [] ∘ H.text

_a ∷ ∀ a b. String -> String -> String -> String -> HTML a b
_a id' class' href' = H.a [_i id', _c class', P.href href'] ∘ _txt

_img ∷ ∀ a b. String -> HTML a b
_img src = H.img [_src src]

_click ∷ ∀ a b. (Unit -> b Unit) -> IProp ( onClick ∷ MouseEvent | a ) (b Unit)
_click = E.onClick ∘ E.input_

_table ∷ ∀ a b. Array String -> Array (HTML a b) -> HTML a b
_table headings tbody = H.table_ 
  [ H.colgroup_ $ const (H.col []) <$> headings
  , H.thead_ [ H.tr_ $ H.th_ ∘ _txt <$> headings ]
  , H.tbody_ tbody 
  ]

_tr ∷ ∀ a b. String -> String -> HTML a b
_tr a b = H.tr_ [ _th a, _td b ]

_radio ∷ ∀ a b. String -> Boolean -> Array (HTML a b)
_radio label checked
    = [ H.input [ P.type_ P.InputRadio, P.checked checked ]
      , H.label_ $ _txt label
      ]

_checkbox ∷ ∀ a b. String -> Boolean -> Array (HTML a b)
_checkbox label checked
    = [ H.input [P.type_ P.InputCheckbox, P.checked checked ]
      , H.label_ $ _txt label
      ]

_span ∷ ∀ a b. String -> HTML a b
_span = H.span_ ∘ _txt
_p ∷ ∀ a b. String -> HTML a b
_p = H.p_ ∘ _txt
_b ∷ ∀ a b. String -> HTML a b
_b = H.b_ ∘ _txt
_th ∷ ∀ a b. String -> HTML a b
_th = H.th_ ∘ _txt
_td ∷ ∀ a b. String -> HTML a b
_td = H.td_ ∘ _txt
_h ∷ ∀ a b. Int -> String -> HTML a b
_h 1 = H.h1_ ∘ _txt
_h 2 = H.h2_ ∘ _txt
_h 3 = H.h3_ ∘ _txt
_h 4 = H.h4_ ∘ _txt
_h 5 = H.h5_ ∘ _txt
_h _ = H.h6_ ∘ _txt
