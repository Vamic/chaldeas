module Component where

import Prelude
import Operators

import Halogen                 as Halogen
import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P

import Data.Array hiding (take)
import Data.Enum
import Data.Formatter.Number
import Data.Generic.Rep
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum
import Data.Generic.Rep.Show
import Data.Int
import Data.Maybe
import Data.Profunctor.Strong
import Data.String hiding (null, singleton)
import Data.Tuple
import Effect.Class
import Filters
import Halogen (Component, ComponentDSL, ComponentHTML, liftEffect, modify_)
import Halogen.HTML
import Halogen.HTML.Properties
import Routing.Hash
import Web.UIEvent.MouseEvent

import Database

print' ∷ Number -> String
print' = format $ Formatter { comma: true
                            , before: 0
                            , after: 0
                            , abbreviations: false
                            , sign: false
                            } 
print ∷ Int -> String
print = print' ∘ toNumber


type Input = Unit
type Message = Void
data Query a = Focus (Maybe Servant) a
             | AddFilter Filter a
             | UnFilter  Filter a
             | MatchAny  Boolean a
             | SetSort   SortBy  a

data SortBy = Rarity
            | ATK
            | HP
            | StarRate
            | Hits
            | NPDamage

type State = { filters  ∷ Array Filter
             , matchAny ∷ Boolean
             , focus    ∷ Maybe Servant
             , sortBy   ∷ SortBy
             }

urlName ∷ Servant -> String
urlName (Servant {name}) = replaceAll (Pattern " ") (Replacement "") name

component ∷ ∀ m. MonadEffect m => String -> Component HTML Query Unit Message m
component initialHash = Halogen.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
 
  initialState ∷ Input -> State
  initialState = const { filters:  []
                       , matchAny: true
                       , focus:    find ((_ == initialHash) ∘ urlName) servants
                       , sortBy:   Rarity
                       }
 
  render ∷ State -> ComponentHTML Query
  render {filters, focus, matchAny, sortBy} 
      = modal focus
        [ H.aside [_i "active"] ∘ append
          [_h 1 "Sort by"
          , H.form_ $ enumArray <#> \sort 
             -> H.p [_click $ SetSort sort] $ _radio (show sort) (sortBy == sort)
          
          , _h 1 "Active filters"
          , H.form_ ∘ singleton ∘ H.table_ ∘ singleton $ H.tr_ 
            [ _th "Match"
            , H.td [_click $ MatchAny false] $ _radio "All" (not matchAny)
            , H.td [_click $ MatchAny true]  $ _radio "Any"      matchAny
            ] 
          ] $ reverse filters <#> \filt@(Filter tab _ _)
             -> H.p [_click $ UnFilter filt, _c "unselected"] ∘ _txt 
                $ "ⓧ " ++ show tab ++ ": " ++ show filt 
        , H.section [_i "servants"] 
          ∘ (if sortBy == Rarity then identity else reverse)
          $ portrait <$> doSort (maybeFilter servants)
        , H.aside [_i "filters"] ∘ cons
          (_h 1 "Filters") ∘ concat 
          $ enumArray <#> \tab
           -> [ _h 3 $ show tab 
              , H.ul_ $ filterEffects tab <#> \filt
               -> H.li (meta filt) ∘ _txt $ show filt
              ]
        ]
    where 
      maybeFilter = if null filters then identity else filter match
      match serv  = (if matchAny then any else all) 
                    (\(Filter _ _ f) -> f serv) filters
      filterEffects tab = case tab of
          FilterAction    -> matchFilter tab <$> getAll ∷ Array InstantEffect
          FilterAlignment -> matchFilter tab <$> getAll ∷ Array Alignment
          FilterAttribute -> matchFilter tab <$> getAll ∷ Array Attribute
          FilterBuff      -> matchFilter tab <$> getAll ∷ Array BuffEffect
          FilterCard      -> matchFilter tab <$> getAll ∷ Array Card
          FilterClass     -> matchFilter tab <$> getAll ∷ Array Class
          FilterDebuff    -> matchFilter tab <$> getAll ∷ Array DebuffEffect
          FilterDeck      -> matchFilter tab <$> getAll ∷ Array Deck
          FilterEvent     -> getExtraFilters FilterEvent
          FilterOther     -> getExtraFilters FilterOther
          FilterPhantasm  -> matchFilter tab <$> getAll ∷ Array PhantasmType
          FilterPassive   -> uncurry (Filter tab) ∘ (identity&&&hasPassive) 
                        <$> getPassives
          FilterTrait     -> matchFilter tab <$> getAll ∷ Array Trait
      doSort = case sortBy of
          NPDamage -> sortWith $ \serv -> npDamage serv
          Rarity   -> sortWith $ \(Servant s) -> show (5 - s.rarity) ++ s.name
          ATK      -> sortWith $ \(Servant s) -> s.stats.max.atk
          HP       -> sortWith $ \(Servant s) -> s.stats.max.hp
          StarRate -> sortWith $ \(Servant s) -> s.gen.starRate
          Hits     -> sortWith $ \(Servant {hits}) 
                   -> hits.a + hits.b + hits.q + hits.ex
      meta filt 
        | filt `elem` filters = [_c "selected"]
        | otherwise           = [_c "unselected", _click $ AddFilter filt]
{-
ToTab FilterTab a
             | Focus (Maybe Servant) a
             | UnFocus (Maybe Servant) a
             | ToNP FilterNP a
             | AddFilter Filter a
             | RemoveFilter Filter a
-}
  eval ∷ Query ~> ComponentDSL State Query Message m
  eval = case _ of
      AddFilter filt     next -> (_ >> next) $ modFilters (cons filt)
      UnFilter  filt     next -> (_ >> next) $ modFilters (delete filt)
      MatchAny  matchAny next -> (_ >> next) $ modify_ _{ matchAny = matchAny }
      SetSort   sortBy   next -> (_ >> next) $ modify_ _{ sortBy = sortBy }
      Focus     focus    next -> (_ >> next) $ do
          liftEffect $ hash focus
          modify_ _{ focus = focus } 
    where
      modFilters f = modify_ \state@{filters} -> state{ filters = f filters }
      hash Nothing = setHash ""
      hash (Just s) = setHash $ urlName s

noBreakName ∷ String -> String
noBreakName = unBreak ∘ split (Pattern "(")
  where
    unBreak [a, b] = a ++ "(" ++ replaceAll (Pattern " ") (Replacement " ") b
    unBreak xs = joinWith "(" xs

deck ∷ ∀ a b. Deck -> Array (HTML a b)
deck (Deck a b c d e) = card ∘ show <$> [a, b, c, d, e]
  where 
    card x = H.span [_c x] ∘ _txt $ take 1 x --_img $ "img/Card/" ++ show x ++ ".png"

portrait ∷ ∀ a. Servant -> HTML a (Query Unit)
portrait serv@(Servant s) 
    = H.div [_c $ "servant stars" ++ show s.rarity, _click ∘ Focus $ Just serv]
      [ _img $ "img/Servant/" ++ s.name ++ ".png"
      , H.div_ [ _img $ "img/Class/" ++ show s.class ++ ".png"]
      , H.aside_ $ deck s.deck
      , H.header_ [ _span $ noBreakName s.name ]
      , H.footer_ ∘ _txt ∘ joinWith "  " $ replicate s.rarity "★" 
      ]

modal ∷ ∀ a. Maybe Servant -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
modal Nothing = H.div [_i "layout"] ∘ append 
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
modal (Just serv@(Servant s@{gen, hits, stats:{base, max, grail}, phantasm})) 
  = H.div [_i "layout", _c "fade"] ∘ append 
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ portrait serv
      , _table ["", "ATK", "HP"]
        [ H.tr_ [ _th "Base",  _td $ print base.atk,  _td $ print base.hp ]
        , H.tr_ [ _th "Max",   _td $ print max.atk,   _td $ print max.hp ]
        , H.tr_ [ _th "Grail", _td $ print grail.atk, _td $ print grail.hp ]
        ]
      , _table ["", "Q", "A", "B", "EX", "NP"]
      
        [ H.tr_
          [ _th "Hits"
          , _td $ show hits.q
          , _td $ show hits.a
          , _td $ show hits.b
          , _td $ show hits.ex
          , _td $ show phantasm.hits
          ] 
        ] 
      , H.table_ 
        [ _tr "Class"       $ show s.class 
        --, H.tr_ [_th "Deck", H.td_ $ show s.deck]
        , _tr "Deck"        $ show s.deck
        , _tr "NP Type"     ∘ show ∘ fromMaybe Support 
                            $ find (_ `has` serv) [SingleTarget, MultiTarget]
        --, _tr "NP Damage"   ∘ print' $ npDamage serv
        , _tr "Alignment"   $ showAlignment s.align
        , _tr "Attribute"   $ show s.attr
        , _tr "Star Weight" $ show gen.starWeight
        , _tr "Star Rate"   $ show gen.starRate ++ "%"
        , _tr "NP/Hit"      $ show gen.npPerHit ++ "%"
        , _tr "NP/Defend"   $ show gen.npPerDefend ++ "%"
        , _tr "Death Rate"  $ show s.death ++ "%"
        {-
        , rate "Damage"     ratings.damage
        , rate "NP Gain"    ratings.np
        , rate "Critical"   ratings.critical
        , rate "Utility"    ratings.utility
        , rate "Support"    ratings.support
        , rate "Durability" ratings.durability
        -}
        ]
      , H.h2 [_c "npheading"] $ _txt "Noble Phantasm"
      , H.table [_c "phantasm"]
        [ _tr "Name" $ phantasm.name
        , _tr "Rank" $ show phantasm.rank
        , _tr "Card" $ show phantasm.card
        , _tr "Class" $ phantasm.kind
        --, _tr "Damage" ∘ print' $ npDamage serv
        , H.tr_ [_th "Effects", H.td_ $ _p ∘ show <$> phantasm.effect]
        , H.tr_ [_th "Overcharge", H.td_ $ _p ∘ show <$> phantasm.over]
        ]
      , _h 2 "Active Skills"] ++ (activeEl <$> s.actives) ++
      [ _h 2 "Passive Skills"] ++ (passiveEl <$> s.passives) ++
      [ _h 2 "Traits"
      , H.section_ $ traitEl <$> s.traits
      ]
    ]
  where 
    rate label n = H.tr_ [_th label, H.td [_c "rating"] ∘ _txt ∘ joinWith " " 
                 $ replicate n "⬛︎" ]

activeEl ∷ ∀ a b. Active -> HTML a b
activeEl {name, icon, cd, effect} = H.section_ $
    [ _img $ "img/Skill/" ++ show icon ++ ".png"
    , _h 3 name
    , _span $ "CD: " ++ show cd
    ] ++ (_p ∘ show <$> effect)

passiveEl ∷ ∀ a b. Passive -> HTML a b
passiveEl {name, rank, icon, effect} = H.section_ $
    [ _img $ "img/Skill/" ++ show icon ++ ".png" 
    , _h 3 $ name ++ " " ++ show rank
    ] ++ (_p ∘ show <$> effect)

traitEl ∷ ∀ a b. Trait -> HTML a b
traitEl trait = H.span [_c "trait"] ∘ _txt $ show trait

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
  [ H.thead_ [ H.tr_ $ H.th_ ∘ _txt <$> headings ]
  , H.tbody_ tbody 
  ]

_tr ∷ ∀ a b. String -> String -> HTML a b
_tr a b = H.tr_ [ _th a, _td b ]

_radio ∷ ∀ a b. String -> Boolean -> Array (HTML a b)
_radio label checked
    = [ H.input [ P.type_ P.InputRadio, P.checked checked ]
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

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _7_ ∷ Generic SortBy _
derive instance _8_ ∷ Eq SortBy
derive instance _9_ ∷ Ord SortBy
instance _10_ ∷ Show SortBy where
  show StarRate = "Star Rate"
  show NPDamage = "NP Damage"
  show a = genericShow a
instance _11_ ∷ Enum SortBy where
  succ = genericSucc
  pred = genericPred
instance _12_ ∷ Bounded SortBy where
  top = genericTop
  bottom = genericBottom
instance _13_ ∷ BoundedEnum SortBy where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
