module Component where

import Prelude
import Operators

import Halogen                 as Halogen
import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P

import Data.Array              (all, cons, delete, filter, find, replicate, sortWith)
import Data.Maybe 
import Effect.Class            (class MonadEffect)
import Halogen.HTML            (HTML)
import Halogen.HTML.Properties (IProp)
import Web.UIEvent.MouseEvent  (MouseEvent)
import Data.Enum     
import Data.Int                (toNumber)
import Data.Formatter.Number
import Data.Profunctor.Strong
import Data.Generic.Rep    
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum 
import Data.Generic.Rep.Show 
import Data.String
import Data.Tuple              (uncurry)
import Routing.Hash

import Halogen (ClassName(..), Component, ComponentDSL, ComponentHTML, liftEffect, modify_)

import Database

print' ∷ Number → String
print' = format $ Formatter { comma: true
                            , before: 0
                            , after: 0
                            , abbreviations: false
                            , sign: false
                            } 
print ∷ Int → String
print = print' ∘ toNumber


type Input = Unit
type Message = Void
data Query a = ToTab FilterTab a
             | Focus (Maybe Servant) a
             | AddFilter Filter a
             | UnFilter Filter a

data FilterTab = Phantasms
               | Actions | Buffs | Debuffs 
               | Classes | Traits | Attributes | Alignments

data Filter = Filter String (Servant → Boolean)
instance _b_ ∷ Eq Filter where
  eq (Filter a _) (Filter b _) = eq a b
instance _c_ ∷ Show Filter where
  show (Filter a _) = a
matchFilter ∷ ∀ a. MatchServant a ⇒ a → Filter
matchFilter = uncurry Filter ∘ (show&&&has)
  
type State = { filters   ∷ Array Filter
             , filterTab ∷ FilterTab
             , focus     ∷ Maybe Servant
             , sorter    ∷ Servant → String
             }

urlName ∷ Servant → String
urlName (Servant {name}) = replaceAll (Pattern " ") (Replacement "") name

component ∷ ∀ m. MonadEffect m ⇒ String → Component HTML Query Unit Message m
component initialHash = Halogen.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState ∷ Input → State
  initialState = const { filters:   []
                       , filterTab: Buffs
                       , focus:     find ((_ ≡ initialHash) ∘ urlName) servants
                       , sorter:    \(Servant s) → show (5 - s.rarity) ⧺ s.name
                       }

  render ∷ State → ComponentHTML Query
  render {filters, filterTab, focus, sorter} 
      = modal focus
        [ H.aside [_i "active"] ∘ cons 
          ( _h 1 "Active filters" )
          $ filters ↦ \filt 
            → H.p [_click $ UnFilter filt] ∘ _txt $ show filt 
        , H.section [_i "servants"]
          $ portrait ↤ sortWith sorter' (filter match servants)
        , H.aside [_i "filters"] ∘ append
          [ _h 1 "Filters"
          , H.nav_ $ enumArray ↦ \tab 
              → H.h4 [_click $ ToTab tab] ∘ _txt $ show tab
          ] $ filterEffects ↦ \filt 
              → H.p [_click $ AddFilter filt] ∘ _txt $ show filt
        ]
    where
      sorter' serv@(Servant s) = sorter serv ⧺ s.name
      --match (Servant s) = Male ∉ s.traits ∧ Female ∉ s.traits
      match serv = all (\(Filter _ f) → f serv) filters
      filterEffects = case filterTab of
          Actions    → matchFilter ↤ getAll ∷ Array InstantEffect
          Alignments → matchFilter ↤ getAll ∷ Array Alignment
          Attributes → matchFilter ↤ getAll ∷ Array Attribute
          Buffs      → matchFilter ↤ getAll ∷ Array BuffEffect
          Classes    → matchFilter ↤ getAll ∷ Array Class
          Debuffs    → matchFilter ↤ getAll ∷ Array DebuffEffect
          Phantasms  → matchFilter ↤ getAll ∷ Array PhantasmType
          Traits     → matchFilter ↤ getAll ∷ Array Trait

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
      ToTab     tab     next → (_ ≫ next) $ modify_ _{ filterTab = tab }
      AddFilter filt    next → (_ ≫ next) $ modFilters (cons filt)
      UnFilter  filt    next → (_ ≫ next) $ modFilters (delete filt)
      Focus     focus   next → (_ ≫ next) $ do
          liftEffect $ hash focus
          modify_ _{ focus = focus } 
    where
      modFilters f = modify_ \state@{filters} → state{ filters = f filters }
      hash Nothing = setHash ""
      hash (Just s) = setHash $ urlName s

noBreakName ∷ String → String
noBreakName = unBreak ∘ split (Pattern "(")
  where
    unBreak [a, b] = a ⧺ "(" ⧺ replaceAll (Pattern " ") (Replacement " ") b
    unBreak xs = joinWith "(" xs

deck ∷ ∀ a b. Deck → Array (HTML a b)
deck (Deck a b c d e) = card ∘ show ↤ [a, b, c, d, e]
  where 
    card x = H.span [_c x] ∘ _txt $ take 1 x --_img $ "img/Card/" ⧺ show x ⧺ ".png"

portrait ∷ ∀ a. Servant → HTML a (Query Unit)
portrait serv@(Servant s) 
    = H.div [_c $ "servant stars" ⧺ show s.rarity, _click ∘ Focus $ Just serv]
      [ _img $ "img/Servant/" ⧺ s.name ⧺ ".png"
      , H.div_ [ _img $ "img/Class/" ⧺ show s.class ⧺ ".png"]
      , H.aside_ $ deck s.deck
      , H.header_ [ _span $ noBreakName s.name ]
      , H.footer_ ∘ _txt ∘ joinWith "  " $ replicate s.rarity "★" 
      ]

modal ∷ ∀ a. Maybe Servant → Array (HTML a (Query Unit)) → HTML a (Query Unit)
modal Nothing = H.div [_i "layout"] ∘ append 
  [ H.div [_i "cover", _click $ Focus Nothing] [], H.article_ [] ]
modal (Just serv@(Servant s@{ratings, gen, hits, stats:{base, max, grail}, phantasm})) 
  = H.div [_i "layout", _c "fade"] ∘ append 
    [ H.div [_i "cover", _click $ Focus Nothing] []
    , H.article_ $
      [ portrait serv
      , _table ["", "ATK", "HP"]
        [ H.tr_ [ _th "Base",  _td $ print base.atk,  _td $ print base.hp ]
        , H.tr_ [ _th "Max",   _td $ print max.atk,   _td $ print max.hp ]
        , H.tr_ [ _th "Grail", _td $ print grail.atk, _td $ print grail.hp ]
        , H.br_
        ]
      , _table ["", "", "Q", "A", "B", "EX", "NP"]
        [ H.tr_
          [ _td ""
          , _th "Hits"
          , _td $ show hits.q
          , _td $ show hits.a
          , _td $ show hits.b
          , _td $ show hits.ex
          , _td $ show phantasm.hits
          ] 
        ]
      , H.table_ 
        [ H.br_
        , _tr "Class"      $ show s.class 
        --, H.tr_ [_th "Deck", H.td_ $ deck s.deck]
        , _tr "NP Type"    ∘ show ∘ fromMaybe Support 
                           $ find (_ `has` serv) [SingleTarget, MultiTarget]
        , _tr "Alignment"  $ showAlignment s.align
        , _tr "Attribute"  $ show s.attr
        , _tr "Star Weight" $ show gen.starAbsorb
        , _tr "Star Rate"   $ show gen.starGen ⧺ "%"
        , _tr "NP/Hit"      $ show gen.npPerHit ⧺ "%"
        , _tr "NP/Defend"   $ show gen.npAttacked ⧺ "%"
        --, _tr "Death Rate"  $ show s.death ⧺ "%"
        {-
        , rate "Damage"     ratings.damage
        , rate "NP Gain"    ratings.np
        , rate "Critical"   ratings.critical
        , rate "Utility"    ratings.utility
        , rate "Support"    ratings.support
        , rate "Durability" ratings.durability
        -}
        ]
      , _h 2 "Active Skills"
      ] ⧺ (activeEl ↤ s.actives) ⧺
      [ _h 2 "Passive Skills" 
      ] ⧺ (passiveEl ↤ s.passives)
    ]
  where 
    rate label n = H.tr_ [_th label, H.td [_c "rating"] ∘ _txt ∘ joinWith " " 
                 $ replicate n "⬛︎" ]

activeEl ∷ ∀ a b. Active → HTML a b
activeEl {name, icon, cd, effect} = H.section_ $
    [ _img $ "img/Skill/" ⧺ show icon ⧺ ".png"
    , _h 3 name
    , _span $ "CD: " ⧺ show cd
    ] ⧺ (_p ∘ show ↤ effect)

passiveEl ∷ ∀ a b. Passive → HTML a b
passiveEl {name, rank, icon, effect} = H.section_ $
    [ _img $ "img/Skill/" ⧺ show icon ⧺ ".png" 
    , _h 3 $ name ⧺ " " ⧺ show rank
    ] ⧺ (_p ∘ show ↤ effect)

----------------
-- ABBREVIATIONS
----------------

_i   ∷ ∀ a b. String → IProp (id ∷ String | b) a 
_i   = P.id_
_c   ∷ ∀ a b. String → IProp (class ∷ String | b) a
_c   = P.class_ ∘ ClassName
_src ∷ ∀ a b. String → IProp (src ∷ String | b) a
_src = P.src
_style ∷ ∀ a b. String → IProp (style ∷ String | b) a
_style = P.attr (H.AttrName "style")

_txt ∷ ∀ a b. String → Array (HTML a b)
_txt = (flip cons) [] ∘ H.text

_a ∷ ∀ a b. String → String → String → String → HTML a b
_a id' class' href' = H.a [_i id', _c class', P.href href'] ∘ _txt

_img ∷ ∀ a b. String → HTML a b
_img src = H.img [_src src]

_click ∷ ∀ a b. (Unit → b Unit) → IProp ( onClick ∷ MouseEvent | a ) (b Unit)
_click = E.onClick ∘ E.input_

_table ∷ ∀ a b. Array String → Array (HTML a b) → HTML a b
_table headings tbody 
    = H.table_ [ H.thead_ [ H.tr_ $ H.th_ ∘ _txt ↤ headings ], H.tbody_ tbody ]

_tr ∷ ∀ a b. String → String → HTML a b
_tr a b = H.tr_ [ _th a, _td b ]

_span ∷ ∀ a b. String → HTML a b
_span = H.span_ ∘ _txt
_p ∷ ∀ a b. String → HTML a b
_p = H.p_ ∘ _txt
_b ∷ ∀ a b. String → HTML a b
_b = H.b_ ∘ _txt
_th ∷ ∀ a b. String → HTML a b
_th = H.th_ ∘ _txt
_td ∷ ∀ a b. String → HTML a b
_td = H.td_ ∘ _txt
_h ∷ ∀ a b. Int → String → HTML a b
_h 1 = H.h1_ ∘ _txt
_h 2 = H.h2_ ∘ _txt
_h 3 = H.h3_ ∘ _txt
_h 4 = H.h4_ ∘ _txt
_h 5 = H.h5_ ∘ _txt
_h _ = H.h6_ ∘ _txt

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Generic FilterTab _
derive instance _1_ ∷ Eq FilterTab
derive instance _2_ ∷ Ord FilterTab
instance _3_ ∷ Show FilterTab where
  show Phantasms = "Noble Phantasm"
  show a = genericShow a
instance _4_ ∷ Enum FilterTab where
  succ = genericSucc
  pred = genericPred
instance _5_ ∷ Bounded FilterTab where
  top = genericTop
  bottom = genericBottom
instance _6_ ∷ BoundedEnum FilterTab where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
