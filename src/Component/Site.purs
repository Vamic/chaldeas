module Component.Site where

import Prelude
import Operators

import Halogen                 as Halogen
import Halogen.HTML            as H

import Data.Array            (all, cons, delete, filter, replicate)
import Data.Maybe 
import Data.String           (joinWith)
import Halogen.HTML          (HTML)
import Halogen               (Component, ComponentDSL, ComponentHTML, modify_)
import Data.Enum     
import Data.Generic.Rep    
import Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum 
import Data.Generic.Rep.Show 

import Component.Common
import Database

type Input = Unit
type Message = Void
data Query a = Focus (Maybe Servant) a
             | ToActive ActiveEffect a
             | ToNP FilterNP a
             | ToTab FilterTab a
             | UnActive ActiveEffect a

data FilterNP = AnyTarget | SingleTarget | MultiTarget
instance showFilterNP ∷ Show FilterNP where
  show = case _ of
    AnyTarget → "All"
    SingleTarget → "Single-Target"
    MultiTarget → "Multi-Target"

matchNP ∷ FilterNP → NoblePhantasm → Boolean
matchNP AnyTarget     _ = true
matchNP SingleTarget np = not $ isMulti np
matchNP MultiTarget  np = isMulti np

data FilterTab = Actions | Buffs | Debuffs

type State = { filterEffect ∷ Array ActiveEffect
             , filterNP     ∷ FilterNP
             , filterTab    ∷ FilterTab
             , focus        ∷ Maybe Servant
             }

component ∷ ∀ m. Component HTML Query Unit Message m
component = Halogen.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState ∷ Input → State
  initialState = const { filterEffect: []
                       , filterNP:     AnyTarget
                       , filterTab:    Buffs
                       , focus:        Nothing
                       }

  render ∷ State → ComponentHTML Query
  render {filterEffect, filterNP, filterTab, focus} 
      = H.div [_i "layout"]
        [ H.aside [_i "filters"]
          [ H.section_
            [ H.header_ $ _txt "Filter by Noble Phantasm"
            , H.nav_ $ enumArray ↦ \np
              → H.span [_click $ ToNP np] ∘ _txt $ show np
            ]
          , H.section_
            [ H.header_ $ _txt "Filter by effect"
            , H.section_ $ filterEffect ↦ \effect
              → H.p [_click $ UnActive effect] ∘ _txt $ show effect
            , H.nav_ $ enumArray ↦ \tab 
              → H.span [_click $ ToTab tab] ∘ _txt $ show tab
            , H.section_ $ filters ↦ \effect
              → H.p [_click $ ToActive effect] ∘ _txt $ show effect
            ]
          ]
        , H.section [_i "servants"] $ filter match servants ↦ \serv@(Servant s) 
          → H.div [_c $ "stars" ⧺ show s.rarity, _click ∘ Focus $ Just serv]
            [ H.img [_src $ "img/Servant/" ⧺ s.name ⧺ ".png"]
            , H.header_ [ _span s.name ]
            , H.footer_ ∘ _txt ∘ joinWith "  " $ replicate s.rarity "★" 
            ]
        ]
    where
      match serv@(Servant s) = all (_ `hasActive` serv) filterEffect
                             ∧ matchNP filterNP s.phantasm
      filters = (_ ∖ filterEffect) $ case filterTab of
        Actions → toActive ↤ (getAll ∷ Array InstantEffect)
        Buffs   → toActive ↤ (getAll ∷ Array BuffEffect)
        Debuffs → toActive ↤ (getAll ∷ Array DebuffEffect)
      modal Nothing = H.div [_i "layout"]
      modal (Just (Servant s)) = H.div [_i "layout", _c "fade"] ∘ cons 
        ( H.article_ 
          [ ]
        )

  eval ∷ Query ~> ComponentDSL State Query Message m
  eval = case _ of
      Focus    focus  next → (_ ≫ next) $ modify_ _{ focus = focus }
      ToActive active next → (_ ≫ next) $ modFilters (cons active)
      ToNP     np     next → (_ ≫ next) $ modify_ _{ filterNP = np }
      ToTab    tab    next → (_ ≫ next) $ modify_ _{ filterTab = tab }
      UnActive active next → (_ ≫ next) $ modFilters (delete active)
    where
      modFilters f = modify_ \state@{filterEffect} 
                   → state{ filterEffect = f filterEffect }

-------------------------------
-- GENERICS BOILERPLATE; IGNORE
-------------------------------

derive instance _0_ ∷ Generic FilterTab _
derive instance _1_ ∷ Eq FilterTab
derive instance _2_ ∷ Ord FilterTab
instance _3_ ∷ Show FilterTab where
  show = genericShow
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

derive instance _7_ ∷ Generic FilterNP _
derive instance _8_ ∷ Eq FilterNP
derive instance _9_ ∷ Ord FilterNP
instance _11_ ∷ Enum FilterNP where
  succ = genericSucc
  pred = genericPred
instance _12_ ∷ Bounded FilterNP where
  top = genericTop
  bottom = genericBottom
instance _13_ ∷ BoundedEnum FilterNP where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
