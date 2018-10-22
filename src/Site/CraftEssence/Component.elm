module Site.CraftEssence.Component exposing (Model, Msg, component)

import Html.Keyed         as Keyed
import List.Extra         as List
import Browser.Navigation as Navigation
import Browser   exposing (Document)
import Date      exposing (Date)
import Html.Lazy exposing (lazy3)

import Html.Events     as E
import Html            as H exposing (Html)
import Html.Attributes as P

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)
import Database.Skill        exposing (..)
import Persist.Preferences   exposing (..)
import Printing              exposing (..)
import Site.Algebra          exposing (..)
import Site.Common           exposing (..)
import Site.Filtering        exposing (..)
import Site.Rendering        exposing (..)
import Site.Update           exposing (..)
import Sorting               exposing (..)

import Class.Has     as Has
import Class.ToImage as ToImage

import Site.CraftEssence.Filters exposing (..)
import Site.CraftEssence.Sorting exposing (..)

type alias Model = SiteModel CraftEssence CraftEssence ()

type alias Msg = SiteMsg CraftEssence CraftEssence Servant

reSort : Model -> Model
reSort st = { st | sorted = getSort st.sortBy }

component : (String -> Value -> Cmd Msg) -> Component Model Msg
component store =
  let
    init : Value -> Navigation.Key -> Model
    init flags navKey =
        siteInit (collectFilters getFilters) flags navKey ()
        |> reSort
        >> updateListing identity
        >> \st -> { st | root = "CraftEssences" }
    
    view : Model -> Html Msg
    view st = 
      let
        nav = 
            [ text_ H.strong "Craft Essences"
            , a_ "Servants" <| Switch Nothing
            ]
      in
        lazy3 unlazyView st.prefs st.listing st.sortBy
        |> siteView st [Rarity, ID, ATK, HP] nav
        >> popup st.prefs st.focus

    unlazyView prefs listing sortBy = 
        listing
        |> List.map (keyedPortrait False prefs)
        |> doIf (sortBy /= Rarity) List.reverse
        >> Keyed.node "section" [P.id "content"]
    
    update : Msg -> Model -> (Model, Cmd Msg)
    update = siteUpdate store identity .name reSort
  in
    { init = init, view = view, update = update }

portrait : Bool -> Preferences -> (String, CraftEssence) -> Html Msg
portrait big prefs (label, ce) = 
  if not big && prefer prefs Thumbnails then 
    H.div [P.class "thumb", E.onClick << Focus <| Just ce]
    [ToImage.thumbnail <| ToImage.craftEssence ce]
  else
    let
      noBreak  = noBreakName big False
      artorify = doIf (prefer prefs Artorify) <| 
                 String.replace "Altria" "Artoria"
      meta     = doIf (not big) ((::) (E.onClick << Focus <| Just ce)) <|
                 [P.class <| "portrait stars" ++ String.fromInt ce.rarity]
      addLabel = 
          doIf (label /= "") <| (++)
          [text_ H.span <| noBreak label, H.br [] []]
    in
      H.div meta
      [ ToImage.image <| ToImage.craftEssence ce 
      , H.header [] << addLabel <|
        [text_ H.span << noBreak <| artorify ce.name]
      , H.footer []
        [text_ H.span <| stars True ce.rarity]
      ]


keyedPortrait : Bool -> Preferences -> (String, CraftEssence) 
             -> (String, Html Msg)
keyedPortrait big prefs (label, ce) =
    (ce.name, lazy3 portrait big prefs (label, ce))

popup : Preferences -> Maybe CraftEssence -> List (Html Msg) -> Html Msg
popup prefs a = case a of
  Nothing -> 
    H.div [P.class <| mode prefs] << (++)
    [ H.div [P.id "cover", E.onClick <| Focus Nothing] []
    , H.article [P.id "focus"] []
    ]
  Just ce ->
    let
      {base, max} = ce.stats
      bondLink bond = 
          E.onClick << Switch <| List.find (.name >> (==) bond) servants
      bondMsg = case ce.bond of
        Nothing   -> []
        Just bond -> [ H.em []
                        [ H.text "If equipped by "
                        , H.a 
                          [href_ "CraftEssences", P.class "link", bondLink bond] 
                          [H.text bond]
                        , H.text ": "
                        ]
                      ]
      effectsEl f = 
          H.section [] << (++) bondMsg <<
          flip List.map ce.effect <|
          mapAmount f 
          >> effectEl craftEssences (Just .effect)
      mlbEl = 
        if base == max then 
          [] 
        else 
          [effectsEl <| \x _ -> Flat x, h_ 2 "Max Limit Break"]
      showInt = 
          toFloat 
          >> commas
          >> H.text
          >> List.singleton
    in
      H.div [P.class <| mode prefs ++ " fade"] << (++)
      [ H.div [P.id "cover", E.onClick <| Focus Nothing] []
      , H.article [P.id "focus"] 
        [ H.div [] 
          [ portrait True prefs ("", ce)
          , H.div [] <|
            [ table_ ["", "ATK", "HP"] 
              [ H.tr []
                [ text_ H.th "Base"
                , H.td [] <| showInt base.atk
                , H.td [] <| showInt base.hp 
                ]
              , H.tr []
                [ text_ H.th "Max"
                , H.td [] <| showInt max.atk
                , H.td [] <| showInt max.hp 
                ]
              ]
            , ToImage.image <| ToImage.icon ce.icon
            , h_ 2 "Effects"
            ] ++ mlbEl ++
            [ effectsEl <| \_ y -> Flat y ]
          ]
        ]
      ]
