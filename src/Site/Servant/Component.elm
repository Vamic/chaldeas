module Site.Servant.Component exposing (Model, Msg, component, setRoot)

import Json.Decode        as Decode
import Json.Encode        as Encode
import Html.Keyed         as Keyed
import List.Extra         as List
import Maybe.Extra        as Maybe
import Browser.Navigation as Navigation
import Dict      exposing (Dict)
import Html.Lazy exposing (lazy4, lazy5)

import Html.Events     as E
import Html            as H exposing (Html)
import Html.Attributes as P

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.Base         exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)
import Database.Skill        exposing (..)
import Persist.Flags         exposing (..)
import Persist.Preferences   exposing (..)
import Printing              exposing (..)
import MyServant             exposing (..)
import MyServant.Leveling    exposing (..)
import Site.Algebra          exposing (..)
import Site.Base             exposing (..)
import Site.Common           exposing (..)
import Site.Filtering        exposing (..)
import Site.Rendering        exposing (..)
import Site.Update           exposing (..)
import Sorting               exposing (..)

import Class.Has     as Has     exposing (Has)
import Class.ToImage as ToImage exposing (ImagePath)
import Class.Show    as Show
import LZW

import Site.Servant.Filters exposing (..)
import Site.Servant.Sorting exposing (..)

type alias Model = SiteModel Servant MyServant
    { mine     : Dict OrdServant MyServant
    , mineOnly : Bool
    , ascent   : Int
    , myServs  : List MyServant
    , export   : Maybe String
    , entry    : String
    }

type alias Msg = SiteMsg Servant MyServant

reSort : Preferences -> Model -> Model
reSort prefs st =
    { st
    | sorted = getSort (prefer prefs AddSkills) st.sortBy st.extra.myServs
    }

reMine : Model -> Model
reMine ({extra} as st) =
    { st
    | extra = { extra | myServs = List.map (owned st.extra.mine) servants } 
    }

getRoot : Bool -> String
getRoot mineOnly = if mineOnly then "MyServants" else "Servants"

setRoot : Model -> Model
setRoot st = { st | root = getRoot st.extra.mineOnly }

component : (String -> Value -> Cmd Msg) -> Component Model Msg
component store =
  let
    init : Flags -> Navigation.Key -> Model
    init flags navKey =
        siteInit (collectFilters getFilters) flags navKey
            { mine = flags.mine
            , mineOnly = False
            , ascent = 1
            , myServs = []
            , export = Nothing
            , entry = ""
            }
        |> reMine
        >> reSort flags.preferences
        >> updateListing flags.preferences .base
        >> setRoot

    view : Preferences -> Model -> Html Msg
    view prefs st =
      let
        nav =
            [ a_ ["Craft Essences"]
            , if st.extra.mineOnly then
                a_ ["Servants"]
              else
                text_ H.strong "Servants"
            , if st.extra.mineOnly then
                text_ H.strong "My Servants"
              else
                a_ ["My Servants"]
            --, a_ ["Teams"]
            ]
      in
        lazy4 unlazyView prefs st.extra.mineOnly st.listing st.sortBy
        |> siteView prefs st enumSortBy nav
        >> popup prefs st.extra.mineOnly st.extra.ascent st.focus st.extra.export

    unlazyView prefs mineOnly listing sortBy =
      let
        baseAscend = if prefer prefs MaxAscension then 4 else 1
        doPortrait (label, ms) =
            portrait False prefs mineOnly baseAscend
            ( if label == "" && mineOnly then showStats ms else label
            , ms
            )
        withStats (_, ms) = (showStats ms, ms)
        listed =
          if mineOnly then
            listing
            |> List.filter isMine
            >> if sortBy /= Rarity then identity else List.map withStats
          else
            listing
        getMats label f = case f <| List.map Tuple.second listed of
          []   -> []
          mats ->
            [ ( label ++ "H"
              , h_ 2 <| "Total " ++ label ++ " Materials Needed"
              )
            , ( label
              , H.footer [P.class "materials"] <| List.map materialEl mats
              )
            ]
      in
        listed
        |> List.map (keyedPortrait False prefs mineOnly baseAscend)
        >> (if sortBy == Rarity then identity else List.reverse)
        >> ( if not mineOnly then identity else
             flip (++) <|
             [ ("I/O", H.div [P.id "io"] 
               [ button_ "Export" True <| Export True
               , button_ "Import" True <| Export False
               ])
             ]
             ++ getMats "Ascension" ascendWishlist
             ++ getMats "Skill" skillWishlist
           )
        >> Keyed.node "section" [P.id "content"]

    update : Preferences -> Msg -> Model -> (Model, Cmd Msg)
    update prefs a st =
      let
        {extra} = st
        relist  = updateListing prefs .base
      in case a of
        Ascend ms ascent -> case ms.level of
          0 -> pure { st | extra = { extra | ascent = ascent } }
          _ -> update prefs (OnMine True { ms | ascent = ascent }) st
        Focus focus ->
            ( { st | focus = focus, extra = { extra | ascent = 1 } }
            , setFocus st.navKey st.root <| Maybe.map (.base >> .name) focus
            )
        OnMine keep msPreCalc ->
          let
            ms   = (if not keep then identity else recalc) msPreCalc
            mine =
              if keep then
                Dict.insert (ordServant ms.base) ms st.extra.mine
              else
                Dict.remove (ordServant ms.base) st.extra.mine
          in
            ( relist << reSort prefs <| reMine
              { st
              | extra = { extra | mine = mine }
              , focus = Maybe.next st.focus <| Just ms
              }
            , storeMine store mine
            )
        Entry x -> pure { st | extra = { extra | entry = x }}
        Import  -> 
          case Decode.decodeString decodeMine <| LZW.decompress st.extra.entry of
            Err err -> pure
              { st | extra = { extra | export = Just <| Decode.errorToString err } }
            Ok mine -> 
              ( relist << reSort prefs <| reMine 
                { st | extra = { extra | mine = mine, export = Nothing } }
              , storeMine store mine
              )
        Export actual -> pure 
            { st 
            | extra = 
              { extra 
              | export = Just <| 
                         if not actual then "" 
                         else LZW.compress << Encode.encode 0 <| 
                         encodeMine st.extra.mine
              } 
            }
        _ -> siteUpdate .base .name (reSort prefs) prefs a st
  in
    { init = init, view = view, update = update }

showStats : MyServant -> String
showStats ms =
    String.fromInt ms.level ++ "/"
    ++ String.fromInt (maxLevel ms.servant) ++ " "
    ++ String.join "·" (List.map String.fromInt ms.skills)

isMine : (a, MyServant) -> Bool
isMine (_, ms) = ms.level /= 0

portrait : Bool -> Preferences -> Bool -> Int -> (String, MyServant) 
        -> Html Msg
portrait big prefs mineOnly baseAscension (label, ms) =
  if not big && prefer prefs Thumbnails then
    H.a 
    [ P.class "thumb"
    , P.href <| "/" ++ getRoot mineOnly ++  "/" ++ urlName ms.base.name
    , E.onClick << Focus <| Just ms
    ]
    [ToImage.thumbnail <| ToImage.servant ms.base]
  else
    let
      class  = P.class <| "portrait stars" ++ String.fromInt ms.base.rarity
      parent =
        if big then 
          H.div [class]
        else 
          H.a 
          [ class
          , E.onClick << Focus <| Just ms
          , P.href <| "/" ++ getRoot mineOnly ++ "/" ++ urlName ms.base.name
          ]          
      noBreak  = noBreakName big <| prefer prefs HideClasses
      artorify = if not <| prefer prefs Artorify then identity else
                 String.replace "Altria" "Artoria"
      addLabel =
          if label == "" then identity else (++)
          [text_ H.span <| noBreak label, H.br [] []]
      ascension = if ms.level /= 0 then ms.ascent else baseAscension
      ascent = if ascension <= 1 then "" else " " ++ String.fromInt ascension
    in
      parent
      [ ToImage.image << ImagePath "Servant" <| ms.base.name ++ ascent
      , H.div [] [ ToImage.image <| ToImage.class ms.base.class ]
      , H.header [] << addLabel <|
        [text_ H.span << noBreak <| artorify ms.base.name]
      , if big then
          H.footer []
          [ button_ "<" (ascension > 1) << Ascend ms <| ascension - 1
          , text_ H.span <| stars True ms.base.rarity
          , button_ ">" (ascension < 4) << Ascend ms <| ascension + 1
          ]
        else
          H.footer [] [text_ H.span <| stars True ms.base.rarity]
      ]

keyedPortrait : Bool -> Preferences -> Bool -> Int -> (String, MyServant)
             -> (String, Html Msg)
keyedPortrait big prefs mineOnly baseAscension (label, ms) =
    (ms.base.name, lazy5 portrait big prefs mineOnly baseAscension (label, ms))

popup : Preferences -> Bool -> Int -> Maybe MyServant -> Maybe String
     -> List (Html Msg) -> Html Msg
popup prefs mineOnly ascent a ex = case (ex, a) of
  (Just "", _) ->
    H.div [P.id "elm", P.class <| mode prefs ++ " fade"] << (++)
    [ H.a [P.id "cover", P.href <| "/" ++ getRoot mineOnly] []
    , H.article [P.id "focus"] 
      [ H.textarea [E.onInput Entry] []
      , button_ "Import" True Import
      ]
    ]
  (Just export, _) ->
    H.div [P.id "elm", P.class <| mode prefs ++ " fade"] << (++)
    [ H.a [P.id "cover", P.href <| "/" ++ getRoot mineOnly] []
    , H.article [P.id "focus"] [H.textarea [P.value export] []]
    ]
  (_, Nothing) ->
    H.div [P.id "elm", P.class <| mode prefs] << (++)
    [ H.a [P.id "cover", P.href <| "/" ++ getRoot mineOnly] []
    , H.article [P.id "focus"] []
    ]
  (_, Just ms) ->
    let
      b   = ms.base
      s   = ms.servant
      fou = ms.fou
      link ({show} as has) tab x =
          H.a
          [ P.href <| "/" ++ getRoot mineOnly
          , E.onClick << FilterBy <| singleFilter has tab x
          ]
          [H.text <| show x]
      npRank rank   = case rank of
        Unknown -> "--"
        _       -> Show.rank rank
      {base}        = s.stats
      {max, grail}  = b.stats
      showTables    = prefer prefs ShowTables
      showTable showCol effects =
          if not showTables then identity else consAfter <<
          table_ (List.map showCol <| List.range 1 5)
          <| List.map npRow (List.uniqueBy ordRangeInfo <| ranges effects)
      overMeta      = if s.phantasm.first then [P.class "activates"] else []
      linkAlignment = link Has.alignment FilterAlignment
      alignBox      = case s.align of
        [] ->
            [H.text "None"]
        [Neutral, Neutral] ->
            [H.text "True ", linkAlignment Neutral]
        [a1, a2, a3, a4] ->
            [ linkAlignment a1
            , H.text " "
            , linkAlignment a2
            , H.text " / "
            , linkAlignment a3
            , H.text " "
            , linkAlignment a4
            ]
        _ -> s.align |> List.concatMap (\x -> [linkAlignment x, H.text " "])
      calcWith = if prefer prefs AddSkills then Tuple.first else Tuple.second
      calc sort =
          Dict.get (ordSortBy sort) ms.sorted
          |> Maybe.withDefault (1/0, 1/0)
          >> calcWith
          >> formatSort sort
      skillBox i ({icon}, lvl) =
          [ H.td [] [ToImage.image <| ToImage.icon icon]
          , H.td [] << int_ 1 10 lvl <| \val ->
              OnMine True
              { ms | skills = List.updateAt i (always val) ms.skills }
          ]
      myServantBox = List.singleton <| case ms.level of
        0 ->
            button_ "+Add to My Servants" True << OnMine True <| newServant s
        _ ->
            H.table []
            [ H.tr []
                [ H.td [] [text_ H.strong "Level:"]
                , H.td [] << int_ 1 100 ms.level <| \val ->
                  OnMine True { ms | level = val }
                , H.td [] [text_ H.strong "NP:"]
                , H.td [] << int_ 1 5 ms.npLvl <| \val ->
                  OnMine True { ms | npLvl = val }
                , H.td [] [text_ H.strong "+ATK:"]
                , H.td [] << int_ 0 990 fou.atk <| \val ->
                  OnMine True { ms | fou = { fou | atk = val } }
                , H.td [] [text_ H.strong "+HP:"]
                , H.td [] << int_ 0 990 fou.hp <| \val ->
                  OnMine True { ms | fou = { fou | hp = val } }
                ]
            , H.tr [] << (++)
                [ H.td [] [button_ "Delete" True << OnMine False <| unowned s]
                , H.td [] [text_ H.strong "Skills:"]
                ] << List.concat << List.map2 skillBox (List.range 0 10) <|
                  List.zip s.skills ms.skills
            ]
      showInt =
          toFloat
          >> commas
          >> H.text
          >> List.singleton
      showPercent =
          String.fromFloat
          >> flip (++) "%"
          >> H.text
          >> List.singleton
    in
      H.div [P.id "elm", P.class <| mode prefs ++ " fade"] << (++)
      [ H.a [P.id "cover", P.href <| "/" ++ getRoot mineOnly] []
      , H.article [P.id "focus"] <|
        [ H.div []
          [ portrait True prefs mineOnly ascent ("", ms)
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
                , H.tr []
                [ text_ H.th "Grail"
                , H.td [] <| showInt grail.atk
                , H.td [] <| showInt grail.hp
                ]
                ]
            , table_ ["", "Q", "A", "B", "EX", "NP"]
                [ H.tr [] <<
                (::) (text_ H.th "Hits") <|
                List.map (String.fromInt >> text_ H.td)
                [ s.hits.quick
                , s.hits.arts
                , s.hits.buster
                , s.hits.ex
                , s.phantasm.hits
                ]
                ]
            , H.table []
                [ tr_ "Class"       [ link Has.class FilterClass s.class ]
                , tr_ "Deck"        [ link Has.deck FilterDeck s.deck ]
                , tr_ "NP Type"     [ link Has.phantasmType FilterPhantasm <|
                                    phantasmType s.phantasm
                                  ]
                , tr_ "Attribute"   [ link Has.attribute FilterAttribute s.attr ]
                , tr_ "Alignment"   alignBox
                , tr_ "ID"          [H.text <| "#" ++ String.fromInt s.id]
                , tr_ "Star Weight" [H.text <| String.fromInt s.gen.starWeight]
                , tr_ "Star Rate"   <| showPercent s.gen.starRate
                , tr_ "NP/Hit"      <| showPercent s.gen.npAtk
                , tr_ "NP/Defend"   [H.text <| String.fromInt s.gen.npDef ++ "%"]
                , tr_ "Death Rate"  <| showPercent s.death
                ]
            ]
          ]
        , H.form [P.id "myservant"] myServantBox
        , h_ 2 "Noble Phantasm"
        , H.table [P.id "phantasm"]
          [ tr_ "Name" [H.text s.phantasm.name]
          , tr_ "Rank" [H.text <| npRank s.phantasm.rank]
          , tr_ "Card" [link Has.card FilterCard s.phantasm.card]
          , tr_ "Class" [H.text s.phantasm.kind]
          , tr_ "Effects" <<
            showTable (String.fromInt >> (++) "NP")
            b.phantasm.effect <|
            List.map (effectEl servants <| Just Has.servant) s.phantasm.effect
          , tr_ "Overcharge" <<
            showTable ((*) 100 >> String.fromInt >> flip (++) "%")
            b.phantasm.over <|
            List.map (effectEl servants <| Just Has.servant) s.phantasm.over
          ]
        , h_ 2 "Active Skills"
        ] ++ List.map2 (skillEl showTables) s.skills b.skills ++
        [ h_ 2 "Passive Skills"
        ] ++ List.map passiveEl s.passives ++ bondEl (getBond s) ++
        [ h_ 2 "Traits"
        , H.section [] << List.intersperse (H.text ", ") <|
          List.map (link Has.trait FilterTrait) s.traits
        , h_ 2 "Ascension"
        , H.table [P.class "materials"] <<
          flip List.indexedMap (ascendUpEl s.ascendUp) <| \i el ->
              H.tr []
                [ text_ H.th << String.fromInt <| i + 1
                , H.td [] <| withCost (ascendCost s i) el
                ]
        , h_ 2 "Skill Reinforcement"
        , H.table [P.class "materials"] <<
          flip List.indexedMap (skillUpEl s.skillUp) <| \i el ->
              H.tr []
                [ text_ H.th << String.fromInt <| i + 2
                , H.td [] <| withCost (skillCost s i) el
                ]
        , h_ 2 "Calculator"
        , H.table [P.id "calc"]
          [ H.tr []
            [ H.td []
                [ h_ 3 "NP Generation"
                , H.table []
                [ tr_ "Per Arts card" [H.text <| calc NPArts]
                , tr_ "Per full deck" [H.text <| calc NPDeck]
                ]
                ]
            , H.td []
                [ h_ 3 "NP Damage"
                , H.table []
                [ tr_ "100% Overcharge" [H.text <| calc NPDmg]
                , tr_ "500% Overcharge" [H.text <| calc NPDmgOver]
                ]
                ]
            ]
          , H.tr []
            [ H.td []
                [ h_ 3 "Star Generation"
                , H.table []
                [ tr_ "Per Quick card" [H.text <| calc StarQuick]
                , tr_ "Per full deck"  [H.text <| calc StarDeck]
                ]
                ]
            , H.td []
                [ h_ 3 "NP Special Damage"
                , H.table []
                [ tr_ "100% Overcharge" [H.text <| calc NPSpec]
                , tr_ "500% Overcharge" [H.text <| calc NPSpecOver]
                ]
                ]
            ]
          ]
        ]
      ]

ascendUpEl : Ascension -> List (List (Html Msg))
ascendUpEl x = case x of
  Clear a b c d ->
      flip List.map [a, b, c, d] <|
      (++) "Clear "
      >> H.text
      >> List.singleton
  Welfare a ->
      ImagePath "Material" a
      |> ToImage.image
      >> List.singleton
      >> List.repeat 4
  Ascension a b c d ->
      [a, b, c, d]
      |> List.map (List.map materialEl)

skillUpEl : Reinforcement -> List (List (Html Msg))
skillUpEl (Reinforcement a b c d e f g h) =
    [a, b, c, d, e, f, g, h, [ (CrystallizedLore, 1) ]]
    |> List.map (List.map materialEl)

withCost : Int -> List (Html Msg) -> List (Html Msg)
withCost a = case a of
  0 -> identity
  _ -> (::) <| materialEl (QP, a)

materialEl : (Material, Int) -> Html Msg
materialEl (mat, amt) =
  let
    imageLinkEl =
      ( if ignoreMat mat then identity else (++) 
        [ P.class "link"
        , E.onClick << FilterBy <| singleFilter Has.material FilterMaterial mat
        ] 
      )
      [ ToImage.src <| ToImage.material mat
      , P.title <| Show.material mat
      ]
  in
    H.div []
    [ H.img imageLinkEl []
    , text_ H.span <| "×" ++ commas (toFloat amt)
    ]

skillEl : Bool -> Skill -> Skill -> Html Msg
skillEl showTables sk base =
  let
    effectTable =
        table_ (List.map String.fromInt <| List.range 1 10) <<
        List.map lvlRow << List.uniqueBy ordRangeInfo <| ranges base.effect
  in
    H.section [] << 
    (if not showTables then identity else consAfter effectTable) <|
    [ ToImage.image <| ToImage.icon sk.icon
    , h_ 3 <| sk.name ++ Show.rank sk.rank
    , text_ H.strong "CD: "
    , H.text <<
      ( if sk /= base then identity 
        else flip (++) <| "~" ++ String.fromInt (sk.cd - 2)
      ) <|
      String.fromInt sk.cd
    ] ++ List.map (effectEl servants <| Just Has.servant) sk.effect

passiveEl : Skill -> Html Msg
passiveEl p =
  let
    filter =
        matchFilter (Just <| .icon >> ToImage.icon) Has.passive
        FilterPassiveSkill p
  in
    H.section [] <|
    [  ToImage.image <| ToImage.icon p.icon
    , H.h3 []
      [ H.span [P.class "link", E.onClick <| FilterBy [filter]] [H.text p.name]
      , H.text <| " " ++ Show.rank p.rank
      ]
    ] ++ List.map (Show.skillEffect >> text_ H.p) p.effect

bondEl : Maybe CraftEssence -> List (Html Msg)
bondEl a = case a of
  Nothing -> []
  Just ce ->
      [ h_ 2 "Max-Bond Craft Essence"
      , H.section []
        [ ToImage.image <| ToImage.icon ce.icon
        , H.h3 [] [a_ ["Craft Essences", ce.name]]
        , H.p [] <|
          [ text_ H.span "★★★★ "
          , text_ H.strong "ATK: ", text_ H.span "100 "
          , text_ H.strong "HP: ", text_ H.span "100"
          ] ++ List.map (effectEl servants Nothing) ce.effect
        ]
      ]

npRow : RangeInfo -> Html Msg
npRow r =
  let
    step = r.max - r.min
    col  =
        (*) step
        >> (+) r.min
        >> toCell r.percent
  in
    H.tr [] <| List.map col [0, 0.5, 0.75, 0.825, 1.0]

overRow : RangeInfo -> Html Msg
overRow r =
  let
    step = (r.max - r.min) / 4
    col  =
        toFloat
        >> (*) step
        >> (+) r.min
        >> toCell r.percent
  in
    H.tr [] << List.map col <| List.range 0 4
