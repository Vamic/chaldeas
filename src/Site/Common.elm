module Site.Common exposing (..)

{-| Utility functions for rendering to HTML. -}

import Browser.Dom        as Dom
import Json.Decode        as Json
import Browser.Navigation as Navigation
import Url.Builder        as Url
import Task exposing (Task)

import Html            as H exposing (Html)
import Html.Events     as E
import Html.Attributes as P

import StandardLibrary     exposing (..)
import Printing            exposing (..)
import Database.Base       exposing (..)
import Database.Skill      exposing (..)
import Persist.Preferences exposing (..)
import Site.Algebra        exposing (..)
import Site.Filtering      exposing (..)

import Class.Show as Show

{-| Scrolls an HTML element to its top.
Used when a Servant or Craft Essence is clicked on. -}
scrollToTop : String -> Cmd (SiteMsg a b)
scrollToTop id = Task.attempt (always DoNothing) <| Dom.setViewportOf id 0 0

{-| Updates the URL in the address bar and adds an entry to browser history. -}
setPath : Navigation.Key -> List String -> Cmd msg
setPath key path =
    Navigation.pushUrl key <| Url.absolute (List.map urlName path) []

{-| Displays details for a Servant or Craft Essence in the popup. -}
setFocus : Navigation.Key -> String -> Maybe String -> Cmd (SiteMsg a b)
setFocus key root a = case a of
  Nothing   -> setPath key [root]
  Just name -> setPath key [root, urlName name]

{-| Displays a number in a `<td>` element, optionally followed by `'%'`. -}
toCell : Bool -> Float -> Html msg
toCell isPercent =
    places 0
    >> (if not isPercent then identity else flip (++) "%")
    >> text_ H.td

{-| Displays a `<tr>` of skill effect values that increase when leveled. -}
lvlRow : RangeInfo -> Html msg
lvlRow r =
  let
    step = (r.max - r.min) / 10
    go   =
        toFloat
        >> (*) step
        >> (+) r.min
        >> toCell r.percent
  in
    List.range 0 8 ++ [10]
    |> List.map go
    >> H.tr []

{-| Converts certain spaces into no-break-spaces in portrait names.
For example, parenthesized phrases such as (Lancer Alter) do not break on lines. -}
noBreakName : Bool -> Bool -> String -> String
noBreakName shouldPrettify hideClasses =
  let
    classNames = List.map Show.class enumClass
    replaceSpaces  = String.replace " " " "
    replacePirates =
        String.replace "Anne Bonny"  "Anne Bonny" >>
        String.replace "& Mary Read" "& Mary Read"
    unBreak xs = case xs of
      [x, y] ->
        if shouldPrettify then
          x
        else if not hideClasses then
          x ++ " (" ++ y
        else x ++ case String.split " " <| stripSuffix ")" y of
          []      -> " (" ++ replaceSpaces y
          w :: ws ->
            if not <| List.member w classNames then
              " (" ++ replaceSpaces y
            else if List.isEmpty ws then
              ""
            else
              " (" ++ replaceSpaces (String.join " " ws) ++ ")"
      ws   -> String.join " (" ws
  in
    String.split " ("
    >> unBreak
    >> replacePirates
    >> if not shouldPrettify then identity else prettify

{-| `"light"` or `"dark"` depending on `NightMode` preference. -}
mode : Preferences -> String
mode prefs = if prefer prefs NightMode then "dark" else "light"

{-| Displays a `SkillEffect` with a link and marks demerits. -}
effectEl : List a -> Maybe (a -> List SkillEffect) -> SkillEffect
        -> Html (SiteMsg a b)
effectEl xs getEffects ef =
    flip H.p [H.text <| Show.skillEffect ef] <|
    if demerit ef then
      [P.class "demerit"]
    else case getEffects |> Maybe.andThen (skillFilter xs ef) of
      Nothing     -> []
      Just filter -> [P.class "link", E.onClick <| FilterBy [filter]]

{-| `<a>` -}
a_ : List String -> Html msg
a_ path = 
  let
    last = Maybe.withDefault "" << List.head <| List.reverse path
  in
    H.a [P.href << (++) "/" << String.join "/" <| List.map urlName path] 
    [H.text last]

{-| `<h[1-6]>` -}
h_ : Int -> String -> Html msg
h_ level = text_ <| case level of
    1 -> H.h1
    2 -> H.h2
    3 -> H.h3
    4 -> H.h4
    5 -> H.h5
    _ -> H.h6

{-| `<button>` -}
button_ : String -> Bool -> msg -> Html msg
button_ label enable click =
  let
    meta =
        P.type_ "button" ::
        if enable then [E.onClick click] else [P.disabled True]
  in
    H.button meta [H.text label]

{-| `<input type="checkbox">` -}
checkbox_ : Maybe (Html msg) -> String -> Bool -> List (Html msg)
checkbox_ icon label checked =
    [ H.input [P.type_ "checkbox", P.checked checked] []
    , H.label [] <| case icon of
        Nothing -> [H.text label]
        Just ic -> [ic, H.text label]
    ]

{-| Fires the `onChange` web event.
Unlike `onInput`, ignores events such as backspaces. -}
onChange : (String -> msg) -> H.Attribute msg
onChange tagger =
  E.stopPropagationOn "change" <<
  Json.map (\x -> (x, True)) <| Json.map tagger E.targetValue

{-| `<input type="number"> with supplied `min`, `max`, and `value`. -}
int_ : Int -> Int -> Int -> (Int -> SiteMsg a b) -> List (Html (SiteMsg a b))
int_ minVal maxVal actualVal changed =
    [ H.input
      [ P.type_      "number"
      , P.value   <| String.fromInt actualVal
      , P.min     <| String.fromInt minVal
      , P.max     <| String.fromInt maxVal
      , P.step    <| String.fromInt 1
      , onChange  <| \val ->
          case String.toInt val of
            Nothing -> DoNothing
            Just intVal ->
              if intVal >= minVal && intVal <= maxVal then
                changed intVal
              else
                DoNothing
      ] []
    , text_ H.span <| "/" ++ String.fromInt maxVal
    ]

{-| `<input type="radio">` -}
radio_ : String -> Bool -> List (Html msg)
radio_ label checked =
    [ H.input [P.type_ "radio", P.checked checked] []
    , text_ H.label label
    ]

{-| `<table>` with supplied headings -}
table_ : List String -> List (Html msg) -> Html msg
table_ headings tbody =
    H.table []
    [ H.colgroup [] <| List.map (always <| H.col [] []) headings
    , H.thead [] [H.tr [] <| List.map (text_ H.th) headings]
    , H.tbody [] tbody
    ]

{-| Wraps text in a web element. -}
text_ : (List p -> List (Html msg) -> Html msg) -> String -> Html msg
text_ el txt = el [] [H.text txt]

{-| `<tr>` with a supplied `th` and `td` -}
tr_ : String -> List (Html (SiteMsg a b)) -> Html (SiteMsg a b)
tr_ th td = H.tr [] [text_ H.th th, H.td [] td]
