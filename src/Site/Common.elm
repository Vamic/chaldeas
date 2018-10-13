module Site.Common exposing (..)

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

scrollToTop : String -> Cmd (SiteMsg a b c)
scrollToTop id = Task.attempt (always DoNothing) <| Dom.setViewportOf id 0 0

setPath : (String -> Cmd msg) -> Navigation.Key -> List String -> Cmd msg
setPath analytics key path = 
  let
    url = Url.absolute ("chaldeas" :: path) []
  in
    Cmd.batch
      [ Navigation.pushUrl key url
      , analytics url
      ]

setFocus : (String -> Cmd (SiteMsg a b c)) 
        -> Navigation.Key -> String -> Maybe String -> Cmd (SiteMsg a b c)
setFocus analytics key root a = case a of
  Nothing   -> setPath analytics key [root]
  Just name -> 
      Cmd.batch 
      [ scrollToTop "focus"
      , setPath analytics key [root, urlName name]
      ]

toCell : Bool -> Float -> Html msg
toCell isPercent =
    places 0 -- TODO
    >> doIf isPercent (flip (++) "%")
    >> text_ H.td

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
          x ++ "(" ++ y
        else x ++ case String.split " " <| stripSuffix ")" y of
          []      -> "(" ++ replaceSpaces y
          w :: ws -> 
            if not <| List.member w classNames then 
              "(" ++ replaceSpaces y
            else if List.isEmpty ws then 
              ""
            else
              "(" ++ replaceSpaces (String.join " " ws) ++ ")"
      ws   -> String.join "(" ws
  in
    String.split "("
    >> unBreak
    >> replacePirates
    >> doIf shouldPrettify prettify

mode : Preferences -> String
mode prefs = if prefer prefs NightMode then "dark" else "light"

effectEl : Maybe (a -> List SkillEffect) -> SkillEffect -> Html (SiteMsg a b c)
effectEl getEffects ef = 
    flip H.p [H.text <| Show.skillEffect ef] <|
    if demerit ef then
      [P.class "demerit"]
    else case getEffects |> Maybe.andThen (skillFilter ef) of
      Nothing     -> []
      Just filter -> [P.class "link", E.onClick <| FilterBy [filter]]

a_ : String -> msg -> Html msg
a_ label click = H.a [E.onClick click] [H.text label]

h_ : Int -> String -> Html msg
h_ level = text_ <| case level of
    1 -> H.h1
    2 -> H.h2
    3 -> H.h3
    4 -> H.h4
    5 -> H.h5
    _ -> H.h6 

button_ : String -> Bool -> msg -> Html msg
button_ label enable click =
  let
    meta = 
        P.type_ "button" :: 
        if enable then [E.onClick click] else [P.disabled True]
  in
    H.button meta [H.text label]

checkbox_ : Maybe (Html msg) -> String -> Bool -> List (Html msg)
checkbox_ icon label checked =
    [ H.input [P.type_ "checkbox", P.checked checked] []
    , H.label [] <| case icon of
        Nothing -> [H.text label]
        Just ic -> [ic, H.text label]
    ]

onChange : (String -> msg) -> H.Attribute msg
onChange tagger =
  E.stopPropagationOn "change" <<
  Json.map (\x -> (x, True)) <| Json.map tagger E.targetValue

int_ : Int -> Int -> Int -> (Int -> SiteMsg a b c) 
    -> List (Html (SiteMsg a b c))
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
    ]

radio_ : String -> Bool -> List (Html msg)
radio_ label checked =
    [ H.input [P.type_ "radio", P.checked checked] [] 
    , text_ H.label label
    ]

table_ : List String -> List (Html msg) -> Html msg
table_ headings tbody = 
    H.table []
    [ H.colgroup [] <| List.map (always <| H.col [] []) headings
    , H.thead [] [H.tr [] <| List.map (text_ H.th) headings]
    , H.tbody [] tbody
    ]

text_ : (List p -> List (Html msg) -> Html msg) -> String -> Html msg
text_ el txt = el [] [H.text txt]

tr_ : String -> List (Html (SiteMsg a b c)) -> Html (SiteMsg a b c)
tr_ th td = H.tr [] [text_ H.th th, H.td [] td]
