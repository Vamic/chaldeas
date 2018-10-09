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

scrollToTop : String -> Cmd (SiteMsg a b c)
scrollToTop id = Task.attempt (always DoNothing) <| Dom.setViewportOf id 0 0

setPath : Navigation.Key -> List String -> Cmd msg
setPath key path = 
    Navigation.pushUrl key <| Url.absolute ("chaldeas" :: path) []

setFocus : Navigation.Key -> String -> Maybe String -> Cmd (SiteMsg a b c)
setFocus key root a = case a of
  Nothing   -> setPath key [root]
  Just name -> Cmd.batch [scrollToTop "focus", setPath key [root, urlName name]]

text : String -> List (Html (SiteMsg a b c))
text a = [H.text a]

toCell : Bool -> Float -> Html (SiteMsg a b c)
toCell isPercent =
    places 0 -- TODO
    >> doIf isPercent (flip (++) "%")
    >> text
    >> H.td []

lvlRow : RangeInfo -> Html (SiteMsg a b c)
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
    classNames = List.map showClass enumClass
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
    flip H.p [H.text <| showSkillEffect ef] <|
    if demerit ef then
      [P.class "demerit"]
    else case getEffects |> Maybe.andThen (skillFilter ef) of
      Nothing     -> []
      Just filter -> [P.class "link", E.onClick <| FilterBy [filter]]

a_ : String -> SiteMsg a b c -> Html (SiteMsg a b c)
a_ label click = H.a [E.onClick click] [H.text label]

h_ : Int -> String -> Html (SiteMsg a b c)
h_ level = (\h -> h [] << text) <| case level of
    1 -> H.h1
    2 -> H.h2
    3 -> H.h3
    4 -> H.h4
    5 -> H.h5
    _ -> H.h6 

button_ : String -> Bool -> SiteMsg a b c -> Html (SiteMsg a b c)
button_ label enable click =
  let
    meta = if enable then [E.onClick click] else [P.disabled True]
  in
    H.button meta [H.text label]

checkbox_ : Maybe (Html (SiteMsg a b c)) -> String -> Bool 
         -> List (Html (SiteMsg a b c))
checkbox_ icon label checked =
    [ H.input [P.type_ "checkbox", P.checked checked] []
    , H.label [] <| case icon of
        Nothing -> [H.text label]
        Just ic -> [ic, H.text label]
    ]

onChange : (String -> msg) -> H.Attribute msg
onChange tagger =
  E.stopPropagationOn "change" <| 
  Json.map (\x -> (x, True)) (Json.map tagger E.targetValue)

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

radio_ : String -> Bool -> List (Html (SiteMsg a b c))
radio_ label checked =
    [ H.input [P.type_ "radio", P.checked checked] [] 
    , text_ H.label label
    ]

table_ : List String -> List (Html (SiteMsg a b c)) -> Html (SiteMsg a b c)
table_ headings tbody = 
    H.table []
    [ H.colgroup [] <| List.map (always <| H.col [] []) headings
    , H.thead [] [H.tr [] <| List.map (text_ H.th) headings]
    , H.tbody [] tbody
    ]

text_ : (List p -> List (Html (SiteMsg a b c)) -> Html (SiteMsg a b c)) 
     -> String -> Html (SiteMsg a b c)
text_ el = el [] << text

tr_ : String -> List (Html (SiteMsg a b c)) -> Html (SiteMsg a b c)
tr_ th td = H.tr [] [text_ H.th th, H.td [] td]
