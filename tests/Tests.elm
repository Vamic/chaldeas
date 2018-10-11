import Browser
import Dict exposing (Dict)
import Http
import List.Extra  as List
import Maybe.Extra as Maybe

import Html            as H exposing (Html)
import Html.Attributes as P

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)
import Printing              exposing (..)
import Site.Common           exposing (..)

import Wiki exposing (Wiki)
import MaybeRank exposing (MaybeRank(..))

main =
    Browser.element
      { init          = init
      , update        = update
      , subscriptions = always Sub.none
      , view          = view
      }

type Outcome = Success | Failure String

type Test = Test String Outcome | Suite String (List Test)

assert : String -> Bool -> Outcome
assert label succeeded = if succeeded then Success else Failure label

pageTitles : List String
pageTitles = [ "Gilgamesh", "Emiya (Assassin)" ]

total : Int
total = List.length pageTitles

type alias Model = 
    { pages    : Dict String String
    , missing  : List String
    , progress : Int
    , tests    : List Test
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { pages    = Dict.empty
      , missing  = []
      , progress = 0
      , tests    = []
      }
    , requestAll
    )

type Msg = ReceivePage String (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg st = case msg of
  ReceivePage title result ->
    let
      newSt = case result of 
        Ok page -> { st | pages = Dict.insert title page st.pages }
        Err _ ->   { st | missing = title :: st.missing }
      newProgress = st.progress + 1
      tests = if newProgress == total then runTests st.pages else st.tests
    in
      ({ newSt | progress = st.progress + 1, tests = tests }, Cmd.none)

showIf : Bool -> a -> Maybe a
showIf a = if a then Just else always Nothing

view : Model -> Html Msg
view st = 
  let
    size     = Dict.size st.pages
    loaded   = st.progress == total
    progress = String.fromInt size ++ "/" ++ String.fromInt total 
    missing  =
      if List.isEmpty st.missing then
        []
      else
        [ H.li [P.class "failure"]
          [ H.text "Unavailable:"
          , H.ul [] <| List.map (text_ H.li) st.missing
          ]
        ]
  in
    H.ul [] << Maybe.values <|
    [ Just <| 
      H.li [P.class "progress"] 
      [ H.text <| "Loading " ++ progress ]
    , showIf loaded <| 
      H.li 
      [P.class <| if List.isEmpty st.missing then "success" else "failure"]
      [H.text <| "Loaded " ++ progress]
    ]

requestPage : String -> Cmd Msg
requestPage title = 
    "http://grandorder.wiki/index.php?action=raw&title=" ++ title
    |> Http.getString 
    >> Http.send (ReceivePage title)

requestAll : Cmd Msg
requestAll = Cmd.batch <| List.map requestPage pageTitles

runTests : Dict String String -> List Test
runTests pages = 
    List.map (wikiSuite pages testCraftEssence) craftEssences
    ++ List.map (wikiSuite pages testServant) servants

wikiSuite : Dict String String 
     -> ((MaybeRank -> Wiki) -> { a | name : String } -> List Test) 
     -> { a | name : String }
     -> Test
wikiSuite pages test x =
    Suite x.name <| case Dict.get x.name pages of
      Nothing   -> [Failure "Page not found"]
      Just page -> List.filter ((/=) Success) <| test (Wiki.fromString page) x

wikiMatch : Wiki -> String -> String -> Test
wikiMatch wiki k obj = 
  let
    cleanup = List.map <| filterOut "%,{}[]()'"
  in
    Test k <| case Maybe.map cleanup (Wiki.field wiki k) of
      Nothing -> Failure <| "Missing property"
      Just v  -> 
          assert (obj ++ " not in [" ++ String.join ", " v ++ "].") <|
          List.member obj v

shouldMatch : String -> List String -> List String -> Test
shouldMatch label x y = 
  let
    x_ = List.unique x
    y_ = List.unique y
    diffTest xs = case xs of
      [] -> Success
      _  -> Failure <| String.join ", " xs
  in
    Suite label
      [ Test "Missing from Wiki" << diffTest <| List.dif ]

testCraftEssence : (MaybeRank -> Wiki) -> CraftEssence -> List Test
testCraftEssence getWiki ce = 
  let
    wiki = getWiki Unranked
  in
    []
testServant : (MaybeRank -> Wiki) -> Servant -> List test
testServant wiki s = []
