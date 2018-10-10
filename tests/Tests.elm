module Tests exposing (..)

import Dict exposing (Dict)

import Database exposing (..)

import Http

pageTitles : List String
pageTitles = [ "Gilgamesh", "Emiya (Assassin)" ]

type alias Model = 
    { pages    : Dict String String
    , missing  : List String
    , progress : Int
    }

init : Model
init = 
    { pages    = Dict.empty
    , missing  = Dict.empty
    , progress = 0
    }

type Msg = Click | NewBook (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Click ->
      ( model, getWarAndPeace )

    NewBook (Ok book) ->
      ...

    NewBook (Err _) ->
      ...

getWarAndPeace : Cmd Msg
getWarAndPeace =
  Http.send NewBook <|
    Http.getString "https://example.com/books/war-and-peace.md"
