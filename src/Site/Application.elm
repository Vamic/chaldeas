module Site.Application exposing (app)

import Html            as H exposing (Html)
import Html.Attributes as P

import List.Extra         as List
import Browser.Dom        as Dom
import Browser.Navigation as Navigation
import Json.Decode        as Json

import Browser exposing (Document, UrlRequest)
import Date
import Dict    exposing (Dict)
import Url     exposing (Url)
import Task
import Time

import StandardLibrary     exposing (..)
import Persist.Flags       exposing (..)
import Persist.Preferences exposing (..)
import Printing            exposing (..)
import Site.Algebra        exposing (..)

import Site.CraftEssence.Component as CraftEssences
import Site.Servant.Component      as Servants

{-| The page currently being shown. -}
type Viewing = CraftEssences | Servants | Teams

showViewing : Viewing -> String
showViewing a = case a of
  CraftEssences -> "CraftEssences"
  Servants      -> "Servants"
  Teams         -> "Teams"

type alias Model =
    { error   : Maybe String
    , navKey  : Navigation.Key
    , teams   : List Team
    , onTeam  : Maybe (Team, Int)
    , viewing : Viewing
    , ceModel : CraftEssences.Model
    , sModel  : Servants.Model
    }

type Msg
    = RequestUrl       UrlRequest
    | ChangeUrl        Url
    | CraftEssencesMsg CraftEssences.Msg
    | ServantsMsg      Servants.Msg
    | OnError          (Result Dom.Error ())

printError : Dom.Error -> String
printError a = case a of
  Dom.NotFound id -> "Element #" ++ id ++ " not found!"

{-| If loaded with a url for a particular Servant or Craft Essence,
the corresponding Servant/CE is displayed. -}
focusFromPath : String -> (b -> String) -> SiteModel a b c -> SiteModel a b c
focusFromPath path show st =
  let
    snd   = Tuple.second
    match =
        show
        >> urlName
        >> (==) path
  in case Maybe.map match st.focus of
    Just True -> st
    _ -> { st | focus = List.find (snd >> match) st.listing |> Maybe.map snd }

stateFromPath : String -> Model -> (Model, String)
stateFromPath fullPath st =
  let
    path =
        String.split "/" fullPath
        |> List.reverse
        >> List.head
        >> Maybe.withDefault ""
      in
        if String.contains "CraftEssences" fullPath then
          let
            ceModel = focusFromPath path .name st.ceModel
          in
            ( { st
              | viewing = CraftEssences
              , ceModel = ceModel
              }
            , Maybe.withDefault "Craft Essences" <|
              Maybe.map .name ceModel.focus
            )
        else
          let
            sModel   = focusFromPath path (.base >> .name) st.sModel
            {extra}  = sModel
            mineOnly = String.contains "MyServants" fullPath
          in
            ( { st
              | viewing = Servants
              , sModel  = Servants.setRoot 
                          { sModel | extra = { extra | mineOnly = mineOnly } }
              }
            , Maybe.withDefault (doIf mineOnly ((++) "My ") "Servants") <|
              Maybe.map (.base >> .name) sModel.focus
            )

resetPopup : Cmd Msg
resetPopup = Task.attempt OnError <| Dom.setViewportOf "focus" 0 0

app onInit analytics title store =
  let
    child constr unMsg = constr ((<<) (Cmd.map unMsg) << store)

    ceChild : Component CraftEssences.Model CraftEssences.Msg
    ceChild = child CraftEssences.component <| \a -> case a of
      CraftEssencesMsg x -> x
      _                  -> DoNothing

    sChild : Component Servants.Model Servants.Msg
    sChild = child Servants.component <| \a -> case a of
      ServantsMsg x -> x
      _             -> DoNothing

    init : Value -> Url -> Navigation.Key -> (Model, Cmd Msg)
    init val url key =
      let
        (error, flags) =
          case Json.decodeValue decodeFlags val of
            Ok ok   -> (Nothing, ok)
            Err err ->
              (Just <| Json.errorToString err
              , { today       = 0 |> Time.millisToPosix >> Date.today
                , preferences = noPreferences
                , mine        = Dict.empty
                , teams       = []
                }
              )
        (st, newTitle) = stateFromPath url.path
          { error   = error
          , navKey  = key
          , teams   = flags.teams
          , onTeam  = Nothing
          , viewing = Servants
          , ceModel = ceChild.init flags key
          , sModel  = sChild.init flags key
          }
      in
        (st, Cmd.batch [onInit, title newTitle])

    view : Model -> Document Msg
    view st =
      let
        showError = case st.error of
          Nothing -> identity
          Just err -> (::) <| H.div [P.id "error"] [H.text err]
      in
        Document "CHALDEAS" << showError <| case st.viewing of
          CraftEssences ->
            [ H.map CraftEssencesMsg <| ceChild.view st.ceModel ]
          Servants ->
            [ H.map ServantsMsg <| sChild.view st.sModel ]
          Teams    ->
            []

    update : Msg -> Model -> (Model, Cmd Msg)
    update parentMsg st = case parentMsg of
      OnError (Ok _)        -> pure st
      OnError (Err err)     -> pure { st | error = Just <| printError err }
      RequestUrl urlRequest -> case urlRequest of
        Browser.Internal url  -> 
          if String.contains (showViewing st.viewing) url.path 
          && String.contains "/" (String.dropLeft 1 url.path) then
            pure st
          else
            (st, Navigation.pushUrl st.navKey <| Url.toString url)
        Browser.External href -> 
            (st, Navigation.load href)
      ChangeUrl {path} ->
        let
          (newSt, newTitle) = stateFromPath path st
        in
          (newSt, Cmd.batch [analytics path, title newTitle, resetPopup])
      CraftEssencesMsg msg ->
          let
            (model, cmd) = ceChild.update msg st.ceModel
          in
            ({ st | ceModel = model }, Cmd.map CraftEssencesMsg cmd)
      ServantsMsg msg ->
          let
            (model, cmd) = sChild.update msg st.sModel
          in
            ({ st | sModel = model }, Cmd.map ServantsMsg cmd)
  in
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = always Sub.none
    , onUrlRequest  = RequestUrl
    , onUrlChange   = ChangeUrl
    }
