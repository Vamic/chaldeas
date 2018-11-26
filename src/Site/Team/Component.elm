module Site.Team.Component exposing (Model, Msg, component)

import Browser.Navigation as Navigation

import Html            as H exposing (Html)
import Html.Attributes as P

import StandardLibrary       exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)
import Persist.Flags         exposing (..)
import Persist.Preferences   exposing (..)
import Site.Algebra          exposing (..)
import Site.Common           exposing (..)
import Site.Rendering        exposing (..)
import Site.Update           exposing (..)

import Class.ToImage as ToImage

type alias Model = SiteModel Team Team ()

type alias Msg = SiteMsg Team Team

component : (String -> Value -> Cmd Msg) -> Component Model Msg
component store =
  let
    init : Flags -> Navigation.Key -> Model
    init flags navKey = 
        siteInit (always []) flags navKey () |> \st -> 
        { st 
        | root    = "Teams"
        , listing = flip List.map flags.teams <| \x -> (x.name, x)
        }

    view : Model -> Html Msg
    view st =
      let
        nav =
            [ a_ ["Craft Essences"]
            , a_ ["Servants"]
            , a_ ["My Servants"]
            , text_ H.strong "Teams"
            ]
      in
        H.div [P.id "elm", P.class <| mode st.prefs] << (++)
        [ H.a [P.id "cover", P.href <| "/Teams"] []
        , H.article [P.id "focus"] []
        ] <<
        siteView st [] nav <<
        H.section [P.id "content"] <<
        List.map teamEl <|
        st.listing ++ [("", emptyTeam)]

    update : Msg -> Model -> (Model, Cmd Msg)
    update = siteUpdate store identity (always "") identity
  in
    { init = init, view = view, update = update }

emptyTeam : Team
emptyTeam = Team "" <| List.repeat 6 (Nothing, Nothing)

teamEl : (String, Team) -> Html msg
teamEl (name, {members}) =
    H.div [P.class "team"] <| text_ H.header name :: List.map memberEl members

memberEl : (Maybe Servant, Maybe CraftEssence) -> Html msg
memberEl (s, ce) =
    H.div [] 
    [ Maybe.withDefault (H.img [] []) <| 
      Maybe.map (ToImage.image << ToImage.servant) s
    , Maybe.withDefault (H.img [] []) <| 
      Maybe.map (ToImage.image << ToImage.craftEssence) ce
    ]
