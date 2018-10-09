module Site.Application exposing (app)

import List.Extra         as List
import Browser.Navigation as Navigation

import Browser exposing (Document, UrlRequest)
import Html exposing (Html)
import Url exposing (Url)

import StandardLibrary       exposing (..)
import Database.CraftEssence exposing (..)
import MyServant             exposing (..)
import Printing              exposing (..)
import Site.Algebra          exposing (..)
import Site.Common           exposing (..)

import Site.CraftEssence.Component as CraftEssences
import Site.Servant.Component      as Servants

type Viewing = CraftEssences | Servants

type alias Model =
    { sModel      : Servants.Model
    , ceModel     : CraftEssences.Model
    , viewing     : Viewing
    }

type Msg
    = RequestUrl UrlRequest
    | ChangeUrl  Url
    | ServantsMsg      Servants.Msg
    | CraftEssencesMsg CraftEssences.Msg

focusFromPath : String -> (b -> String) -> SiteModel a b c -> SiteModel a b c
focusFromPath path show st =
    { st 
    | focus = List.find (Tuple.second >> show >> urlName >> (==) path) 
              st.listing 
              |> Maybe.map Tuple.second
    }

app sStore ceStore = 
  let
    sChild  = Servants.component sStore
    ceChild = CraftEssences.component ceStore

    init : Value -> Url -> Navigation.Key -> (Model, Cmd Msg)
    init flags url key = 
      let
        viewing = 
          if String.contains "CraftEssences" url.path then
            CraftEssences
          else
            Servants
        path = 
            String.split "/" url.path
            |> List.reverse
            >> List.head
            >> Maybe.withDefault ""
        mineOnly = String.startsWith "MyServants" url.path
        ceModel  = focusFromPath path .name <| ceChild.init flags key
        sModel   = focusFromPath path (.base >> .name) <| sChild.init flags key
        {extra}  = sModel
      in
        pure 
          { sModel  = { sModel | extra = { extra | mineOnly = mineOnly } }
          , ceModel = ceModel
          , viewing = viewing
          }
    
    view : Model -> Document Msg
    view st = 
        Document "CHALDEAS" <| case st.viewing of
          CraftEssences -> 
            [ Html.map CraftEssencesMsg (ceChild.view st.ceModel) ]
          Servants -> 
            [ Html.map ServantsMsg (sChild.view st.sModel) ]
    
    update : Msg -> Model -> (Model, Cmd Msg)
    update parentMsg st = case parentMsg of
      RequestUrl url -> pure st
      ChangeUrl url -> pure st
      CraftEssencesMsg msg -> case msg of
        Switch toServant -> 
          let
            {sModel} = st
            focus = Maybe.map (owned sModel.team) toServant
          in
            ( { st 
              | viewing = Servants
              , sModel  = { sModel | focus = focus } 
              }
            , setPath sModel.navKey [sModel.root]
            )
        _ -> 
          let
            (model, cmd) = ceChild.update msg st.ceModel
          in
            ({ st | ceModel = model }, Cmd.map CraftEssencesMsg cmd)
      ServantsMsg msg -> case msg of
        Switch toCraftEssence -> 
          let
            {ceModel} = st
          in
            ( { st 
              | viewing = CraftEssences
              , ceModel = { ceModel | focus = toCraftEssence } 
              }
            , setPath ceModel.navKey [ceModel.root]
            )
        _ -> 
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
