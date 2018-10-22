module Site.Application exposing (app)

import List.Extra         as List
import Browser.Navigation as Navigation

import Browser exposing (Document, UrlRequest)
import Html exposing (Html)
import Url exposing (Url)

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.CraftEssence exposing (..)
import MyServant             exposing (..)
import Printing              exposing (..)
import Site.Algebra          exposing (..)
import Site.Common           exposing (..)

import Site.CraftEssence.Component as CraftEssences
import Site.Servant.Component      as Servants

{-| The page currently being shown. -}
type Viewing = CraftEssences | Servants

type alias Model =
    { ceModel : CraftEssences.Model
    , sModel  : Servants.Model
    , viewing : Viewing
    }

type Msg
    = RequestUrl UrlRequest
    | ChangeUrl  Url
    | CraftEssencesMsg CraftEssences.Msg
    | ServantsMsg      Servants.Msg


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
              , sModel  = { sModel | extra = { extra | mineOnly = mineOnly } }
              }
            , Maybe.withDefault (doIf mineOnly ((++) "My ") "Servants") <| 
              Maybe.map (.base >> .name) sModel.focus
            )

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
    init flags url key = 
      let
        (st, newTitle) = stateFromPath url.path
          { ceModel = ceChild.init flags key
          , sModel  = sChild.init flags key
          , viewing = Servants
          }
      in
        (st, Cmd.batch [onInit, title newTitle])
    
    view : Model -> Document Msg
    view st = 
        Document "CHALDEAS" <| case st.viewing of
          CraftEssences -> 
            [ Html.map CraftEssencesMsg <| ceChild.view st.ceModel ]
          Servants -> 
            [ Html.map ServantsMsg <| sChild.view st.sModel ]
    
    update : Msg -> Model -> (Model, Cmd Msg)
    update parentMsg st = case parentMsg of
      RequestUrl urlRequest -> case urlRequest of
        Browser.Internal url  -> pure st
        Browser.External href -> (st, Navigation.load href)
      ChangeUrl {path} -> 
        let
          (newSt, newTitle) = stateFromPath path st
        in
          (newSt, Cmd.batch [analytics path, title newTitle])
      CraftEssencesMsg msg -> case msg of
        Switch toServant -> 
          let
            {sModel} = st
            {extra}  = sModel
            focus    = Maybe.map (owned sModel.team) toServant
          in
            ( { st 
              | viewing = Servants
              , sModel  = 
                  { sModel 
                  | focus = focus
                  , extra = { extra | mineOnly = False }
                  } 
              }
            , Cmd.map ServantsMsg << 
              setFocus sModel.navKey "Servants" <| 
              Maybe.map (.base >> .name) focus
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
            , Cmd.map CraftEssencesMsg << 
              setFocus ceModel.navKey "CraftEssences" <| 
              Maybe.map .name toCraftEssence
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
