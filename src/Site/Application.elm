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
    { sModel      : Servants.Model
    , ceModel     : CraftEssences.Model
    , viewing     : Viewing
    }

type Msg
    = RequestUrl UrlRequest
    | ChangeUrl  Url
    | ServantsMsg      Servants.Msg
    | CraftEssencesMsg CraftEssences.Msg


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

stateFromPath : String -> Model -> Model
stateFromPath fullPath st =
  let
    viewing = 
      if String.contains "CraftEssences" fullPath then
        CraftEssences
      else
        Servants
    path = 
        String.split "/" fullPath
        |> List.reverse
        >> List.head
        >> Maybe.withDefault ""
    mineOnly = String.contains "MyServants" fullPath
    (ceModel, sModel) = case viewing of
      CraftEssences -> (focusFromPath path .name st.ceModel, st.sModel)
      Servants -> (st.ceModel, focusFromPath path (.base >> .name) st.sModel)
    {extra} = sModel
      in
        { viewing = viewing 
        , ceModel = ceModel
        , sModel  = { sModel | extra = { extra | mineOnly = mineOnly } }
        }

app onInit analytics store = 
  let
    child constr unMsg = constr ((<<) (Cmd.map unMsg) << store)
        
    sChild : Component Servants.Model Servants.Msg
    sChild  = child Servants.component <| \a -> case a of
      ServantsMsg x -> x
      _             -> DoNothing

    ceChild : Component CraftEssences.Model CraftEssences.Msg
    ceChild = child CraftEssences.component <| \a -> case a of
      CraftEssencesMsg x -> x
      _                  -> DoNothing

    init : Value -> Url -> Navigation.Key -> (Model, Cmd Msg)
    init flags url key = 
        ( stateFromPath url.path
          { ceModel = ceChild.init flags key
          , sModel  = sChild.init flags key
          , viewing = Servants
          }
        , onInit
        )
    
    view : Model -> Document Msg
    view st = 
        Document "CHALDEAS" <| case st.viewing of
          CraftEssences -> 
            [ Html.map CraftEssencesMsg (ceChild.view st.ceModel) ]
          Servants -> 
            [ Html.map ServantsMsg (sChild.view st.sModel) ]
    
    update : Msg -> Model -> (Model, Cmd Msg)
    update parentMsg st = case parentMsg of
      RequestUrl urlRequest -> case urlRequest of
        Browser.Internal url  -> pure st
        Browser.External href -> (st, Navigation.load href)
      ChangeUrl {path} -> (stateFromPath path st, analytics path)
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
            , Cmd.batch 
              [ setPath sModel.navKey [sModel.root]
              , Cmd.map ServantsMsg <| scrollToTop "content"
              ]
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
            , Cmd.batch 
              [ setPath ceModel.navKey [ceModel.root]
              , Cmd.map CraftEssencesMsg <| scrollToTop "content"
              ]
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
