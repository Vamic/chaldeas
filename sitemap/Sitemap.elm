import Browser

import Html as H exposing (Html)

import StandardLibrary       exposing (..)
import Database              exposing (..)
import Database.CraftEssence exposing (..)
import Printing              exposing (..)

main =
    Browser.element
      { init          = init
      , update        = update
      , subscriptions = always Sub.none
      , view          = view
      }

type alias Model = ()

init : () -> (Model, Cmd Msg)
init _ = pure ()

type alias Msg = ()

update : Msg -> Model -> (Model, Cmd Msg)
update _ st = pure st

view : Model -> Html Msg
view st = 
    H.div [] << List.concatMap (flip (::) <| [H.br [] []]) << List.map H.text << 
    List.sort <<
    List.map ((++) "https://jnbooth.github.io/chaldeas/") <|
    ["Servants", "CraftEssences"]
    ++ List.map (.name >> urlName >> ((++) "Servants/")) servants
    ++ List.map (.name >> urlName >> ((++) "CraftEssences/")) craftEssences
