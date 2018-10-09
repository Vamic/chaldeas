module Site.ToImage exposing (..)

import Html            as H exposing (Html)
import Html.Events     as E
import Html.Attributes as P

import MyServant             exposing (..)
import Printing              exposing (..)
import Database.Base         exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)
import Database.Skill        exposing (..)

type alias ImagePath = 
    { dir  : String
    , file : String
    }

type alias ToImage a = a -> ImagePath

imageEl : ImagePath -> Html b
imageEl {dir, file} =
    H.img
    [ P.src <| "/chaldeas/img/" ++ dir ++ "/" ++ fileName file ++ ".png" 
    , P.title <| unCamel file
    ]
    []

thumbnailEl : ImagePath -> Html b
thumbnailEl {dir, file} =
    H.img
    [ P.src <| "/chaldeas/img/" ++ dir ++ "/" ++ fileName file ++ " Thumbnail.png" 
    , P.title <| unCamel file
    ]
    []

imageLink : b -> ImagePath -> Html b
imageLink link {dir, file} =
    H.img
    [ P.src <| "/chaldeas/img/" ++ dir ++ "/" ++ fileName file ++ ".png" 
    , P.title <| unCamel file
    , P.class "link"
    , E.onClick link
    ]
    []

toImageCard : ToImage Card
toImageCard = ImagePath "Card" << Debug.toString

toImageClass : ToImage Class
toImageClass = ImagePath "Class" << Debug.toString

toImageServant : ToImage Servant
toImageServant = ImagePath "Servant" << .name

toImageMyServant : ToImage MyServant
toImageMyServant = toImageServant << .base

toImageCraftEssence : ToImage CraftEssence
toImageCraftEssence = ImagePath "CraftEssence" << .name

toImageIcon : ToImage Icon
toImageIcon = ImagePath "Skill" << showIcon

toImageMaterial : ToImage Material
toImageMaterial = ImagePath "Material" << showMaterial

toImageBuffEffect : ToImage BuffEffect
toImageBuffEffect a = case a of
  AlignAffinity _ -> toImageBuffEffect AttackUp
  AttackVs _      -> toImageBuffEffect AttackUp
  ClassAffinity _ -> toImageBuffEffect AttackUp
  DefenseVs _     -> toImageBuffEffect DefenseUp
  Resist _        -> toImageBuffEffect DebuffResist
  StarAffinity _  -> toImageBuffEffect StarUp
  Success _       -> toImageBuffEffect DebuffSuccess
  _               -> ImagePath "Effect" <| Debug.toString a

toImageDebuffEffect : ToImage DebuffEffect
toImageDebuffEffect a = case a of
  ApplyTrait _ -> ImagePath "Effect" "ApplyTrait"
  _            -> ImagePath "Effect" <| Debug.toString a
