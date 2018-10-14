module Class.ToImage exposing (..)

{-| Links to the `/img` folder.

# Usage
@docs ImagePath, ToImage, image, thumbnail, link
# `Database.Base`
@docs card, class, icon, material
# `Database.CraftEssence`
@docs craftEssence
# `Database.Servant`
@docs servant
# `Database.Skill`
@docs buffEffect, debuffEffect
# `MyServant`
@docs myServant
-}

import Html            as H exposing (Html)
import Html.Events     as E
import Html.Attributes as P

import MyServant             exposing (..)
import Printing              exposing (..)
import Database.Base         exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)
import Database.Skill        exposing (..)

import Class.Show as Show

type alias ImagePath = 
    { dir  : String
    , file : String
    }

type alias ToImage a = a -> ImagePath

image : ImagePath -> Html b
image {dir, file} =
    H.img
    [ P.src <| "/chaldeas/img/" ++ dir ++ "/" ++ fileName file ++ ".png" 
    , P.title <| unCamel file
    ]
    []

thumbnail : ImagePath -> Html b
thumbnail {dir, file} =
    H.img
    [ P.src <| "/chaldeas/img/" ++ dir ++ "/" ++ fileName file ++ " Thumbnail.png" 
    , P.title <| unCamel file
    ]
    []

link : b -> ImagePath -> Html b
link href {dir, file} =
    H.img
    [ P.src <| "/chaldeas/img/" ++ dir ++ "/" ++ fileName file ++ ".png" 
    , P.title <| unCamel file
    , P.class "link"
    , E.onClick href
    ]
    []

card : ToImage Card
card = ImagePath "Card" << Show.card

class : ToImage Class
class = ImagePath "Class" << Show.class

servant : ToImage Servant
servant = ImagePath "Servant" << .name

myServant : ToImage MyServant
myServant = servant << .base

craftEssence : ToImage CraftEssence
craftEssence = ImagePath "CraftEssence" << .name

icon : ToImage Icon
icon = ImagePath "Skill" << Show.icon

material : ToImage Material
material = ImagePath "Material" << Show.material

buffEffect : ToImage BuffEffect
buffEffect a = case a of
  AlignAffinity _ -> buffEffect AttackUp
  AttackVs _      -> buffEffect AttackUp
  ClassAffinity _ -> buffEffect AttackUp
  DefenseVs _     -> buffEffect DefenseUp
  Resist _        -> buffEffect DebuffResist
  StarAffinity _  -> buffEffect StarUp
  Success _       -> buffEffect DebuffSuccess
  _               -> ImagePath "Effect" <| Show.nameBuffEffect a

debuffEffect : ToImage DebuffEffect
debuffEffect a = ImagePath "Effect" <| Show.nameDebuffEffect a
