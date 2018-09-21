module Site.ToImage where

import StandardLibrary
import Generic                 as G
import Halogen.HTML.Events     as E
import Halogen.HTML            as H
import Halogen.HTML.Properties as P
import Data.String             as String

import Halogen.HTML (HTML, ClassName(..))

import Database
import MyServant
import Printing

data ImagePath = ImagePath String String

toImage :: ∀ t a b. ToImage t => t -> HTML a b
toImage x =
    H.img
    [ P.src $ "img/" <> dir <> "/" <> fileName file <> ".png"
    , P.title $ unCamel file
    ]
    where
      ImagePath dir file = toImagePath x

toImageLink :: ∀ t a b. ToImage t => (Unit -> b Unit) -> t -> HTML a (b Unit)
toImageLink link x =
    H.img
    [ P.src $ "img/" <> dir <> "/" <> fileName file <> ".png"
    , P.title $ unCamel file
    , P.class_ $ ClassName "link"
    , E.onClick $ E.input_ link
    ]
  where
    ImagePath dir file = toImagePath x

toThumbnail :: ∀ t a b. ToImage t => t -> HTML a b
toThumbnail x =
    H.img
    [ P.src $ "img/" <> dir <> "/" <> fileName file <> " Thumbnail.png"
    , P.title $ unCamel file
    ]
    where
      ImagePath dir file = toImagePath x

class ToImage a where
    toImagePath :: a -> ImagePath

instance _0_ :: ToImage ImagePath where
    toImagePath = identity

instance _1_ :: ToImage Card where
    toImagePath = ImagePath "Card" <<< G.genericShow

instance _2_ :: ToImage Class where
    toImagePath = ImagePath "Class" <<< G.genericShow

instance _3_ :: ToImage Servant where
    toImagePath = ImagePath "Servant" <<< show

instance _4_ :: ToImage MyServant where
    toImagePath = toImagePath <<< getBase

instance _5_ :: ToImage CraftEssence where
    toImagePath = ImagePath "CraftEssence" <<< show

instance _6_ :: ToImage Icon where
    toImagePath = ImagePath "Skill" <<< String.drop 4 <<< G.genericShow

instance _7_ :: ToImage Material where
    toImagePath = ImagePath "Material" <<< show

instance _10_ :: ToImage BuffEffect where
    toImagePath (AlignAffinity _) = toImagePath AttackUp
    toImagePath (AttackVs _)      = toImagePath AttackUp
    toImagePath (ClassAffinity _) = toImagePath AttackUp
    toImagePath (DefenseVs _)     = toImagePath DefenseUp
    toImagePath (Resist _)        = toImagePath DebuffResist
    toImagePath (StarAffinity _)  = toImagePath StarUp
    toImagePath (Success _)       = toImagePath DebuffSuccess
    toImagePath x                 = ImagePath "Effect" $ G.genericShow x

instance _11_ :: ToImage DebuffEffect where
    toImagePath (ApplyTrait _) = ImagePath "Effect" "ApplyTrait"
    toImagePath x              = ImagePath "Effect" $ G.genericShow x
