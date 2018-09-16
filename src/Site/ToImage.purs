module Site.ToImage where

import Prelude
import Generic     as G
import Data.String as String

import Halogen.HTML (HTML)

import Database
import Site.Common

data ImagePath = ImagePath String String

toImage :: ∀ t a b. ToImage t => t -> HTML a b
toImage x = _img $ "img/" <> dir <> "/" <> file <> ".png"
    where
      ImagePath dir file = toImagePath x

toThumbnail :: ∀ t a b. ToImage t => t -> HTML a b
toThumbnail x = _img $ "img/" <> dir <> "/" <> file <> " Thumbnail.png"
    where
      ImagePath dir file = toImagePath x

class ToImage a where
    toImagePath :: a -> ImagePath

instance _0_ :: ToImage Icon where
    toImagePath = ImagePath "Skill" <<< String.drop 4 <<< G.genericShow

instance _1_ :: ToImage Card where
    toImagePath = ImagePath "Card" <<< G.genericShow

instance _2_ :: ToImage Class where
    toImagePath = ImagePath "Class" <<< G.genericShow

instance _3_ :: ToImage Servant where
    toImagePath = ImagePath "Servant" <<< show

instance _4_ :: ToImage CraftEssence where
    toImagePath = ImagePath "Craft Essence" <<< show

instance _10_ :: ToImage BuffEffect where
    toImagePath (AlignAffinity _) = toImagePath AttackUp
    toImagePath (AttackVs _)      = toImagePath AttackUp
    toImagePath (ClassAffinity _) = toImagePath AttackUp
    toImagePath (DefenseVs _)     = toImagePath DefenseUp
    toImagePath (Resist _)        = toImagePath DebuffResist
    toImagePath (StarAffinity _)  = toImagePath StarUp
    toImagePath (Success _)       = toImagePath DebuffSuccess
    toImagePath MentalResist      = toImagePath DebuffResist
    toImagePath MentalSuccess     = toImagePath DebuffSuccess
    toImagePath GutsPercent       = toImagePath Guts
    toImagePath x                 = ImagePath "Effect" $ G.genericShow x

instance _11_ :: ToImage DebuffEffect where
    toImagePath MentalVuln     = toImagePath DebuffVuln
    toImagePath CharmVuln      = toImagePath DebuffVuln
    toImagePath (ApplyTrait _) = ImagePath "Effect" "ApplyTrait"
    toImagePath x              = ImagePath "Effect" $ G.genericShow x
