module Database.Base exposing
  ( Alignment(..), enumAlignment
  , Attribute(..)
  , Card(..)
  , Class(..), enumClass
  , Stat, addStats
  , Trait(..), enumTrait
  , Icon(..)
  , Material(..), OrdMaterial, enumMaterial, ordMaterial, ignoreMat
  , pairWith
  )

{-| Basic Enum datatypes to represent game information. -}

import StandardLibrary exposing (..)

-- Note: 'No alignment', as in the case of Nursery Rhyme, is an empty array
-- rather than a separate alignment.
type Alignment
    = Lawful | Neutral | Chaotic | Good | Balanced | Evil
    | Mad | Summer | Bride

enumAlignment : List Alignment
enumAlignment =
    [ Lawful,  Neutral,  Chaotic,  Good,  Balanced,  Evil
    , Mad,  Summer,  Bride
    ]

{-| Determines the "attribute modifier" in `Database.Calculator`.
Currently used only for filters. -}
-- Note: There is a fifth category in the game, "Beast",
-- which is not represented here because only enemies can have it.
type Attribute = Mankind | Earth | Heaven | Star

{-| The building blocks of a Servant's Deck and NP type. -}
-- Note: "Extra" cards, also known as EX, are not represented as they only
-- occur at the end of a Brave Combo.
type Card = Quick | Arts | Buster

{-| Determines the "class attack bonus" and "triangle modifier"
in `Database.Calculator`.-}
-- Note: MoonCancer and AlterEgo are not represented as they are JP-only.
type Class
    = Saber | Archer | Lancer | Caster | Rider | Assassin | Berserker
    | Shielder | Ruler | Avenger

enumClass : List Class
enumClass =
    [ Saber,  Archer,  Lancer,  Caster,  Rider,  Assassin,  Berserker
    , Shielder,  Ruler,  Avenger
    ]

{-| Craft Essences have minimum and maximum Stats.
Servants have minimum, maximum, and grail Stats. -}
type alias Stat = { atk : Int, hp : Int }

{-| Used for Fou +ATK and +DEF bonuses. -}
addStats : Stat -> Stat -> Stat
addStats x y = { atk = x.atk + y.atk, hp = x.hp + y.hp }

{-| Traits are miscellaneous flags attached to Servants. -}
-- Note:  Since many traits are unlabeled,
-- it can be difficult to figure out which are JP-only and which
-- translations are accurate to EN.
type Trait
    = Arthur
    | Beast
    | BrynhildsBeloved
    | DemiServant
    | Demon
    | Demonic
    | Divine
    | Dragon
    | HeavenOrEarth
    | Female
    | GreekMythMale
    | Humanoid
    | Human
    | King
    | Male
    | Nonbinary
    | PseudoServant
    | Riding
    | Roman
    | Saberface
    | Undead
    | EnumaElish

enumTrait : List Trait
enumTrait =
    [ Arthur
    , Beast
    , BrynhildsBeloved
    , DemiServant
    , Demon
    , Demonic
    , Divine
    , Dragon
    , HeavenOrEarth
    , Female
    , GreekMythMale
    , Humanoid
    , Human
    , Male
    , Nonbinary
    , PseudoServant
    , Riding
    , King
    , Roman
    , Saberface
    , Undead
    , EnumaElish
    ]

{-| Corresponding images are found in the [img/Skill](../../img/Skill/) folder.
Every Icon is the name of an image file prefixed with 'Icon'. For example,
`IconAllUp` corresponds to `AllUp.png`. -}
type Icon
    = IconAllUp
    | IconArtsQuickUp
    | IconArtsUp
    | IconAvenger
    | IconBeamUp
    | IconBeamDown
    | IconBubbles
    | IconBullseye
    | IconBusterArtsUp
    | IconBusterUp
    | IconCircuits
    | IconClockUp
    | IconCrosshairUp
    | IconDamageUp
    | IconDarkMagic
    | IconDash
    | IconDiamonds
    | IconDodge
    | IconEclipse
    | IconExclamationUp
    | IconExclamationDown
    | IconFace
    | IconFire
    | IconFlex
    | IconGoddess
    | IconHeal
    | IconHealTurn
    | IconHealUp
    | IconHeart
    | IconHoodUp
    | IconHoodDown
    | IconHoodX
    | IconHorse
    | IconHPUp
    | IconKneel
    | IconMagicCircle
    | IconMask
    | IconMissing
    | IconMystic
    | IconNiffin
    | IconNoble
    | IconNobleRedUp
    | IconNobleTurn
    | IconNobleUp
    | IconPotion
    | IconQuartz
    | IconQuickBusterUp
    | IconQuickUp
    | IconRainbow
    | IconReaperUp
    | IconRoad
    | IconShield
    | IconShieldBreak
    | IconShieldDown
    | IconShieldUp
    | IconSpotlight
    | IconStaffUp
    | IconStar
    | IconStarHaloUp
    | IconStarTurn
    | IconStarUp
    | IconStun
    | IconSwordDown
    | IconSwordUp
    | IconSwordShieldUp
    | IconSwords
    | IconSun
    | IconSunUp
    | IconTeeth
    | IconYinYang

{-| Items for Ascension and Skill Reinforcement. -}
type Material
    = CrystallizedLore
    | Piece Class
    | Monument Class
    | GemOf Class
    | MagicGemOf Class
    | SecretGemOf Class
    | QP
    | BlackBeastGrease
    | ClawOfChaos
    | CursedBeastGallstone
    | DeadlyPoisonousNeedle
    | DragonFang
    | DragonsReverseScale
    | EternalGear
    | EvilBone
    | FoolsChain
    | ForbiddenPage
    | GhostLantern
    | GreatKnightMedal
    | HeartOfTheForeignGod
    | HomunculusBaby
    | LampOfEvilSealing
    | MeteorHorseshoe
    | OctupletCrystals
    | PhoenixFeather
    | PrimordialLanugo
    | ProofOfHero
    | ScarabOfWisdom
    | SeedOfYggdrasil
    | SerpentJewel
    | ShellOfReminiscence
    | SpiritRoot
    | TearstoneOfBlood
    | VoidsDust
    | WarhorsesYoungHorn

enumMaterial : List Material
enumMaterial =
    [ CrystallizedLore ]
    ++ List.map Piece enumClass
    ++ List.map Monument enumClass
    ++ List.map GemOf enumClass
    ++ List.map MagicGemOf enumClass
    ++ List.map SecretGemOf enumClass ++
    [ QP
    , BlackBeastGrease
    , ClawOfChaos
    , CursedBeastGallstone
    , DragonFang
    , DragonsReverseScale
    , EternalGear
    , EvilBone
    , FoolsChain
    , ForbiddenPage
    , GhostLantern
    , GreatKnightMedal
    , HeartOfTheForeignGod
    , HomunculusBaby
    , LampOfEvilSealing
    , MeteorHorseshoe
    , OctupletCrystals
    , PhoenixFeather
    , PrimordialLanugo
    , ProofOfHero
    , ScarabOfWisdom
    , SeedOfYggdrasil
    , SerpentJewel
    , ShellOfReminiscence
    , SpiritRoot
    , DeadlyPoisonousNeedle
    , TearstoneOfBlood
    , VoidsDust
    , WarhorsesYoungHorn
    ]

type alias OrdMaterial = Int

ordMaterial : Material -> OrdMaterial
ordMaterial = enumToOrd enumMaterial

{-| Blacklisted `Material`s -}
ignoreMat : Material -> Bool
ignoreMat a = case a of
  CrystallizedLore -> True
  Piece _          -> True
  Monument _       -> True
  GemOf _          -> True
  MagicGemOf _     -> True
  SecretGemOf _    -> True
  QP               -> True
  _                -> False

{-| Tuple builder from a constructor -}
pairWith : a -> (b -> c) -> List b -> List (c, a)
pairWith val construc = List.map <| construc >> \x -> (x, val)
