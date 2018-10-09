module Database.Base exposing 
  ( Alignment(..), showAlignment
  , Attribute(..), showAttribute
  , Card(..), showCard
  , Class(..), showClass, enumClass
  , Stat, showStat, addStats
  , Trait(..), enumTrait, showTrait
  , Icon(..), showIcon
  , Material(..), showMaterial, OrdMaterial, ordMaterial, ignoreMat
  , pairWith
  )

import Printing exposing (..)

-- Note: 'No alignment', as in the case of Nursery Rhyme, is an empty array
-- rather than a separate alignment.
type Alignment
    = Lawful | Neutral | Chaotic | Good | Balanced | Evil
    | Mad | Summer | Bride

showAlignment : Alignment -> String
showAlignment = Debug.toString

-- | Determines the "attribute modifier" in `Database.Calculator`.
-- | Currently used only for filters.
-- Note: There is a fifth category in the game, "Beast",
-- which is not represented here because only enemies can have it.
type Attribute = Mankind | Earth | Heaven | Star

showAttribute : Attribute -> String
showAttribute = Debug.toString

-- | The building blocks of a Servant's Deck and NP type.
-- Note: "Extra" cards, also known as EX, are not represented as they only
-- occur at the end of a Brave Combo.
type Card = Quick | Arts | Buster

showCard : Card -> String
showCard = Debug.toString

-- | Determines the "class attack bonus" and "triangle modifier" in
-- | in `Database.Calculator`.
-- Note: MoonCancer and AlterEgo are not represented as they are JP-only.
type Class
    = Saber | Archer | Lancer | Caster | Rider | Assassin | Berserker
    | Shielder | Ruler | Avenger

enumClass : List Class
enumClass =
    [ Saber,  Archer,  Lancer,  Caster,  Rider,  Assassin,  Berserker
    , Shielder,  Ruler,  Avenger
    ]

showClass : Class -> String
showClass = Debug.toString

-- | Craft Essences have minimum and maximum Stats.
-- | Servants have minimum, maximum, and grail Stats.
type alias Stat = { atk : Int, hp : Int }

showStat : Stat -> String
showStat {atk, hp} = 
    "ATK: " ++ String.fromInt atk ++ ", HP: " ++ String.fromInt hp

-- | Used for Fou +ATK and +DEF bonuses.
addStats : Stat -> Stat -> Stat
addStats x y = { atk = x.atk + y.atk, hp = x.hp + y.hp }

-- | Traits are miscellaneous flags attached to Servants. Since many traits are
-- | unlabeled, it can be difficult to figure out which are JP-only and which
-- | translations are accurate to EN.
type Trait
    = Arthur
    | Beast
    | Brynhild
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
    | Mecha
    | Nonbinary
    | PseudoServant
    | Riding
    | Roman
    | Saberface
    | ThreatToHumanity
    | Undead
    | EnumaElish

enumTrait : List Trait
enumTrait = 
    [ Arthur
    , Beast
    , Brynhild
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
    , King
    , Male
    , Mecha
    , Nonbinary
    , PseudoServant
    , Riding
    , Roman
    , Saberface
    , ThreatToHumanity
    , Undead
    , EnumaElish
    ]


showTrait : Trait -> String
showTrait a = case a of
  Brynhild         -> "Brynhild's Beloved"
  DemiServant      -> "Demi-servant"
  HeavenOrEarth    -> "Heaven or Earth"
  GreekMythMale    -> "Greek Mythology Males"
  PseudoServant    -> "Pseudo-Servant"
  ThreatToHumanity -> "Threat to Humanity"
  EnumaElish       -> "Weak to Enuma Elish"
  _                -> unCamel <| Debug.toString a

-- | Corresponding images are found in the [img/Skill](../../img/Skill/) folder.
-- | Every Icon is the name of an image file prefixed with 'Icon'. For example,
-- | `IconAllUp` corresponds to `AllUp.png`.
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
    | IconSun
    | IconSunUp
    | IconSwordUp
    | IconSwordShieldUp
    | IconCrosshairUp
    | IconTeeth
    | IconYinYang

showIcon : Icon -> String
showIcon = Debug.toString >> String.dropLeft 4

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
    | ProofOfHero
    | ScarabOfWisdom
    | SeedOfYggdrasil
    | SerpentJewel
    | ShellOfReminiscence
    | SpiritRoot
    | TearstoneOfBlood
    | VoidsDust
    | WarhorsesYoungHorn

showMaterial : Material -> String
showMaterial a = case a of
  Piece c             -> showClass c ++ " Piece"
  Monument c          -> showClass c ++ " Monument"
  GemOf c             -> "Gem of " ++ showClass c
  MagicGemOf c        -> "Magic Gem of " ++ showClass c
  SecretGemOf c       -> "Secret Gem of " ++ showClass c
  DragonsReverseScale -> "Dragon's Reverse Scale"
  FoolsChain          -> "Fool's Chain"
  LampOfEvilSealing   -> "Lamp of Evil-Sealing"
  VoidsDust           -> "Void's Dust"
  WarhorsesYoungHorn  -> "Warhorse's Young Horn"
  _                   -> unCamel <| Debug.toString a

type alias OrdMaterial = String

ordMaterial : Material -> OrdMaterial
ordMaterial a = case a of
  CrystallizedLore -> "0"
  Piece c          -> "1" ++ showClass c
  Monument c       -> "2" ++ showClass c
  GemOf c          -> "3" ++ showClass c
  MagicGemOf c     -> "4" ++ showClass c
  SecretGemOf c    -> "5" ++ showClass c
  QP               -> "6"
  _                -> Debug.toString a

-- | Blacklisted `Material`s
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

-- | Tuple builder from a constructor
pairWith : a -> (b -> c) -> List b -> List (c, a)
pairWith val construc = List.map <| construc >> \x -> (x, val)
