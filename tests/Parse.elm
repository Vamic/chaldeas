module Parse exposing (..)

import Database.Base         exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)
import Database.Skill        exposing (..)
import MaybeRank             exposing (..)
import Printing              exposing (..)

import Class.Show as Show

upgradeNP : List String
upgradeNP =
    [ "Orion"
    , "Asterios"
    , "Sasaki Kojirou"
    , "Arjuna"
    , "Paracelsus von Hohenheim"
    , "Nikola Tesla"
    , "Mordred"
    , "Fergus mac Roich"
    ]

uniqueNP : List String
uniqueNP = 
    [ "Mash Kyrielight" 
    , "Frankenstein"
    , "EMIYA"
    ]

upgradeSkill : List (String, String)
upgradeSkill =
    [ ("Tamamo-no-Mae", "Fox's Wedding") ]

uniqueSkill : List (String, String)
uniqueSkill =
    [ ("Brynhild", "Primordial Rune")
    , ("Scathach", "Primordial Rune")
    , ("Euryale",  "Whim of the Goddess")
    , ("Stheno",   "Whim of the Goddess")
    , ("Henry Jekyll & Hyde", "Monstrous Strength")
    ]

npRank : Servant -> MaybeRank
npRank s =
  if List.member s.name upgradeNP then
    Upgrade s.phantasm.rank
  else if List.member s.name uniqueNP then
    Unique s s.phantasm.rank
  else
    Pure s.phantasm.rank

ceNames : List String
ceNames = List.map .name craftEssences

skillRank : Servant -> Skill -> (Skill, MaybeRank)
skillRank s x =
  let
    flagged = List.member (s.name, x.name)
  in
    if List.member x.name ceNames then
      ({ x | name = x.name ++ " (Skill)" }, Pure x.rank)
    else if flagged upgradeSkill then
      (x, Upgrade x.rank)
    else if flagged uniqueSkill then
      (x, Unique s x.rank)
    else
      (x, Pure x.rank)

translate : String -> String
translate a = case a of
  "Cheerful-Type Mystic Code" -> "Cheerful Model Mystic Code"
  "Mugashiki—Shinkuu Myou" -> "Mugashiki - Shinkuu Myōu"
  "Leonardo da Vinci" -> "Leonardo Da Vinci"
  "Beautiful Princess (Sea)" -> "Princess of Loveliness (Ocean)"
  "Treasure Hunt (Sea)" -> "Treasure Hunt (Ocean)"
  "Jack-o'-lantern" -> "Jack-o'-Lantern"
  _ -> prettify a

printIcon : Icon -> String
printIcon a = case a of
  IconBusterArtsUp -> "ArtsBuster Up"
  IconSwordUp -> "Attack Up"
  IconSwordShieldUp -> "Dual Up"
  IconRainbow -> "Bond Up"
  IconBusterUp -> "Buster Up"
  IconQuickBusterUp -> "BusterQuick Up"
  IconStarHaloUp -> "CStar Drop Up"
  IconStarUp -> "CStar Gather Up"
  IconStarTurn -> "CStar Turn"
  IconHeart -> "Charm"
  IconExclamationUp -> "Crit Up"
  IconFire -> "Death Res Up"
  IconReaperUp -> "Death Up"
  IconHoodUp -> "Debuff Res Up"
  IconStaffUp -> "Debuff Up"
  IconShieldUp -> "Defense Up"
  IconDodge -> "Evade"
  IconKneel -> "Guts"
  IconShieldBreak -> "Invul Pierce"
  IconShield -> "Invul"
  IconRoad -> "Journey"
  IconHPUp -> "Max HP Up"
  IconNoble -> "NP Charge"
  IconNobleUp -> "NP Generation"
  IconNobleRedUp -> "NP Generation2"
  IconNobleTurn -> "NP Turn"
  IconBeamUp -> "NP Up"
  IconQuartz -> "QP Up"
  IconArtsQuickUp -> "QuickArts Up"
  IconCircuits -> "Skill Seal"
  IconCrosshairUp -> "Taunt"
  _ -> unCamel <| Show.icon a

effects : List SkillEffect -> List String
effects = 
  let
    showEffect a = case a of
      Debuff _ _ (ApplyTrait _) _ -> "Apply Trait"
      To _ (DamageVs _) _ -> "Extra Damage"
      To _ DamagePoison _ -> "Extra Damage"
      Grant _ _ (AttackVs _) _ -> "Special Attack"
      Grant _ _ (ClassAffinity _) _ -> "Special Attack"
      Grant _ _ (DefenseVs _) _ -> "Special Defense"
      Grant _ _ (StarAffinity _) _ -> "Special Stars"
      Grant _ _ (Success _) _ -> Debug.toString DebuffSuccess
      Grant _ _ (Resist _) _ -> Debug.toString DebuffResist
      To _ ef _ -> Debug.toString ef
      Grant _ _ ef _ -> Debug.toString ef
      Debuff _ _ ef _ -> Debug.toString ef
      Bonus ef _ _ -> Debug.toString ef
      Chances _ _ ef -> showEffect ef
      Chance _ ef -> showEffect ef
      Times _ ef -> showEffect ef
      When _ ef -> showEffect ef
      ToMax _ ef -> showEffect ef
  in
    List.map showEffect

synonym : List (List String)
synonym = 
    [ ["special","against"]
    , ["increase", "increasing"]
    , ["reduce", "decrease", "down"]
    , ["attack", "atk"]
    , ["defense", "def"]
    , ["prevent", "seal", "lock"]
    , ["per", "each", "every"]
    , ["additional", "extra"]
    , ["effectiveness", "performance"]
    , ["maximum", "max"]
    , ["np", "phantasm"]
    , ["self", "yourself"]
    , ["restore", "recover", "regenerate", "restore's"]
    , ["invincible", "invincibility"]
    , ["rate", "chance"]
    , ["resist", "resistance"]
    , ["immune", "immunity"]
    ]

readEffect : String -> List String
readEffect _ = ["TODO"]
