module Parse exposing (..)

import List.Extra as List

import Database.Base         exposing (..)
import Database.CraftEssence exposing (..)
import Database.Servant      exposing (..)
import Database.Skill        exposing (..)
import MaybeRank             exposing (MaybeRank(..))
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

skillNames : Servant -> List Skill
skillNames s = List.map (skillRank s >> Tuple.first) s.skills

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
      To _ (SpecialDamage _) _ -> "Extra Damage"
      To _ DamagePoison _ -> "Extra Damage"
      Grant _ _ (Special ef _) _ -> "Special " ++ Debug.toString ef
      -- Grant _ _ (Success _) _ -> Debug.toString DebuffSuccess
      -- Grant _ _ (Resist _) _ -> Debug.toString DebuffResist
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

effectMap : List (List String, List String)
effectMap =
  let
    show = List.map Debug.toString
  in
    [ (["formula"], [])
    , (["bonus","increase"], [])
    , (["end","of","turn"], [])
    , (["can","only","be","used"], [])

    , (["increase","card","effectiveness","drop","gather","gain","strength","debuff","rate","resist"], show [Performance Quick, Performance Arts, Performance Buster, StarUp, StarAbsorb, NPGen, NPUp, DebuffSuccess, DebuffResist])
    , (["reduce","attack","defense","critical","chance","debuff","resist","np","strength"], show [AttackDown, DefenseDown, CritChance, DebuffVuln, NPDown])
    , (["reduce","critical","chance","debuff","resist"], show [CritChance, DebuffVuln])
    , (["reduce","defense","critical","chance"], show [DefenseDown, CritChance])
    , (["reduce","attack","defense"], show [AttackDown, DefenseDown])
    , (["increase","gain","critical","strength","drop","recovery","debuff","resist"], show [NPGen, CritUp, HealingReceived, StarUp, DebuffResist])
    , (["increase","attack","defense","star"], show [AttackUp, DefenseUp, StarUp])
    , (["reduce","np","strength","critical","strength"], show [NPDown, CritDown])
    , (["increase","attack","defense"], show [AttackUp, DefenseUp])
    , (["remove","debuffs","restore","hp"], show [RemoveDebuffs, Heal])
    , (["additional","damage","turn"], ["Special AttackUp"])
    , (["decrease","1000hp","yourself"], show [DemeritDamage])
    , (["damage","previous","hp"], show [Avenge])
    , (["hp","fall"], show [DemeritHealth])
    , (["additional","damage","all","enemies"], show [Damage])
    , (["confusion"], show [Confusion])
    , (["current","hp"], show [LastStand])
    , (["extra","own","hp"], show [LastStand])
    , (["against","drop"], ["Special StarUp"])
    , (["apply","trait"], ["Apply Trait"])
    , (["extra","damage"], ["Extra Damage"])
    , (["deal","special","damage"], ["Extra Damage"])
    , (["special","attack"], ["Special AttackUp"])
    , (["special","damage"], ["Special AttackUp"])
    , (["special","defense"], ["Special DefenseUp"])
    , (["transform","hyde"], show [BecomeHyde])
    , (["increase","bond","points"], show [Bond])
    , (["nullify"], show [BuffBlock])
    , (["prevent","buff"], show [BuffBlock])
    , (["decrease","attack","success","yourself"], show [BuffFail])
    , (["decrease","buff","success"], show [BuffFail])
    , (["increase","buff","success"], show [BuffUp])
    , (["burn"], show [Burn])
    , (["increase","charm","resist"], show [Resist Charm])
    , (["charm","resist"], show [CharmVuln])
    , (["charm","success"], show [Success Charm])
    , (["charm"], show [Charm])
    , (["cooldowns"], show [Cooldowns])
    , (["decrease","critical","chance"], show [CritChance])
    , (["increase","critical","damage"], show [CritUp])
    , (["increase","critical","strength"], show [CritUp])
    , (["remove","poison"], show [Cure])
    , (["curse"], show [Curse])
    , (["additional","damage","all","enemies"], show [Damage])
    , (["damage","cut"], show [DamageDown])
    , (["reduce","incoming","damage"], show [DamageDown])
    , (["reduce","damage","taken"], show [DamageDown])
    , (["deal","damage","defense"], show [DamageThruDef])
    , (["deal","damage","def-ignoring"], show [DamageThruDef])
    , (["increase","incoming","damage"], show [DamageVuln])
    , (["reduce","death","resist"], show [DeathDown])
    , (["debuff","immunity"], show [DebuffResist])
    , (["reduce","defense"], show [DefenseDown])
    , (["increase","defense"], show [DefenseUp])
    , (["remove","buffs","self"], show [DemeritBuffs])
    , (["charge","enemy","gauge"], show [DemeritCharge])
    , (["decrease","hp","fall"], show [DemeritHealth])
    , (["reduce","hp","turn"], show [HealthLoss])
    , (["deal","damage","yourself"], show [DemeritDamage])
    , (["reduce","hp"], show [DemeritDamage])
    , (["reduce","enemy","np","gauge","by"], show [GaugeDown])
    , (["deplete","gauge","on","use"], show [GaugeSpend])
    , (["reduce","own","gauge"], show [DemeritGauge])
    , (["drain","own","gauge"], show [DemeritGauge])
    , (["death","trigger"], show [DemeritKill])
    , (["sacrifice"], show [DemeritKill])
    , (["evade"], show [Evasion])
    , (["increase","master","exp"], show [EXP])
    , (["fear"], show [Fear])
    , (["friend","points"], show [FriendPoints])
    , (["gain","star","turn"], show [StarsPerTurn])
    , (["gain","stars"], show [GainStars])
    , (["decrease","charge"], show [GaugeDown])
    , (["decrease","gauge"], show [GaugeDown])
    , (["gauge","turn"], show [GaugePerTurn])
    , (["restore","gauge"], show [GaugeUp])
    , (["increase","gauge"], show [GaugeUp])
    , (["charge","gauge"], show [GaugeUp])
    , (["begin","np","charged"], show [GaugeUp])
    , (["np","charged","by"], show [GaugeUp])
    , (["reduce","np","gauge","by"], show [DemeritGauge])
    , (["guts"], show [Guts])
    , (["increase","healing","effectiveness"], show [HealUp])
    , (["hp","recovery","per","turn"], show [HealPerTurn])
    , (["restore","turn"], show [HealPerTurn])
    , (["hp","recovery"], show [HealingReceived])
    , (["recover"], show [Heal])
    , (["heal"], show [Heal])
    , (["ignore","invincibility"], show [IgnoreInvinc])
    , (["invincible"], show [Invincibility])
    , (["death","immunity"], show [KillResist])
    , (["death","resist"], show [KillResist])
    , (["death","success"], show [KillUp])
    , (["increase","death","rate"], show [KillUp])
    , (["death"], show [Kill])
    , (["maximum","hp"], show [MaxHP])
    , (["increase","mental","success"], show [MentalSuccess])
    , (["decrease","mental","resist"], show [MentalVuln])
    , (["mental","resist"], show [MentalResist])
    , (["increase","mystic","code"], show [MysticCode])
    , (["decrease","np","strength"], show [NPDown])
    , (["increase","np","gain"], show [NPGen])
    , (["increase","np","strength"], show [NPUp])
    , (["increase","np","damage"], show [NPUp])
    , (["increase","attack","resist"], show [OffensiveResist])
    , (["chance","apply","each"], show [OverChance])
    , (["increase","overcharge"], show [Overcharge])
    , (["quick","performance"], show [Performance Quick])
    , (["arts","performance"], show [Performance Arts])
    , (["buster","performance"], show [Performance Buster])
    , (["increase","qp","drop"], show [QPDrop])
    , (["increase","qp","quest"], show [QPQuest])
    , (["remove","mental"], show [RemoveMental])
    , (["remove","mental_debuff"], show [RemoveMental])
    , (["remove","buff"], show [RemoveBuffs])
    , (["remove","debuff"], show [RemoveDebuffs])
    , (["poison","resist"], show [Resist Poison])
    , (["poison"], show [Poison])
    , (["star","gather"], show [StarAbsorb])
    , (["seal","np"], show [SealNP])
    , (["seal","skill"], show [SealSkills])
    , (["increase","star","generation"], show [StarUp])
    , (["increase","star","drop"], show [StarUp])
    , (["immobilize"], show [Stun])
    , (["increase","stun","success"], show [Success Stun])
    , (["increase","stun","resist"], show [Resist Stun])
    , (["stun","later"], show [StunBomb])
    , (["stun","delayed"], show [StunBomb])
    , (["sure-hit"], show [SureHit])
    , (["sure","hit"], show [SureHit])
    , (["target","focus"], show [Taunt])

    , (["increase","status","effects"], show [MentalResist])
    , (["damage","plus"], show [DamageUp])
    , (["increase","damage"], show [DamageUp])
    , (["increase","debuff","resist"], show [DebuffResist])
    , (["debuff","rate"], show [DebuffSuccess])
    , (["increase","debuff","success"], show [DebuffSuccess])
    , (["increase","stun","success"], show [DebuffSuccess])
    , (["reduce","debuff","resist"], show [DebuffVuln])
    , (["decrease","np"], show [GaugeDown])
    , (["decrease","attack"], show [AttackDown])
    , (["increase","attack"], show [AttackUp])
    , (["severe","damage"], show [Damage])
    , (["deal","damage"], show [Damage])
    , (["stun"], show [Stun])
    , (["high","chance","status"], show [Charm])
    
    , (["against","buster"], [])
    , (["effect","activates"], [])
    , (["depends"], [])
    , (["when","defeated","by"], [])
    , (["when","equipped"], [])
    ]

readEffect : String -> List String
readEffect s = 
  if 
    String.startsWith "<!--" s && String.endsWith "-->" s then
      []
  else
    let
      words =
          s
          |> String.toLower
          >> filterOut ".|[]#,'"
          >> String.trim
          >> String.split " "
      inWords x = List.member x words || List.member (x ++ "s") words
      synonyms x = Maybe.withDefault [x] <| List.find (List.member x) synonym
      match = Tuple.first >> List.all (synonyms >> List.any inWords)
    in
      List.find match effectMap
      |> Maybe.map Tuple.second
      >> Maybe.withDefault words
