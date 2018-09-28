-- | Parses text into game data. Stores specific game information like
-- | "what does 'Reduces one enemy's Critical Rate' mean" and
-- | "which skills of which Servants are unique to them
-- | (e.g. Hyde's Monstrous Strength)."
module Test.Parse
  ( translate
  , printIcon
  , effects
  , readEffect
  , skillRanks
  , npRank
  ) where

import StandardLibrary
import Generic     as G
import Data.String as String

import Test.Data.Multimap as Multimap

import Printing (prettify)
import Database (BuffEffect(..), Card(..), DebuffEffect(..), Icon(..), InstantEffect(..), Servant(..), Skill, SkillEffect(..), craftEssences)
import Test.Data.MaybeRank (MaybeRank(..))
import Test.Data.Multimap (Multimap)

upgradeNP :: Array String
upgradeNP =
    [ "Orion"
    , "Asterios"
    , "Sasaki Kojirou"
    ]

uniqueNP :: Array String
uniqueNP = 
    [ "Mash Kyrielight" ]

npRank :: Servant -> MaybeRank
npRank s'@(Servant s)
  | s.name `elem` upgradeNP = Upgrade s.phantasm.rank
  | s.name `elem` uniqueNP  = Unique s' s.phantasm.rank
  | otherwise               = Pure s.phantasm.rank

upgradeSkill :: Array (String : String)
upgradeSkill =
    [ "Tamamo-no-Mae" : "Fox's Wedding"
    ]

uniqueSkill :: Array (String : String)
uniqueSkill =
    [ "Brynhild" : "Primordial Rune"
    , "Scathach" : "Primordial Rune"
    , "Euryale"  : "Whim of the Goddess"
    , "Stheno"   : "Whim of the Goddess"
    , "Henry Jekyll & Hyde" : "Monstrous Strength"
    ]

ceNames :: Array String
ceNames = show <$> craftEssences

skillRanks :: Servant -> Multimap Skill MaybeRank
skillRanks s'@(Servant s) = Multimap.fromFoldable $ go <$> s.skills
  where
    flagged x = elem (s.name : x.name)
    go x
      | x.name `elem` ceNames    = x { name = x.name <> " (Skill)" }
                                     : Pure x.rank
      | x `flagged` upgradeSkill = x : Upgrade x.rank
      | x `flagged` uniqueSkill  = x : Unique s' x.rank
      | otherwise                = x : Pure x.rank

translate :: String -> String
translate "Cheerful-Type Mystic Code" = "Cheerful Model Mystic Code"
translate "Mugashiki—Shinkuu Myou" = "Mugashiki - Shinkuu Myōu"
translate "Leonardo da Vinci" = "Leonardo Da Vinci"
translate "Beautiful Princess (Sea)" = "Princess of Loveliness (Ocean)"
translate "Treasure Hunt (Sea)" = "Treasure Hunt (Ocean)"
translate x = prettify x

printIcon :: Icon -> String
printIcon IconAllUp = "All Up"
printIcon IconArtsUp = "Arts Up"
printIcon IconBusterArtsUp = "ArtsBuster Up"
printIcon IconSwordUp = "Attack Up"
printIcon IconSwordShieldUp = "Dual Up"
printIcon IconRainbow = "Bond Up"
printIcon IconBusterUp = "Buster Up"
printIcon IconQuickBusterUp = "BusterQuick Up"
printIcon IconStarHaloUp = "CStar Drop Up"
printIcon IconStarUp = "CStar Gather Up"
printIcon IconStarTurn = "CStar Turn"
printIcon IconHeart = "Charm"
printIcon IconExclamationUp = "Crit Up"
printIcon IconDamageUp = "Damage Up"
printIcon IconFire = "Death Res Up"
printIcon IconReaperUp = "Death Up"
printIcon IconHoodUp = "Debuff Res Up"
printIcon IconStaffUp = "Debuff Up"
printIcon IconShieldUp = "Defense Up"
printIcon IconDodge = "Evade"
printIcon IconKneel = "Guts"
printIcon IconHealTurn = "Heal Turn"
printIcon IconHealUp = "Heal Up"
printIcon IconHeal = "Heal"
printIcon IconShieldBreak = "Invul Pierce"
printIcon IconShield = "Invul"
printIcon IconRoad = "Journey"
printIcon IconHPUp = "Max HP Up"
printIcon IconNoble = "NP Charge"
printIcon IconNobleUp = "NP Generation"
printIcon IconNobleRedUp = "NP Generation2"
printIcon IconNobleTurn = "NP Turn"
printIcon IconBeamUp = "NP Up"
printIcon IconQuartz = "QP Up"
printIcon IconQuickUp = "Quick Up"
printIcon IconArtsQuickUp = "QuickArts Up"
printIcon IconCircuits = "Skill Seal"
printIcon IconCrosshairUp = "Taunt"
printIcon x = show x

effects :: Array SkillEffect -> Array String
effects = map showEffect
  where
    showEffect (Debuff _ _ (ApplyTrait _) _) = "Apply Trait"
    showEffect (To _ (DamageVs _) _) = "Extra Damage"
    showEffect (To _ DamagePoison _) = "Extra Damage"
    showEffect (Grant _ _ (AttackVs _) _) = "Special Attack"
    showEffect (Grant _ _ (AlignAffinity _) _) = "Special Attack"
    showEffect (Grant _ _ (ClassAffinity _) _) = "Special Attack"
    showEffect (Grant _ _ (DefenseVs _) _) = "Special Defense"
    showEffect (Grant _ _ (StarAffinity _) _) = "Special Stars"
    showEffect (Grant _ _ (Success _) _) = G.genericShow DebuffSuccess
    showEffect (Grant _ _ (Resist _) _) = G.genericShow DebuffResist
    showEffect (To _ ef _) = G.genericShow ef
    showEffect (Grant _ _ ef _) = G.genericShow ef
    showEffect (Debuff _ _ ef _) = G.genericShow ef
    showEffect (Bonus ef _) = G.genericShow ef
    showEffect (Chances _ _ ef) = showEffect ef
    showEffect (Chance _ ef) = showEffect ef
    showEffect (Times _ ef) = showEffect ef
    showEffect (When _ ef) = showEffect ef
    showEffect (ToMax _ ef) = showEffect ef

synonym :: Array (Array String)
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

readEffect :: String -> Array String
readEffect effect = go
  where
    words = String.split (Pattern " ") <<< String.trim <<<
            filterOut (Pattern ".|[]#") $ String.toLower effect
    inWords x = elem x words || elem (x <> "s") words
    match xs = all (any inWords <<< synonyms) xs
    synonyms word = fromMaybe [word] $ find (elem word) synonym
    -- Since PureScript doesn't yet support where-binding across guard patterns
    go
      | match ["formula"] = []
      | match ["bonus","increases"] = []
      | match ["end","of","turn"] = []

      | match ["reduce","attack","defense","critical","chance","debuff"
              ,"resist","np","strength"] = G.genericShow
          <$> [AttackDown, DefenseDown, CritChance, DebuffVuln, NPDown]
      | match ["reduce","critical","chance","debuff","resist"] = G.genericShow
          <$> [CritChance, DebuffVuln]
      | match ["reduce","defense","critical","chance"] = G.genericShow
          <$> [DefenseDown, CritChance]
      | match ["reduce","attack","defense"] = G.genericShow
          <$> [AttackDown, DefenseDown]
      | match ["increase","gain","critical","strength","drop","recovery"
              ,"debuff","resist"] = G.genericShow
          <$> [NPGen, CritUp, HealingReceived, StarUp, DebuffResist]
      | match ["increase","attack","defense","star"] = G.genericShow
          <$> [AttackUp, DefenseUp, StarUp]
      | match ["increase","attack","defense"] = G.genericShow
          <$> [AttackUp, DefenseUp]
      | match ["reduce","np","strength","critical","strength"] = G.genericShow
          <$> [NPDown, CritDown]
      | match ["remove","debuffs","restore","hp"] = G.genericShow
          <$> [RemoveDebuffs, Heal]

      | match ["additional","damage","turn"] = ["Special Attack"]
      | match ["decrease","1000hp","yourself"] = [G.genericShow DemeritDamage]
      | match ["damage","previous","hp"] = [G.genericShow Avenge]
      | match ["hp","fall"] = [G.genericShow DemeritHealth]
      | match ["additional","damage","all","enemies"] = [G.genericShow Damage]
      | match ["confusion"] = [G.genericShow Confusion]
      | match ["current","hp"] = [G.genericShow LastStand]
      | match ["extra","own","hp"] = [G.genericShow LastStand]
      | match ["against","drop"] = ["Special Stars"]
      | match ["apply","trait"] = ["Apply Trait"]
      | match ["extra","damage"] = ["Extra Damage"]
      | match ["special","attack"] = ["Special Attack"]
      | match ["special","damage"] = ["Special Attack"]
      | match ["special","defense"] = ["Special Defense"]
      | match ["transform","hyde"] = [G.genericShow BecomeHyde]
      | match ["nullify"] = [G.genericShow BuffBlock]
      | match ["prevent","buff"] = [G.genericShow BuffBlock]
      | match ["decrease","attack","success","yourself"] =
          [G.genericShow BuffFail]
      | match ["decrease","buff","success"] = [G.genericShow BuffFail]
      | match ["increase","buff","success"] = [G.genericShow BuffUp]
      | match ["burn"] = [G.genericShow Burn]
      | match ["charm","resist"] = [G.genericShow CharmVuln]
      | match ["charm"] = [G.genericShow Charm]
      | match ["cooldowns"] = [G.genericShow Cooldowns]
      | match ["decrease","critical","chance"] = [G.genericShow CritChance]
      | match ["increase","critical","damage"] = [G.genericShow CritUp]
      | match ["increase","critical","strength"] = [G.genericShow CritUp]
      | match ["remove","poison"] = [G.genericShow Cure]
      | match ["curse"] = [G.genericShow Curse]
      | match ["additional","damage","all","enemies"] = [G.genericShow Damage]
      | match ["damage","cut"] = [G.genericShow DamageDown]
      | match ["reduce","damage","taken"] = [G.genericShow DamageDown]
      | match ["deal","damage","defense"] = [G.genericShow DamageThruDef]
      | match ["deal","damage","def-ignoring"] = [G.genericShow DamageThruDef]
      | match ["increase","incoming","damage"] = [G.genericShow DamageVuln]
      | match ["reduce","death","resist"] = [G.genericShow DeathDown]
      | match ["debuff","immunity"] = [G.genericShow DebuffResist]
      | match ["reduce","defense"] = [G.genericShow DefenseDown]
      | match ["increase","defense"] = [G.genericShow DefenseUp]
      | match ["remove","buffs","self"] = [G.genericShow DemeritBuffs]
      | match ["charge","enemy","gauge"] = [G.genericShow DemeritCharge]
      | match ["decrease","hp","fall"] = [G.genericShow DemeritHealth]
      | match ["deal","damage","yourself"] = [G.genericShow DemeritDamage]
      | match ["reduce","hp"] = [G.genericShow DemeritDamage]
      | match ["reduce","enemy","np","gauge","by"] = [G.genericShow GaugeDown]
      | match ["reduce","own","gauge"] = [G.genericShow DemeritGauge]
      | match ["drain","own","gauge"] = [G.genericShow DemeritGauge]
      | match ["death","trigger"] = [G.genericShow DemeritKill]
      | match ["sacrifice"] = [G.genericShow DemeritKill]
      | match ["evade"] = [G.genericShow Evasion]
      | match ["fear"] = [G.genericShow Fear]
      | match ["gain","star","turn"] = [G.genericShow StarsPerTurn]
      | match ["gain","stars"] = [G.genericShow GainStars]
      | match ["decrease","charge"] = [G.genericShow GaugeDown]
      | match ["decrease","gauge"] = [G.genericShow GaugeDown]
      | match ["gauge","turn"] = [G.genericShow GaugePerTurn]
      | match ["restore","gauge"] = [G.genericShow GaugeUp]
      | match ["increase","gauge"] = [G.genericShow GaugeUp]
      | match ["charge","gauge"] = [G.genericShow GaugeUp]
      | match ["reduce","np","gauge","by"] = [G.genericShow DemeritGauge]
      | match ["guts"] = [G.genericShow Guts]
      | match ["increase","healing","effectiveness"] = [G.genericShow HealUp]
      | match ["hp","recovery","per","turn"] = [G.genericShow HealPerTurn]
      | match ["restore","turn"] = [G.genericShow HealPerTurn]
      | match ["hp","recovery"] = [G.genericShow HealingReceived]
      | match ["recover"] = [G.genericShow Heal]
      | match ["heal"] = [G.genericShow Heal]
      | match ["ignore","invincibility"] = [G.genericShow IgnoreInvinc]
      | match ["invincible"] = [G.genericShow Invincibility]
      | match ["death","immunity"] = [G.genericShow KillResist]
      | match ["death","success"] = [G.genericShow KillUp]
      | match ["increase","death","rate"] = [G.genericShow KillUp]
      | match ["death"] = [G.genericShow Kill]
      | match ["maximum","hp"] = [G.genericShow MaxHP]
      | match ["increase","mental","success"] = [G.genericShow MentalSuccess]
      | match ["decrease","mental","resist"] = [G.genericShow MentalVuln]
      | match ["mental","resist"] = [G.genericShow MentalResist]
      | match ["decrease","np","strength"] = [G.genericShow NPDown]
      | match ["increase","np","gain"] = [G.genericShow NPGen]
      | match ["increase","np","strength"] = [G.genericShow NPUp]
      | match ["increase","np","damage"] = [G.genericShow NPUp]
      | match ["quick","performance"] = [G.genericShow $ Performance Quick]
      | match ["arts","performance"] = [G.genericShow $ Performance Arts]
      | match ["buster","performance"] = [G.genericShow $ Performance Buster]
      | match ["increase","attack","resist"] = [G.genericShow OffensiveResist]
      | match ["chance","apply","each"] = [G.genericShow OverChance]
      | match ["increase","overcharge"] = [G.genericShow Overcharge]
      | match ["remove","mental"] = [G.genericShow RemoveMental]
      | match ["remove","mental_debuff"] = [G.genericShow RemoveMental]
      | match ["remove","buff"] = [G.genericShow RemoveBuffs]
      | match ["remove","debuff"] = [G.genericShow RemoveDebuffs]
      | match ["poison","resist"] = [G.genericShow DebuffResist]
      | match ["poison"] = [G.genericShow Poison]
      | match ["star","gather"] = [G.genericShow StarAbsorb]
      | match ["seal","np"] = [G.genericShow SealNP]
      | match ["seal","skill"] = [G.genericShow SealSkills]
      | match ["increase","star","generation"] = [G.genericShow StarUp]
      | match ["increase","star","drop"] = [G.genericShow StarUp]
      | match ["immobilize"] = [G.genericShow Stun]
      | match ["stun","later"] = [G.genericShow StunBomb]
      | match ["stun","delayed"] = [G.genericShow StunBomb]
      | match ["sure","hit"] = [G.genericShow SureHit]
      | match ["target","focus"] = [G.genericShow Taunt]

      | match ["increase","status","effects"] = [G.genericShow MentalResist]
      | match ["damage","plus"] = [G.genericShow DamageUp]
      | match ["increase","damage"] = [G.genericShow DamageUp]
      | match ["increase","debuff","resist"] = [G.genericShow DebuffResist]
      | match ["debuff","rate"] = [G.genericShow DebuffSuccess]
      | match ["increase","debuff","success"] = [G.genericShow DebuffSuccess]
      | match ["increase","stun","success"] = [G.genericShow DebuffSuccess]
      | match ["reduce","debuff","resist"] = [G.genericShow DebuffVuln]
      | match ["decrease","np"] = [G.genericShow GaugeDown]
      | match ["decrease","attack"] = [G.genericShow AttackDown]
      | match ["increase","attack"] = [G.genericShow AttackUp]
      | match ["severe","damage"] = [G.genericShow Damage]
      | match ["deal","damage"] = [G.genericShow Damage]
      | match ["stun"] = [G.genericShow Stun]

      | match ["high","chance","status"] = [G.genericShow Charm]

      | match ["against","buster"] = []
      | match ["effect","activates"] = []
      | match ["depends"] = []

      | otherwise = [show words]
