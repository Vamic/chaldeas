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

import Prelude
import Operators (filterOut, (^))
import Printing (prettify)
import Database (SkillEffect(..), BuffEffect(..), Card(..), DebuffEffect(..), Icon(..), InstantEffect(..), Servant(..), craftEssences)

import Data.Array (elem)
import Data.Foldable (all, any, find)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..))
import Data.String as S
import Data.Tuple (Tuple)

import Test.Base (MaybeRank(..), RankedSkill(..))

upgradeNP :: Array String
upgradeNP = 
    [ "Orion"
    , "Asterios"
    , "Sasaki Kojirou"
    ]

npRank :: Servant -> MaybeRank
npRank (Servant s) = (if s.name `elem` upgradeNP then Upgrade else Pure) 
                     s.phantasm.rank

upgradeSkill :: Array (Tuple String String)
upgradeSkill = 
    [ "Tamamo-no-Mae" ^ "Fox's Wedding"
    ]

uniqueSkill :: Array (Tuple String String)
uniqueSkill = 
    [ "Brynhild" ^ "Primordial Rune"
    , "Scathach" ^ "Primordial Rune"
    , "Euryale"  ^ "Whim of the Goddess"
    , "Stheno"   ^ "Whim of the Goddess"
    , "Henry Jekyll & Hyde" ^ "Monstrous Strength"
    ]

ceNames :: Array String
ceNames = show <$> craftEssences

skillRanks :: Servant -> Array (Tuple MaybeRank RankedSkill)
skillRanks s'@(Servant s) = tuplify <<< go <$> s.skills
  where
    tuplify ranked@(RankedSkill _ rank) = (rank ^ ranked)
    flagged x = elem (s.name ^ x.name)
    go x
      | x.name `elem` ceNames    = RankedSkill x { name = x.name <> " (Skill)" } 
                                                 $ Pure x.rank
      | x `flagged` upgradeSkill = RankedSkill x $ Upgrade x.rank
      | x `flagged` uniqueSkill  = RankedSkill x $ Unique s' x.rank
      | otherwise                = RankedSkill x $ Pure x.rank

translate :: String -> String
translate "Cheerful-Type Mystic Code" = "Cheerful Model Mystic Code"
translate "Mugashiki—Shinkuu Myou" = "Mugashiki - Shinkuu Myōu"
translate "Leonardo da Vinci" = "Leonardo Da Vinci"
translate "Beautiful Princess (Sea)" = "Princess of Loveliness (Ocean)"
translate "Treasure Hunt (Sea)" = "Treasure Hunt (Ocean)"
translate "Glory Is With Me" = "Glory Is With Me!"
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
    showEffect (Grant _ _ (Success _) _) = genericShow DebuffSuccess
    showEffect (Grant _ _ (Resist _) _) = genericShow DebuffResist
    showEffect (To _ ef _) = genericShow ef
    showEffect (Grant _ _ ef _) = genericShow ef
    showEffect (Debuff _ _ ef _) = genericShow ef
    showEffect (Bonus ef _) = genericShow ef
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
    words = S.split (Pattern " ") <<< S.trim <<< 
            filterOut (Pattern ".|[]#") $ S.toLower effect
    inWords x = elem x words || elem (x <> "s") words 
    match xs = all (any inWords <<< synonyms) xs
    synonyms word = fromMaybe [word] $ find (elem word) synonym
    go
      | match ["formula"] = []
      | match ["end","of","turn"] = []

      | match ["reduce","attack","defense","critical","chance","debuff"
              ,"resist","np","strength"] = genericShow <$> 
          [AttackDown, DefenseDown, CritChance, DebuffVuln, NPDown]
      | match ["reduce","critical","chance","debuff","resist"] = genericShow <$>
          [CritChance, DebuffVuln]
      | match ["reduce","defense","critical","chance"] = genericShow <$> 
          [DefenseDown, CritChance]
      | match ["reduce","attack","defense"] = genericShow <$>
          [AttackDown, DefenseDown]
      | match ["increase","gain","critical","strength","drop","recovery"
              ,"debuff","resist"] = genericShow <$>
          [NPGen, CritUp, HealingReceived, StarUp, DebuffResist]
      | match ["increase","attack","defense","star"] = genericShow <$>
          [AttackUp, DefenseUp, StarUp]
      | match ["increase","attack","defense"] = genericShow <$>
          [AttackUp, DefenseUp]
      | match ["reduce","np","strength","critical","strength"] = genericShow <$>
          [NPDown, CritDown]
      | match ["remove","debuffs","restore","hp"] = genericShow <$>
          [RemoveDebuffs, Heal]

      | match ["additional","damage","turn"] = ["Special Attack"]
      | match ["decrease","1000hp","yourself"] = [genericShow DemeritDamage]
      | match ["damage","previous","hp"] = [genericShow Avenge]
      | match ["hp","fall"] = [genericShow DemeritHealth]
      | match ["additional","damage","all","enemies"] = [genericShow Damage]
      | match ["confusion"] = [genericShow Confusion]
      | match ["current","hp"] = [genericShow LastStand]
      | match ["extra","own","hp"] = [genericShow LastStand]
      | match ["against","drop"] = ["Special Stars"]
      | match ["apply","trait"] = ["Apply Trait"]
      | match ["extra","damage"] = ["Extra Damage"]
      | match ["special","attack"] = ["Special Attack"]
      | match ["special","damage"] = ["Special Attack"]
      | match ["special","defense"] = ["Special Defense"]
      | match ["transform","hyde"] = [genericShow BecomeHyde]
      | match ["nullify"] = [genericShow BuffBlock]
      | match ["prevent","buff"] = [genericShow BuffBlock]
      | match ["decrease","attack","success","yourself"] = [genericShow BuffFail]
      | match ["decrease","buff","success"] = [genericShow BuffFail]
      | match ["increase","buff","success"] = [genericShow BuffUp]
      | match ["burn"] = [genericShow Burn]
      | match ["charm","resist"] = [genericShow CharmVuln]
      | match ["charm"] = [genericShow Charm]
      | match ["cooldowns"] = [genericShow Cooldowns]
      | match ["decrease","critical","chance"] = [genericShow CritChance]
      | match ["increase","critical","damage"] = [genericShow CritUp]
      | match ["increase","critical","strength"] = [genericShow CritUp]
      | match ["remove","poison"] = [genericShow Cure]
      | match ["curse"] = [genericShow Curse]
      | match ["additional","damage","all","enemies"] = [genericShow Damage]
      | match ["damage","cut"] = [genericShow DamageDown]
      | match ["reduce","damage","taken"] = [genericShow DamageDown]
      | match ["deal","damage","defense"] = [genericShow DamageThruDef]
      | match ["deal","damage","def-ignoring"] = [genericShow DamageThruDef]
      | match ["increase","incoming","damage"] = [genericShow DamageVuln]
      | match ["reduce","death","resist"] = [genericShow DeathDown]
      | match ["debuff","immunity"] = [genericShow DebuffResist]
      | match ["reduce","defense"] = [genericShow DefenseDown]
      | match ["increase","defense"] = [genericShow DefenseUp]
      | match ["remove","buffs","self"] = [genericShow DemeritBuffs]
      | match ["charge","enemy","gauge"] = [genericShow DemeritCharge]
      | match ["decrease","hp","fall"] = [genericShow DemeritHealth]
      | match ["deal","damage","yourself"] = [genericShow DemeritDamage]
      | match ["reduce","hp"] = [genericShow DemeritDamage]
      | match ["reduce","enemy","np","gauge","by"] = [genericShow GaugeDown]
      | match ["reduce","own","gauge"] = [genericShow DemeritGauge]
      | match ["drain","own","gauge"] = [genericShow DemeritGauge]
      | match ["death","trigger"] = [genericShow DemeritKill]
      | match ["sacrifice"] = [genericShow DemeritKill]
      | match ["evade"] = [genericShow Evasion]
      | match ["fear"] = [genericShow Fear]
      | match ["gain","star","turn"] = [genericShow StarsPerTurn]
      | match ["gain","stars"] = [genericShow GainStars]
      | match ["decrease","charge"] = [genericShow GaugeDown]
      | match ["decrease","gauge"] = [genericShow GaugeDown]
      | match ["gauge","turn"] = [genericShow GaugePerTurn]
      | match ["restore","gauge"] = [genericShow GaugeUp]
      | match ["increase","gauge"] = [genericShow GaugeUp]
      | match ["charge","gauge"] = [genericShow GaugeUp]
      | match ["reduce","np","gauge","by"] = [genericShow DemeritGauge]
      | match ["guts"] = [genericShow Guts]
      | match ["increase","healing","effectiveness"] = [genericShow HealUp]
      | match ["hp","recovery","per","turn"] = [genericShow HealPerTurn]
      | match ["restore","turn"] = [genericShow HealPerTurn]
      | match ["hp","recovery"] = [genericShow HealingReceived]
      | match ["recover"] = [genericShow Heal]
      | match ["heal"] = [genericShow Heal]
      | match ["ignore","invincibility"] = [genericShow IgnoreInvinc]
      | match ["invincible"] = [genericShow Invincibility]
      | match ["death","immunity"] = [genericShow KillResist]
      | match ["death","success"] = [genericShow KillUp]
      | match ["increase","death","rate"] = [genericShow KillUp]
      | match ["death"] = [genericShow Kill]
      | match ["maximum","hp"] = [genericShow MaxHP]
      | match ["increase","mental","success"] = [genericShow MentalSuccess]
      | match ["decrease","mental","resist"] = [genericShow MentalVuln]
      | match ["mental","resist"] = [genericShow MentalResist]
      | match ["decrease","np","strength"] = [genericShow NPDown]
      | match ["increase","np","gain"] = [genericShow NPGen]
      | match ["increase","np","strength"] = [genericShow NPUp]
      | match ["increase","np","damage"] = [genericShow NPUp]
      | match ["quick","performance"] = [genericShow $ Performance Quick]
      | match ["arts","performance"] = [genericShow $ Performance Arts]
      | match ["buster","performance"] = [genericShow $ Performance Buster]
      | match ["increase","attack","resist"] = [genericShow OffensiveResist]
      | match ["chance","apply","each"] = [genericShow OverChance]
      | match ["increase","overcharge"] = [genericShow Overcharge]
      | match ["remove","mental"] = [genericShow RemoveMental]
      | match ["remove","mental_debuff"] = [genericShow RemoveMental]
      | match ["remove","buff"] = [genericShow RemoveBuffs]
      | match ["remove","debuff"] = [genericShow RemoveDebuffs]
      | match ["poison","resist"] = [genericShow DebuffResist]
      | match ["poison"] = [genericShow Poison]
      | match ["star","gather"] = [genericShow StarAbsorb]
      | match ["seal","np"] = [genericShow SealNP]
      | match ["seal","skill"] = [genericShow SealSkills]
      | match ["increase","star","generation"] = [genericShow StarUp]
      | match ["increase","star","drop"] = [genericShow StarUp]
      | match ["immobilize"] = [genericShow Stun]
      | match ["stun","later"] = [genericShow StunBomb]
      | match ["stun","delayed"] = [genericShow StunBomb]
      | match ["sure","hit"] = [genericShow SureHit]
      | match ["target","focus"] = [genericShow Taunt]

      | match ["increase","status","effects"] = [genericShow MentalResist]
      | match ["damage","plus"] = [genericShow DamageUp]
      | match ["increase","damage"] = [genericShow DamageUp]
      | match ["increase","debuff","resist"] = [genericShow DebuffResist]
      | match ["debuff","rate"] = [genericShow DebuffSuccess]
      | match ["increase","debuff","success"] = [genericShow DebuffSuccess]
      | match ["increase","stun","success"] = [genericShow DebuffSuccess]
      | match ["reduce","debuff","resist"] = [genericShow DebuffVuln]
      | match ["decrease","np"] = [genericShow GaugeDown]
      | match ["decrease","attack"] = [genericShow AttackDown]
      | match ["increase","attack"] = [genericShow AttackUp]
      | match ["severe","damage"] = [genericShow Damage]
      | match ["deal","damage"] = [genericShow Damage]
      | match ["stun"] = [genericShow Stun]

      | match ["high","chance","status"] = [genericShow Charm]

      | match ["against","buster"] = []
      | match ["effect","activates"] = []
      | match ["depends"] = []

      | otherwise = [show words]
