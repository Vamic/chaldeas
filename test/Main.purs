module Test.Main where

import Database

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (catMaybes, elem, filter, head, nub, reverse, (..), (\\))
import Data.Either (Either(..))
import Data.Foldable (all, foldl, traverse_)
import Data.Formatter.Number (Formatter(..), format)
import Data.Generic.Rep.Show
import Data.Int (toNumber)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.String (Pattern(..), stripSuffix)
import Data.String as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Global.Unsafe (unsafeEncodeURIComponent)
import Operators (enumArray)
import Prelude (class Eq, class Ord, class Show, Unit, bind, compare, discard, otherwise, pure, show, ($), (<<<), (<>), (<#>), (>>=), (<$>),(||))
import Test.Output.Fancy (runTest)
import Test.Unit (TestSuite, test, suite, failure)
import Test.Unit.Assert (shouldEqual)
import Test.Unit.Main (runTestWith)

wikiRoot ∷ String
wikiRoot = "https://grandorder.wiki/index.php?action=raw&title="

newtype MediaWiki = MediaWiki (Map String String)
toWiki ∷ String -> MediaWiki
toWiki text = MediaWiki <<< fromFoldable <<< reverse <<< catMaybes 
            $ S.split (S.Pattern "|") text <#> \line -> do
        subAt              <- S.indexOf (S.Pattern "=") line
        let {before, after} = S.splitAt subAt line
            afterLines      = S.split (S.Pattern "\n") after
        firstAfterLine     <- head afterLines
        pure $ Tuple (S.trim $ S.toLower before) 
                     (S.trim <<< sanitize $ S.drop 1 firstAfterLine)
  where
    sanitize = fromCharArray <<< filter legal <<< toCharArray
      where
        legal '%' = false
        legal ',' = false
        legal '{' = false
        legal '[' = false
        legal ']' = false
        legal '}' = false
        legal '(' = false
        legal ')' = false
        legal _   = true

wikiZip ∷ ∀ a. Show a => a -> Aff (Tuple a MediaWiki)
wikiZip a = do
    res <- AX.get ResponseFormat.string $ wikiRoot <> encode a
    case res.body of
      Left err  -> pure <<< Tuple a <<< wikiFail
                    $ AX.printResponseFormatError err
      Right obj -> pure <<< Tuple a $ toWiki obj
  where
    wikiFail err = MediaWiki $ fromFoldable [Tuple "err" err]
    encode = unsafeEncodeURIComponent <<< translate <<< show
    translate "Fergus mac Roich" = "Fergus mac Róich"
    translate "Mugashiki—Shinkuu Myou" = "Mugashiki - Shinkuu Myōu"
    translate "Heroic Portrait: Scathach" = "Heroic Portrait: Scáthach"
    translate "Cu Chulainn" = "Cú Chulainn"
    translate "Cu Chulainn (Prototype)" = "Cú Chulainn (Prototype)"
    translate "Cu Chulainn (Alter)" = "Cú Chulainn (Alter)"
    translate "Cu Chulainn (Caster)" = "Cú Chulainn (Caster)"
    translate "Elisabeth Bathory" = "Elisabeth Báthory"
    translate "Elisabeth Bathory (Halloween)" = "Elisabeth Báthory (Halloween)"
    translate "Scathach" = "Scáthach"
    translate "Scathach (Assassin)" = "Scáthach (Assassin)"
    translate "Angra Mainyu" = "Aŋra Mainiiu"
    translate "Edmond Dantes" = "Edmond Dantès"
    translate "Leonardo da Vinci" = "Leonardo Da Vinci"
    translate x = x

wikiMatch ∷ MediaWiki -> String -> String -> TestSuite
wikiMatch (MediaWiki mw) k obj = test k $ case lookup k mw of
    Nothing -> failure $ "Missing property " <> k <> mwErr
    Just v  -> obj `shouldEqual` v
  where
    mwErr = fromMaybe "" do
        err <- lookup "err" mw
        pure $ ": " <> err

testCraftEssence ∷ CraftEssence -> MediaWiki -> TestSuite
testCraftEssence (CraftEssence ce) info@(MediaWiki mw) = case lookup "err" mw of
    Just err -> suite ce.name <<< test "MediaWiki" $ failure err
    _        -> case lookup "id" mw of
        Nothing  -> suite ce.name <<< test "MediaWiki" $ failure "Not found"
        Just _   -> suite ce.name do
            wiki "id"      <<< prId $ toNumber ce.id
            wiki "maxatk"    $ show ce.stats.max.atk
            wiki "maxhp"     $ show ce.stats.max.hp
            wiki "minatk"    $ show ce.stats.base.atk
            wiki "minhp"     $ show ce.stats.base.hp
            wiki "rarity"    $ show ce.rarity
            wiki "imagetype" $ printIcon ce.icon
  where
    wiki = wikiMatch info
    prId = format $ Formatter { comma: false
                              , before: 4
                              , after: 0
                              , abbreviations: false
                              , sign: false
                              }

shouldMatch ∷ ∀ a. Eq a => Ord a => Show a => Array a -> Array a -> TestSuite
shouldMatch a b = do
    test "From DB" $ shouldEqual (nub a \\ nub b) []
    test "From wiki" $ shouldEqual (nub b \\ nub a) []

testServant ∷ Map ShowString MediaWiki -> Servant -> MediaWiki 
              -> TestSuite
testServant skills (Servant s) info@(MediaWiki mw) = case lookup "err" mw of
    Just err -> suite s.name <<< test "MediaWiki" $ failure err
    _        -> case lookup "id" mw of
        Nothing  -> suite s.name <<< test "MediaWiki" $ failure "Not found"
        Just _   -> suite s.name do
            wiki "id"            <<< prId $ toNumber s.id
            wiki "class"           $ show s.class
            wiki "minatk"          $ show s.stats.base.atk
            wiki "maxatk"          $ show s.stats.max.atk
            wiki "minhp"           $ show s.stats.base.hp
            wiki "maxhp"           $ show s.stats.max.hp
            wiki "grailatk"        $ show s.stats.grail.atk
            wiki "grailhp"         $ show s.stats.grail.hp
            wiki "rarity"          $ show s.rarity
            wiki "commandcard"     $ show s.deck
            wiki "attribute"       $ showAttr s.attr
            wiki "quickhit"        $ show s.hits.quick
            wiki "artshit"         $ show s.hits.arts
            wiki "busterhit"       $ show s.hits.buster
            wiki "extrahit"        $ show s.hits.ex
            wiki "deathresist"     $ toString s.death
            wiki "starabsorption"  $ show s.gen.starWeight
            wiki "stargeneration"  $ toString s.gen.starRate
            wiki "npchargeattack"  $ toString s.gen.npAtk
            wiki "npchargedefense" $ show s.gen.npDef
            wiki "icon"            $ show s.phantasm.card
            wiki "hitcount"        $ showHitcount s.phantasm.hits
            wiki "alignment"       $ showAlign s.align
            for_ s.actives $ \a -> suite a.name 
              $ case lookup (ShowString $ unRank a.name) skills of
                Nothing -> test "Skills" <<< failure 
                           $ "Couldn't find skill " <> a.name
                Just skill'@(MediaWiki skill) -> do
                    let skillEffs = showActive <$> a.effect
                        skillVals = ranges a.effect
                        wikiEffs = catMaybes $ (1..7) <#> \num 
                          -> readActive <$> lookup ("effect" <> show num) skill
                        wikiVals = catMaybes $ (1..7) <#> \num
                          -> do
                            let val i = "e-" <> show num <> "lvl" <> show i
                            wikiFrom <- lookup (val 1) skill
                            wikiTo <- lookup (val 10) skill
                            let isPercent = S.contains (Pattern "%") wikiFrom
                            from <- fromString $ unSuffix wikiFrom (Pattern "%") 
                            to   <- fromString $ unSuffix wikiTo (Pattern "%") 
                            pure $ RangeInfo isPercent from to
                    wikiMatch skill' "cooldown1" $ show a.cd
                    shouldMatch skillEffs wikiEffs
                    shouldMatch skillVals wikiVals
  where
    wiki = wikiMatch info
    showAlign (Tuple Neutral Neutral) = "True Neutral"
    showAlign (Tuple a Mad) = show a <> " Madness"
    showAlign (Tuple a b) = show a <> " " <> show b
    showAttr Mankind = "Human"
    showAttr a       = show a
    showHitcount 0 = "－"
    showHitcount a = show a
    prId = format $ Formatter { comma: false
                              , before: 3
                              , after: 0
                              , abbreviations: false
                              , sign: false
                              }

unSuffix ∷ String -> Pattern -> String
unSuffix s pat = fromMaybe s $ stripSuffix pat s

unRank ∷ String -> String
unRank s = foldl acc s $ enumArray ∷ Array Rank
  where
    acc s' rank = unSuffix s' (Pattern $ " " <> show rank)

newtype ShowString = ShowString String
derive instance _0_ ∷ Eq ShowString
instance _1_ ∷ Ord ShowString where
  compare (ShowString a) (ShowString b) = compare a b
instance _2_ ∷ Show ShowString where
    show (ShowString a) = a

main ∷ Effect Unit
main = launchAff_ do
    skills   <- fromFoldable <$> traverse wikiZip (ShowString <$> skillNames)
    servants <- traverse wikiZip servants
    ces      <- traverse wikiZip craftEssences
    runTestWith runTest do
        traverse_ (uncurry $ testServant skills) servants
        traverse_ (uncurry testCraftEssence) ces
  where
    getActives (Servant s) = s.actives
    skillNames    = nub $ unRank <<< _.name <$> (servants >>= getActives)
printIcon ∷ Icon -> String
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
printIcon a = show a

showActive ∷ ActiveEffect -> String
showActive (To _ ef _) = genericShow ef
showActive (Grant _ _ ef _) = genericShow ef
showActive (Debuff _ _ ef _) = genericShow ef
showActive (Bonus ef _) = genericShow ef
showActive (Chances _ _ ef) = showActive ef
showActive (Chance _ ef) = showActive ef
showActive (Times _ ef) = showActive ef
showActive (When _ ef) = showActive ef
showActive (ToMax _ ef) = showActive ef

readActive ∷ String -> String
readActive effect = go 
  where
    words = S.split (Pattern " ") $ S.toLower effect
    inWords x = elem x words || elem (x <> "s") words
    match xs = all inWords xs
    go
      | match ["stun"] = show Stun
      | match ["increasing","atk"] = genericShow AttackUp
      | match ["increasing","def"] = genericShow DefenseUp
      | match ["prevent","np"] = genericShow SealNP
      | match ["gain","stars","per"] = genericShow StarsPerTurn
      | match ["increase","critical","strength"] = genericShow CritUp
      | match ["increase","star","generation"] = genericShow StarUp
      | match ["debuff","immunity"] = genericShow DebuffResist
      | match ["guts"] = genericShow Guts
      | match ["reduce","gauge"] = genericShow GaugeDown
      | match ["increase","def"] = genericShow DefenseUp
      | match ["increase","np","strength"] = genericShow NPUp
      | match ["restore","turn"] = genericShow HealPerTurn
      | match ["restore","turns"] = genericShow HealPerTurn
      | match ["restore"] = genericShow Heal
      | match ["special","defense"] = "Special Defense"
      | match ["burn"] = genericShow Burn
      | match ["def","down"] = genericShow DefenseDown
      | match ["damage","cut"] = genericShow DamageDown
      | match ["damage","plus"] = genericShow DamageUp
      | match ["death","immunity"] = genericShow KillResist
      | match ["death","trigger"] = genericShow DemeritKill
      | match ["defense","down"] = genericShow DefenseDown
      | match ["evade"] = genericShow Evasion
      | match ["recovery","turn"] = genericShow HealPerTurn
      | match ["recover","turn"] = genericShow HealPerTurn
      | match ["recover"] = genericShow Heal
      | match ["ignore","invincibility"] = genericShow IgnoreInvinc
      | match ["invincible"] = genericShow Invincibility
      | match ["np","seal"] = genericShow SealNP
      | match ["special","attack"] = "Special Attack"
      | match ["sure","hit"] = genericShow SureHit
      | match ["target","focus"] = genericShow Taunt
      | match ["immunity","mental"] = genericShow MentalResist
      | match ["increase","damage"] = genericShow DamageUp
      | match ["increase","debuff","success"] = genericShow DebuffSuccess
      | match ["death"] = genericShow Kill
      | match ["charm","resist"] = genericShow CharmVuln
      | match ["charm"] = genericShow Charm
      | match ["skill","seal"] = genericShow SealSkills
      | match ["chance","each"] = genericShow OverChance
      | match ["decrease","charge"] = genericShow GaugeDown
      | match ["immobilize"] = genericShow Stun
      | match ["arts","effectiveness"] = genericShow $ Performance Arts
      | match ["buster","effectiveness"] = genericShow $ Performance Arts
      | match ["quick","effectiveness"] = genericShow $ Performance Arts
      | match ["charge","gauge"] = genericShow GaugeUp
      | match ["deal","damage","defense"] = genericShow DamageThruDef
      | match ["deal","damage","def-ignoring"] = genericShow DamageThruDef
      | match ["extra","own","hp"] = genericShow LastStand
      | match ["extra","damage"] = "Extra Damage"
      | match ["additional","damage"] = "Extra Damage"
      | match ["deal","damage"] = genericShow Damage
      | match ["decrease","atk"] = genericShow AttackDown
      | match ["decrease","critical","rate"] = genericShow CritChance
      | match ["decrease","def"] = genericShow DefenseDown
      | match ["decrease","debuff","resist"] = genericShow DebuffVuln
      | match ["decrease","hp","fall"] = genericShow DemeritHealth
      | match ["decrease","critical","chance"] = genericShow CritChance
      | match ["drain","own","gauge"] = genericShow DemeritGauge
      | match ["gain","stars"] = genericShow GainStars
      | match ["heal"] = genericShow Heal
      | match ["increase","star","drop"] = genericShow StarUp
      | match ["star","gather"] = genericShow StarAbsorb
      | match ["maximum","hp"] = genericShow MaxHP
      | match ["increase","gauge"] = genericShow GaugeUp
      | match ["attack","against"] = "Special Attack"
      | match ["increase","np","gain"] = genericShow NPGen
      | match ["poison","resist"] = genericShow $ Resist Poison
      | match ["quick","performance"] = genericShow $ Performance Quick
      | match ["arts","performance"] = genericShow $ Performance Arts
      | match ["buster","performance"] = genericShow $ Performance Buster
      | match ["increase","critical","damages"] = genericShow CritUp
      | match ["confusion"] = genericShow Confusion
      | match ["curse"] = genericShow Curse
      | match ["curses"] = genericShow Curse
      | match ["fear"] = genericShow Fear
      | match ["nullify"] = genericShow BuffBlock
      | match ["poison"] = genericShow Poison
      | match ["lock","skills"] = genericShow SealNP
      | match ["reduce","hp"] = genericShow DemeritDamage
      | match ["reduce","damage","taken"] = genericShow DamageDown
      | match ["remove","buffs"] = genericShow RemoveBuffs
      | match ["remove","poison"] = genericShow Cure
      | match ["remove","mental"] = genericShow RemoveMental
      | match ["remove","debuffs"] = genericShow RemoveDebuffs
      | match ["remove","effects#mental_debuff"] = genericShow RemoveMental
      | match ["sacrifice"] = genericShow DemeritKill
      | match ["severe","damage"] = genericShow Damage
      | match ["charge","gauge"] = genericShow GaugeUp
      | match ["cooldowns"] = genericShow Cooldowns
      | match ["increase","gauge"] = genericShow GaugeUp
      | match ["damage","previous","hp"] = genericShow Avenge
      | match ["personality"] = genericShow BecomeHyde
      | otherwise = ""
