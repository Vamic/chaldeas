module Test.Main where

import Database
import Prelude (class Show, Unit, bind, discard, pure, show, ($), (<<<), (<>), (<#>))

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (catMaybes, filter, head, reverse)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Formatter.Number (Formatter(..), format)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toString)
import Data.Int (toNumber)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Global.Unsafe (unsafeEncodeURIComponent)
import Test.Output.Fancy (runTest)
import Test.Unit (TestSuite, test, suite, failure)
import Test.Unit.Assert (shouldEqual)
import Test.Unit.Main (runTestWith)

import Data.String as S

wikiRoot ∷ String
wikiRoot = "https://grandorder.wiki/index.php?action=raw&title="

newtype MediaWiki = MediaWiki (Map String String)
toWiki ∷ String -> MediaWiki
toWiki text = MediaWiki <<< fromFoldable <<< reverse $ fromMaybe err do
    startAt             <- S.indexOf (S.Pattern "Box") text
    let {after: content} = S.splitAt startAt text
    pure <<< catMaybes $ S.split (S.Pattern "|") content <#> \line -> do
        subAt              <- S.indexOf (S.Pattern "=") line
        let {before, after} = S.splitAt subAt line
            afterLines      = S.split (S.Pattern "\n") after
        firstAfterLine     <- head afterLines
        pure $ Tuple (S.trim $ S.toLower before) 
                     (S.trim <<< sanitize $ S.drop 1 firstAfterLine)
  where
    err = [Tuple "err" "Missing"]
    sanitize = fromCharArray <<< filter legal <<< toCharArray
      where
        legal '%' = false
        legal ',' = false
        legal '}' = false
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

testServant ∷ Servant -> MediaWiki -> TestSuite
testServant (Servant s) info@(MediaWiki mw) = case lookup "err" mw of
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

main ∷ Effect Unit
main = launchAff_ do
    servants <- traverse wikiZip servants
    ces      <- traverse wikiZip craftEssences
    runTestWith runTest do
        traverse_ (uncurry testServant) servants
        traverse_ (uncurry testCraftEssence) ces

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
