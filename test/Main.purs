module Test.Main where

import Database
import Prelude (class Show, Unit, bind, discard, map, min, notEq, pure, show, ($), (<<<), (<>), (<$>))

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Array (head, length, sort, take)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Formatter.Number (Formatter(..), format)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.String.CodeUnits (takeWhile)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Output.Fancy (runTest)
import Test.Unit (TestSuite, test, suite, failure)
import Test.Unit.Assert (shouldEqual)
import Test.Unit.Main (runTestWith)

import Data.String as S

newtype WikiItem = WikiItem { datatype ∷ Int, item ∷ String } 
instance _0_ ∷ DecodeJson WikiItem where
  decodeJson json = do
      obj      <- decodeJson json
      datatype <- obj .? "type"
      item     <- obj .? "item"
      pure $ WikiItem { datatype, item }

newtype WikiData = WikiData (Tuple String (Array String))
instance _1_ ∷ DecodeJson WikiData where
  decodeJson json = do
      obj      <- decodeJson json
      property <- obj .? "property"
      dataitem <- obj .? "dataitem"
      pure <<< WikiData <<< Tuple property $ unItem <$> dataitem
    where
      unItem (WikiItem {item}) = S.replaceAll (S.Pattern ",") (S.Replacement "")
                               $ takeWhile (notEq '#') item
  
newtype MediaWiki = MediaWiki (Map String (Array String))
instance _2_ ∷ DecodeJson MediaWiki where
  decodeJson json = do
      obj        <- decodeJson json
      query      <- obj   .? "query"
      properties <- query .? "data"
      pure <<< MediaWiki $ foldData properties
    where
      foldData ∷ Array WikiData -> Map String (Array String)
      foldData = fromFoldable <<< map \(WikiData wd) -> wd

wikiMatch ∷ MediaWiki -> String -> Array String -> TestSuite
wikiMatch (MediaWiki mw) k obj = test k $ case lookup k mw of
    Nothing -> failure $ "Missing property " <> k <> mwErr
    Just v  -> let lineup = sort <<< take (min (length v) (length obj)) in
               lineup obj `shouldEqual` lineup v
  where
    mwErr = fromMaybe "" do
        err <- lookup "err" mw
        h <- head err
        pure $ ": " <> h

wikiFail ∷ String -> MediaWiki
wikiFail err = MediaWiki $ fromFoldable [Tuple "err" [err]]

wikiRoot ∷ String
wikiRoot = "https://grandorder.wiki/api.php?format=json&action=browsebysubject&subject=" 

wikiZip ∷ ∀ a. Show a => a -> Aff (Tuple a MediaWiki)
wikiZip a = do
    res <- AX.get ResponseFormat.json $ wikiRoot <> show a
    case res.body of
      Left err  -> pure <<< Tuple a <<< wikiFail
                    $ AX.printResponseFormatError err
      Right obj -> case decodeJson obj of
        Left err       -> pure <<< Tuple a $ wikiFail err
        Right wikiData -> pure $ Tuple a wikiData


testCraftEssence ∷ CraftEssence -> MediaWiki -> TestSuite
testCraftEssence (CraftEssence ce) info@(MediaWiki mw) = case lookup "err" mw of
    Just [err] -> suite ce.name <<< test "MediaWiki" $ failure err
    _          -> case lookup "Id" mw of
        Nothing  -> suite ce.name <<< test "MediaWiki" $ failure "Not found"
        Just _   -> suite ce.name do
            wiki "Id"     [prId $ toNumber ce.id]
            wiki "Maxatk" [show ce.stats.max.atk]
            wiki "Maxhp"  [show ce.stats.max.hp]
            wiki "Minatk" [show ce.stats.base.atk]
            wiki "Minhp"  [show ce.stats.base.hp]
            wiki "Rarity" [show ce.rarity]
            wiki "Image"  [printIcon ce.icon]
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
    Just [err] -> suite s.name <<< test "MediaWiki" $ failure err
    _          -> case lookup "Id" mw of
        Nothing  -> suite s.name <<< test "MediaWiki" $ failure "Not found"
        Just _   -> suite s.name do
            wiki "Id"        [prId $ toNumber s.id]
            wiki "Alignment" [showAlign s.align]
            wiki "Attribute" [showAttr s.attr]
            wiki "Class"     [show s.class]
            wiki "Maxatk"    [show s.stats.max.atk]
            wiki "Maxhp"     [show s.stats.max.hp]
            wiki "Minatk"    [show s.stats.base.atk]
            wiki "Minhp"     [show s.stats.base.hp]
            wiki "NP_Icon"   [show s.phantasm.card]
            wiki "Rarity"    [show s.rarity]
            --wiki "Actives"   $ _.name <$> s.actives
  where
    wiki = wikiMatch info
    showAlign (Tuple Neutral Neutral) = "True Neutral"
    --showAlign (Tuple a Balanced) = show a <> " Neutral"
    showAlign (Tuple a Mad) = show a <> " Madness"
    showAlign (Tuple a b) = show a <> " " <> show b
    showAttr Mankind = "Human"
    showAttr a       = show a
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
