module Test.Main where

import Prelude
import Operators (maybeDo)

import Effect.Aff     as Aff
import Data.Array     as Array
import Effect.Console as Console
import Database       as DB
import Data.Int       as Int
import Data.Map       as Map
import Data.Maybe     as Maybe
import Data.Number    as Number
import Data.String    as String

import Node.Yargs.Setup       as Setup
import Node.Yargs.Applicative as Yargs

import Control.MonadZero (guard)
import Data.Array ((..), (\\))
import Data.Either (Either(..))
import Data.Formatter.Number (Formatter(..), format)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.String (Pattern(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Unit (TestSuite, test, suite, failure)
import Test.Unit.Assert (assert, equal')
import Test.Unit.Main (runTestWith)
import Test.Fancy (runTest)

import Test.Base (class CleanShow, cleanShow, MaybeRank(..), addRank)
import Test.MediaWiki (MediaWiki(..), wiki, wikiRange)
import Test.Parse (printIcon, effects, readEffect, skillRanks, npRank)

main :: Effect Unit
main = Yargs.runY (usage <> example) $ app 
       <$> yargNums "ce" "Limit Craft Essences"
       <*> yargNums "s"  "Limit Servants"
  where
    yargNums cmd desc = Yargs.yarg cmd [] (Just desc) (Left (-1)) false
    usage = Setup.usage     "$0 [--ce <amount>] [--s <amount>]" 
    example = Setup.example "$0 --ce 10 --s 0" 
                            "Test 10 Craft Essences and no Servants."


app :: Int -> Int -> Effect Unit
app ce servant = Console.log msg *> Aff.launchAff_ do
    skills <- Map.fromFoldable <$> traverse wiki (servants >>= skillRanks)
    stats  <- traverse (wiki <<< Tuple Unranked) servants
    nps    <- traverse (wiki <<< addRank npRank) servants
    ces    <- traverse (wiki <<< Tuple Unranked) (maybeTake ce DB.craftEssences)
    runTestWith runTest do
        traverse_ (testSkills skills) servants
        traverse_ testServant stats
        traverse_ testNP nps
        traverse_ testCraftEssence ces
  where
    show' (-1) = "all"
    show' a    = show a
    msg        = "Scanning " <> show' ce <> " Craft Essences and " 
                 <> show' servant <> " Servants"
    servants   = maybeTake servant DB.servants
    maybeTake :: ∀ a. Int -> Array a -> Array a
    maybeTake (-1)   = identity
    maybeTake limits = Array.take limits

wikiMatch :: MediaWiki -> String -> String -> TestSuite
wikiMatch (MediaWiki mw) k obj = test k $ case Map.lookup k mw of
    Just v  -> assert (obj <> " not in [" <> String.joinWith ", " v <> "]") $
               obj `Array.elem` v
    Nothing -> failure $ "Missing property " <> k <> " in " <> show mw 
            <> Maybe.maybe "" (append ": ") (Map.lookup "err" mw >>= Array.head)

testCraftEssence :: Tuple DB.CraftEssence MediaWiki -> TestSuite
testCraftEssence (Tuple (DB.CraftEssence ce) mw) = suite ce.name do
    match "id"      <<< prId $ Int.toNumber ce.id
    match "maxatk"    $ show ce.stats.max.atk
    match "maxhp"     $ show ce.stats.max.hp
    match "minatk"    $ show ce.stats.base.atk
    match "minhp"     $ show ce.stats.base.hp
    match "rarity"    $ show ce.rarity
    match "imagetype" $ printIcon ce.icon
  where
    match = wikiMatch mw
    prId = format $ Formatter { comma: false
                              , before: 4
                              , after: 0
                              , abbreviations: false
                              , sign: false
                              }

shouldMatch :: ∀ a. Eq a => CleanShow a => Array a -> Array a -> TestSuite
shouldMatch a b = do
    test "Wiki" <<< beNull $ Array.nubEq a \\ Array.nubEq b
    test "DB"   <<< beNull $ Array.nubEq b \\ Array.nubEq a
  where
    beNull xs = equal' ("missing " <> String.joinWith ", " (cleanShow <$> xs)) xs []

testServant :: Tuple DB.Servant MediaWiki -> TestSuite
testServant (Tuple (DB.Servant s) mw) = suite (s.name <> ": Stats") do
      match "id"            <<< prId $ Int.toNumber s.id
      match "class"           $ show s.class
      match "minatk"          $ show s.stats.base.atk
      match "maxatk"          $ show s.stats.max.atk
      match "minhp"           $ show s.stats.base.hp
      match "maxhp"           $ show s.stats.max.hp
      match "grailatk"        $ show s.stats.grail.atk
      match "grailhp"         $ show s.stats.grail.hp
      match "rarity"          $ show s.rarity
      match "commandcard"     $ show s.deck
      match "attribute"       $ showAttr s.attr
      match "quickhit"        $ show s.hits.quick
      match "artshit"         $ show s.hits.arts
      match "busterhit"       $ show s.hits.buster
      match "extrahit"        $ show s.hits.ex
      match "deathresist"     $ toString s.death
      match "starabsorption"  $ show s.gen.starWeight
      match "stargeneration"  $ toString s.gen.starRate
      match "npchargeattack"  $ toString s.gen.npAtk
      match "npchargedefense" $ show s.gen.npDef
      match "icon"            $ show s.phantasm.card
      match "hitcount"        $ showHitcount s.phantasm.hits
      match "alignment"       $ showAlign s.align
  where
    match = wikiMatch mw
    showAlign (Tuple DB.Neutral DB.Neutral) = "True Neutral"
    showAlign (Tuple a DB.Mad) = show a <> " Madness"
    showAlign (Tuple a b) = show a <> " " <> show b
    showAttr DB.Mankind = "Human"
    showAttr a = show a
    showHitcount 0 = "－"
    showHitcount a = show a
    prId = format $ Formatter { comma: false
                              , before: 3
                              , after: 0
                              , abbreviations: false
                              , sign: false
                              }

testNP :: Tuple DB.Servant MediaWiki -> TestSuite
testNP (Tuple (DB.Servant s) mw) = suite (s.name <> ": NP") do
      suite "Primary Effects" do
          shouldMatch (effects s.phantasm.effect) $ 
              wikiRange mw "effect" (0..6) >>= readEffect
      suite "Overcharge Effects" do 
          shouldMatch (effects s.phantasm.over) $
              wikiRange mw "oceffect" (0..6) >>= readEffect

wikiRanges :: MediaWiki -> Array DB.RangeInfo
wikiRanges (MediaWiki mw) = Array.catMaybes $ go <$> (0..7)
  where 
    go i = do
        from <- Map.lookup ("e" <> show i <> "-lvl1") mw  >>= Array.head
        to   <- Map.lookup ("e" <> show i <> "-lvl10") mw >>= Array.head
        let isPercent = String.contains (Pattern "%") from
            stripFrom = maybeDo (String.stripSuffix $ Pattern "%") from
            stripTo   = maybeDo (String.stripSuffix $ Pattern "%") to
        fromVal <- Number.fromString stripFrom
        toVal   <- Number.fromString stripTo  
        guard $ fromVal /= toVal
        pure $ DB.RangeInfo isPercent fromVal toVal

testSkill :: DB.Active -> MediaWiki -> TestSuite
testSkill skill mw = suite skill.name do
    wikiMatch mw "cooldown1" $ show skill.cd
    shouldMatch (DB.ranges skill.effect) $ wikiRanges mw
    shouldMatch (effects skill.effect) $
        wikiRange mw "effect" (0..7) >>= readEffect

testSkills :: Map String MediaWiki -> DB.Servant -> TestSuite
testSkills skills (DB.Servant s) = suite (s.name <> ": Skills") $ 
    traverse_ go s.actives
  where
    go skill = case Map.lookup skill.name skills of
        Nothing -> test skill.name <<< failure $ "Couldn't find skill"
        Just mw -> testSkill skill mw
