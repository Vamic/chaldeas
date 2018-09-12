module Test.Main where

import Prelude
import Operators (maybeDo, (:))

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
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Test.Unit (TestSuite, Test, test, success, suite, failure)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTestWith)
import Test.Console (runTest)

import Test.Base (MaybeRank(..), RankedSkill(..), addRank)
import Test.Wiki (Wiki, printBool, wiki, wikiRange, wikiLookup)
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
    show' x    = show x
    msg        = "Scanning " <> show' ce <> " Craft Essences and " 
                 <> show' servant <> " Servants"
    servants   = maybeTake servant DB.servants
    maybeTake :: ∀ a. Int -> Array a -> Array a
    maybeTake (-1)   = identity
    maybeTake limits = Array.take limits

wikiMatch :: Wiki -> String -> String -> TestSuite
wikiMatch mw k obj = test k $ case wikiLookup mw k of
    Just v  -> assert (obj <> " not in [" <> String.joinWith ", " v <> "].") $
               obj `Array.elem` v
    Nothing -> failure $ "Missing property " <> k <> " in " <> show mw 
            <> Maybe.maybe "" (append ": ") (wikiLookup mw "err" >>= Array.head)


wikiHas :: Wiki -> String -> String -> Boolean
wikiHas mw k obj = Maybe.maybe false (Array.elem obj <<< map String.toLower) $ 
                   wikiLookup mw k

testCraftEssence :: Tuple DB.CraftEssence Wiki -> TestSuite
testCraftEssence (Tuple (DB.CraftEssence ce) mw) = suite ce.name do
    match "id"      <<< prId $ Int.toNumber ce.id
    match "maxatk"    $ show ce.stats.max.atk
    match "maxhp"     $ show ce.stats.max.hp
    match "minatk"    $ show ce.stats.base.atk
    match "minhp"     $ show ce.stats.base.hp
    match "rarity"    $ show ce.rarity
    match "imagetype" $ printIcon ce.icon
    match "limited"   $ printBool ce.limited
  where
    match = wikiMatch mw
    prId = format $ Formatter { comma: false
                              , before: 4
                              , after: 0
                              , abbreviations: false
                              , sign: false
                              }

shouldMatch :: ∀ a. Eq a => Show a => Array a -> Array a -> Test
shouldMatch x y = case wiki : db of
    [] : [] -> success
    xs : [] -> failure $ "Missing from Wiki: " <> showAll xs <> "."
    [] : ys -> failure $ "Missing from DB: "   <> showAll ys <> "."
    xs : ys -> failure $ "Missing from Wiki: " <> showAll xs <> "."
                      <> "Missing from DB: "   <> showAll ys <> "."
{-
    test "Wiki" <<< beNull $ Array.nubEq x \\ Array.nubEq y
    test "DB"   <<< beNull $ Array.nubEq y \\ Array.nubEq x
-}
  where
    wiki = Array.nubEq x \\ Array.nubEq y
    db   = Array.nubEq y \\ Array.nubEq x
    showAll = String.joinWith "," <<< map show

testServant :: Tuple DB.Servant Wiki -> TestSuite
testServant (Tuple (DB.Servant s) mw) = suite (s.name <> ": Info") do
    suite "Profile" do
        match "id"            <<< prId $ Int.toNumber s.id
        match "class"           $ show s.class
        match "rarity"          $ show s.rarity
        match "attribute"       $ showAttr s.attr
        match "alignment"       $ showAlign s.align
        match "deathresist"     $ toString s.death
        test "status" $ assert (show $ wikiLookup mw "status") status
    suite "Stats" do
        match "minatk"          $ show s.stats.base.atk
        match "maxatk"          $ show s.stats.max.atk
        match "minhp"           $ show s.stats.base.hp
        match "maxhp"           $ show s.stats.max.hp
        match "grailatk"        $ show s.stats.grail.atk
        match "grailhp"         $ show s.stats.grail.hp
    suite "Hitcounts" do
        match "quickhit"        $ show s.hits.quick
        match "artshit"         $ show s.hits.arts
        match "busterhit"       $ show s.hits.buster
        match "extrahit"        $ show s.hits.ex
    suite "Generation" do
        match "starabsorption"  $ show s.gen.starWeight
        match "stargeneration"  $ toString s.gen.starRate
        match "npchargeattack"  $ toString s.gen.npAtk
        match "npchargedefense" $ show s.gen.npDef
    suite "Deck" do
        match "commandcard"     $ show s.deck
        match "icon"            $ show s.phantasm.card
        match "hitcount"        $ showHitcount s.phantasm.hits
  where
    match = wikiMatch mw
    showAlign[] = "Changes per Master"
    showAlign [DB.Neutral, DB.Neutral] = "True Neutral"
    showAlign [x, DB.Mad] = show x <> " Madness"
    showAlign xs = String.joinWith " " $ show <$> xs
    showAttr DB.Mankind = "Human"
    showAttr x = show x
    showHitcount 0 = "－"
    showHitcount x = show x
    hasStatus = wikiHas mw "status"
    status
      | s.free && s.limited = hasStatus "welfare"
      | s.limited           = hasStatus "limited"
      | otherwise     = not $ hasStatus "welfare" 
                           || hasStatus "limited"
    prId = format $ Formatter { comma: false
                              , before: 3
                              , after: 0
                              , abbreviations: false
                              , sign: false
                              }

testNP :: Tuple DB.Servant Wiki -> TestSuite
testNP (Tuple (DB.Servant s) mw) = suite (s.name <> ": NP") do
      test "Primary Effects" do
          shouldMatch (effects s.phantasm.effect) $ 
              wikiRange mw "effect" (0..6) >>= readEffect
      test "Overcharge Effects" do 
          shouldMatch (effects s.phantasm.over) $
              wikiRange mw "oceffect" (0..6) >>= readEffect

wikiRanges :: Wiki -> Array DB.RangeInfo
wikiRanges mw = Array.catMaybes $ go <$> (0..7)
  where 
    go i = do
        from <- wikiLookup mw ("e" <> show i <> "-lvl1") >>= Array.head
        to   <- wikiLookup mw ("e" <> show i <> "-lvl10") >>= Array.head
        let isPercent = String.contains (Pattern "%") from
            stripFrom = maybeDo (String.stripSuffix $ Pattern "%") from
            stripTo   = maybeDo (String.stripSuffix $ Pattern "%") to
        fromVal <- Number.fromString stripFrom
        toVal   <- Number.fromString stripTo  
        guard $ fromVal /= toVal
        pure $ DB.RangeInfo isPercent fromVal toVal

testSkill :: DB.Skill -> Wiki -> TestSuite
testSkill skill mw = suite skill.name do
    wikiMatch mw "cooldown1" $ show skill.cd
    test "values" do
        shouldMatch (DB.ranges skill.effect) $ wikiRanges mw
    test "effects" do
        shouldMatch (effects skill.effect) $
        wikiRange mw "effect" (0..7) >>= readEffect

testSkills :: Map RankedSkill Wiki -> DB.Servant -> TestSuite
testSkills skills s'@(DB.Servant s) = suite (s.name <> ": Skills") <<<
    traverse_ go $ snd <$> skillRanks s'
  where
    go :: RankedSkill -> TestSuite
    go ranked@(RankedSkill skill _) = case Map.lookup ranked skills of
        Nothing -> test skill.name <<< failure $ "Couldn't find skill"
        Just mw -> testSkill skill mw
