-- | Tests the database against [GrandOrder.Wiki](grandorder.wiki).
-- |
-- | When running the tests, the following command line arguments are accepted:
-- | `--ce <amount>` to limit the number of Craft Essences tested,
-- | `--s <amount>` to limit the number of Servants tested.
-- |
-- | Currently tested:
-- |
-- | Craft Essences:
-- | id, max atk, max hp, min atk, min hp, rarity, icon type, limited
-- |
-- | Servants:
-- | NP effects, NP overcharge effects, NP commandcard, NP icon, NP hitcount,
-- | id, class, rarity, attribute, alignment, death resist,
-- | status (limited/welfare/etc.),
-- | min atk, max atk, min hp, max hp, grail atk, grail hp,
-- | quick hitcount, arts hitcount, buster hitcount, extra hitcount,
-- | star absorption, star generation, np charge/attack, np charge/defense
-- |
-- | Skills:
-- | cooldown, value ranges, effects

module Test.Main where

import StandardLibrary
import Effect.Aff             as Aff
import Effect.Console         as Console
import Data.Number.Format     as Format
import Data.Int               as Int
import Test.Unit.Main         as Main
import Data.Map               as Map
import Data.Number            as Number
import Node.Yargs.Setup       as Setup
import Data.String            as String
import Test.Console           as Test
import Node.Yargs.Applicative as Yargs

import Data.Formatter.Number (Formatter(..), format)
import Test.Unit (TestSuite, Test, test, success, suite, failure)
import Test.Unit.Assert (assert)

import Database (Alignment(..), Attribute(..), CraftEssence(..), RangeInfo(..), Servant(..), Skill, craftEssences, ranges, servants)

import Test.Base (MaybeRank(..), RankedSkill(..), addRank)
import Test.Wiki (Wiki, printBool, wiki, wikiLookup, wikiRange)
import Test.Parse (effects, npRank, printIcon, readEffect, skillRanks)

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
    skills <- map Map.fromFoldable <<< traverse wiki <<< nubEq $
              servs >>= skillRanks
    stats  <- traverse (wiki <<< flip Tuple Unranked) servs
    nps    <- traverse (wiki <<< addRank npRank) servs
    ces    <- traverse (wiki <<< flip Tuple Unranked)
              (maybeTake ce craftEssences)
    Main.runTestWith Test.runTest do
        traverse_ (testSkills skills) servs
        traverse_ testServant stats
        traverse_ testNP nps
        traverse_ testCraftEssence ces
  where
    show' (-1) = "all"
    show' x    = show x
    msg        = "Scanning " <> show' ce <> " Craft Essences and "
                 <> show' servant <> " Servants"
    servs     = maybeTake servant servants
    maybeTake :: ∀ a. Int -> Array a -> Array a
    maybeTake (-1)   = identity
    maybeTake limits = take limits

wikiMatch :: Wiki -> String -> String -> TestSuite
wikiMatch mw k obj = test k $ case wikiLookup mw k of
    Just v  -> assert (obj <> " not in [" <> String.joinWith ", " v <> "].") $
               obj `elem` v
    Nothing -> failure $ "Missing property " <> k <> " in " <> show mw
                       <> maybe "" (append ": ") (wikiLookup mw "err" >>= head)


wikiHas :: Wiki -> String -> String -> Boolean
wikiHas mw k obj = maybe false (elem obj <<< map String.toLower) $
                   wikiLookup mw k

testCraftEssence :: Tuple CraftEssence Wiki -> TestSuite
testCraftEssence (CraftEssence ce ^ mw) = suite ce.name do
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
shouldMatch x y = case (nubEq x \\ nubEq y ^ nubEq y \\ nubEq x) of
    ([] ^ []) -> success
    (xs ^ []) -> failure $ "Missing from Wiki: " <> showAll xs <> "."
    ([] ^ ys) -> failure $ "Missing from DB: "   <> showAll ys <> "."
    (xs ^ ys) -> failure $ "Missing from Wiki: " <> showAll xs <> ". \
                           \Missing from DB: "   <> showAll ys <> "."
  where
    showAll = String.joinWith ", " <<< map show

testServant :: Tuple Servant Wiki -> TestSuite
testServant (Servant s ^ mw) = suite (s.name <> ": Info") do
    suite "Profile" do
        match "id"            <<< prId $ Int.toNumber s.id
        match "class"           $ show s.class
        match "rarity"          $ show s.rarity
        match "attribute"       $ showAttr s.attr
        match "alignment"       $ showAlign s.align
        match "deathresist"     $ Format.toString s.death
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
        match "stargeneration"  $ Format.toString s.gen.starRate
        match "npchargeattack"  $ Format.toString s.gen.npAtk
        match "npchargedefense" $ show s.gen.npDef
    suite "Deck" do
        match "commandcard"     $ show s.deck
        match "icon"            $ show s.phantasm.card
        match "hitcount"        $ showHitcount s.phantasm.hits
  where
    match = wikiMatch mw
    showAlign[] = "Changes per Master"
    showAlign [Neutral, Neutral] = "True Neutral"
    showAlign [x, Mad] = show x <> " Madness"
    showAlign xs = String.joinWith " " $ show <$> xs
    showAttr Mankind = "Human"
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

testNP :: Tuple Servant Wiki -> TestSuite
testNP (Servant s ^ mw) = suite (s.name <> ": NP") do
      test "Primary Effects" do
          shouldMatch (effects s.phantasm.effect) $
              wikiRange mw "effect" (0..6) >>= readEffect
      test "Overcharge Effects" do
          shouldMatch (effects s.phantasm.over) $
              wikiRange mw "oceffect" (0..6) >>= readEffect

wikiRanges :: Wiki -> Array RangeInfo
wikiRanges mw = mapMaybe go (0..7)
  where
    go i = do
        from <- wikiLookup mw ("e" <> show i <> "-lvl1") >>= head
        to   <- wikiLookup mw ("e" <> show i <> "-lvl10") >>= head
        let isPercent = String.contains (Pattern "%") from
            stripFrom = maybeDo (String.stripSuffix $ Pattern "%") from
            stripTo   = maybeDo (String.stripSuffix $ Pattern "%") to
        fromVal <- Number.fromString stripFrom
        toVal   <- Number.fromString stripTo
        guard $ fromVal /= toVal
        pure $ RangeInfo isPercent fromVal toVal

testSkill :: Skill -> Wiki -> TestSuite
testSkill skill mw = suite skill.name do
    wikiMatch mw "cooldown1" $ show skill.cd
    test "values" do
        shouldMatch (ranges skill.effect) $ wikiRanges mw
    test "effects" do
        shouldMatch (effects skill.effect) $
        wikiRange mw "effect" (0..7) >>= readEffect

testSkills :: Map RankedSkill Wiki -> Servant -> TestSuite
testSkills skills s'@(Servant s) = suite (s.name <> ": Skills") <<<
    traverse_ go $ fst <$> skillRanks s'
  where
    go :: RankedSkill -> TestSuite
    go ranked@(RankedSkill skill _) = case Map.lookup ranked skills of
        Nothing -> test skill.name <<< failure $ "Couldn't find skill"
        Just mw -> testSkill skill mw
