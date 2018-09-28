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

module Test.Main (main) where

import StandardLibrary
import Effect.Aff             as Aff
import Effect.Console         as Console
import Data.Number.Format     as Format
import Data.Int               as Int
import Test.Unit.Main         as Main
import Data.Number            as Number
import Node.Yargs.Setup       as Setup
import Data.String            as String
import Test.Console           as Test
import Node.Yargs.Applicative as Yargs

import Data.Formatter.Number (Formatter(..), format)
import Test.Unit (TestSuite, test, success, suite, failure)
import Test.Unit.Assert (assert)

import Test.Data.Multimap as Multimap
import Test.Data.PairMap  as PairMap
import Test.Wiki     as Wiki

import Database (Alignment(..), Ascension(..), Attribute(..), CraftEssence(..), Material, RangeInfo(..), Reinforcement(..), Servant(..), Skill, craftEssences, ranges, servants)

import Test.Data.MaybeRank (MaybeRank(..))
import Test.Parse (effects, npRank, printIcon, readEffect, skillRanks)
import Test.Wiki (Wiki, WikiList)

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
    ceWiki    <- Wiki.scrape show <<< Multimap.fromFoldable $
                 (_ : Unranked) <$> (maybeTake ce craftEssences)
    servWiki  <- Wiki.scrape show <<< Multimap.fromFoldable $ servs >>= servNPs
    skillWiki <- Wiki.scrape _.name <<< Multimap.unions $ skillRanks <$> servs
    Main.runTestWith Test.runTest do
        traverse_ testCraftEssence $ PairMap.flatten ceWiki
        traverse_ testServant      $ PairMap.flatten servWiki
        traverse_ (testSkills skillWiki) servs
  where
    show' (-1) = "all"
    show' x    = show x
    msg        = "Scanning " <> show' ce <> " Craft Essences and "
                 <> show' servant <> " Servants"
    servs     = maybeTake servant servants
    servNPs x = (x : _) <$> [Unranked, npRank x]
    maybeTake :: ∀ a. Int -> Array a -> Array a
    maybeTake (-1)   = identity
    maybeTake limits = take limits

testCraftEssence :: CraftEssence : MaybeRank : Wiki -> TestSuite
testCraftEssence (CraftEssence ce : _ : mw) = suite ce.name do
    match "id"      <<< prId $ Int.toNumber ce.id
    match "maxatk"    $ show ce.stats.max.atk
    match "maxhp"     $ show ce.stats.max.hp
    match "minatk"    $ show ce.stats.base.atk
    match "minhp"     $ show ce.stats.base.hp
    match "rarity"    $ show ce.rarity
    match "imagetype" $ printIcon ce.icon
    match "limited"   $ Wiki.printBool ce.limited
  where
    match = wikiMatch mw
    prId = format $ Formatter { comma: false
                              , before: 4
                              , after: 0
                              , abbreviations: false
                              , sign: false
                              }

testServant :: Servant : MaybeRank : Wiki -> TestSuite
testServant (Servant s : Unranked : mw) = suite (s.name <> " → Info") do
    suite "Profile" do
        match "id"            <<< prId $ Int.toNumber s.id
        match "class"           $ show s.class
        match "rarity"          $ show s.rarity
        match "attribute"       $ showAttr s.attr
        match "alignment"       $ showAlign s.align
        match "deathresist"     $ Format.toString s.death
        test "status" $ assert (show $ Wiki.lookup mw "status") status
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
    matchArray "Ascension"      $ showAscension s.ascendUp
    matchArray "Skill Reinforcement" $ showReinforcement s.skillUp
  where
    match = wikiMatch mw
    matchArray = wikiMatchArray mw
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
testServant (Servant s : _ : mw) = suite (s.name <> " → NP") do
      shouldEq "Name"        s.phantasm.name $ npInfo 0
      shouldEq "Description" s.phantasm.desc $ npInfo 1
      shouldMatch "Primary Effects" (effects s.phantasm.effect) $
          Wiki.range mw "effect" (0..6) >>= readEffect
      shouldMatch "Overcharge Effects" (effects s.phantasm.over) $
          Wiki.range mw "oceffect" (0..6) >>= readEffect
      shouldMatch "Primary Values" (ranges s.phantasm.effect) $
          wikiRanges mw "leveleffect" false
      shouldMatch "Overcharge Values" (ranges s.phantasm.over) $
          wikiRanges mw "overcharge" false
  where
    cleanup      = String.replaceAll (Pattern " - ") (Replacement "—")
    npTitle      = map cleanup <$> Wiki.lookup mw "skillname"
    npInfo i     = fromMaybe "" $ npTitle >>= flip index i
    shouldEq label x y
      | x == y = test label success
      | otherwise = suite label do
          test "From Data" $ failure x
          test "From Wiki" $ failure y
    

testSkills :: WikiList Skill -> Servant -> TestSuite
testSkills skills s'@(Servant s) = suite (s.name <> " → Skills") $
    traverse_ go rankedSkills
  where
    rankedSkills :: Array (Skill : MaybeRank)
    rankedSkills = Multimap.toUnfoldable $ skillRanks s'
    go (skill : rank) = case PairMap.lookup skill rank skills of
        Nothing -> test skill.name <<< failure $ "Couldn't find skill"
        Just mw -> testSkill skill mw

testSkill :: Skill -> Wiki -> TestSuite
testSkill skill mw = suite skill.name do
    wikiMatch mw "cooldown1" $ show skill.cd
    shouldMatch "values" (ranges skill.effect) $ wikiRanges mw "e" true
    shouldMatch "effects" (effects skill.effect) $ 
        Wiki.range mw "effect" (0..7) >>= readEffect

wikiMatch :: Wiki -> String -> String -> TestSuite
wikiMatch mw k obj = test k $ case cleanup <$> Wiki.lookup mw k of
    Nothing -> failure $ "Missing property " <> k <> " in " <> show mw
                       <> maybe "" (append ": ") (Wiki.lookup mw "err" >>= head)
    Just v  -> assert (obj <> " not in [" <> String.joinWith ", " v <> "].") $
               obj `elem` v
  where
    cleanup = map <<< filterOut $ Pattern "%,{}[]()'"

wikiHas :: Wiki -> String -> String -> Boolean
wikiHas mw k obj = maybe false (elem obj <<< map String.toLower) $
                   Wiki.lookup mw k

wikiMatchArray :: Wiki -> String -> Array (Array String) -> TestSuite
wikiMatchArray _ k [] = test k $ success
wikiMatchArray mw k obj = case Wiki.lookupArray mw k of
    Nothing -> test k <<< failure $ "Missing property " <> k
    Just vs -> suite k $
        for_ lengthRange $ \i -> 
            let label = "Level " <> show (i + 1)
            in case vs !! i of
                  Nothing   -> test label $ failure "Level missing from Wiki"
                  Just vsAt -> shouldMatch label (fromMaybe [] $ obj !! i) vsAt
  where
    lengthRange = 0 .. (length obj - 1)

showMaterials :: Array (Array (Material : Int)) -> Array (Array String)
showMaterials = map (map go)
  where
    go (mat : count) = show mat <> "*" <> show count

showAscension :: Ascension -> Array (Array String)
showAscension (Clear _ _ _ _)     = []
showAscension (Welfare x)         = replicate 4 [x <> "*1"]
showAscension (Ascension a b c d) = showMaterials [a, b, c, d]

showReinforcement :: Reinforcement -> Array (Array String)
showReinforcement (Reinforcement a b c d e f g h)
    = showMaterials [a, b, c, d, e, f, g, h]

shouldMatch :: ∀ a. Eq a => Show a => String -> Array a -> Array a -> TestSuite
shouldMatch label x y = suite label do
    test "Missing from Wiki" <<< diffTest $ x' \\ y'
    test "Missing from Data" <<< diffTest $ y' \\ x'
  where
    showAll = String.joinWith ", " <<< map show
    x' = nubEq x
    y' = nubEq y
    diffTest [] = success
    diffTest xs = failure $ showAll xs <> ""

wikiRanges :: Wiki -> String -> Boolean -> Array RangeInfo
wikiRanges mw pref isSkill = mapMaybe go (0..7)
  where
    showI 1
      | isSkill   = show 1
      | otherwise = ""
    showI i = show i
    maxLvl
      | isSkill   = 10
      | otherwise = 5
    go i = do
        from <- Wiki.lookup mw (pref <> showI i <> "-lvl1") >>= head
        to   <- Wiki.lookup mw (pref <> showI i <> "-lvl" <> show maxLvl) >>= 
                head
        let isPercent = String.contains (Pattern "%") from
            stripFrom = maybeDo (String.stripSuffix $ Pattern "%") from
            stripTo   = maybeDo (String.stripSuffix $ Pattern "%") to
        fromVal <- Number.fromString stripFrom
        toVal   <- Number.fromString stripTo
        guard $ fromVal /= toVal
        pure $ RangeInfo isPercent fromVal toVal
