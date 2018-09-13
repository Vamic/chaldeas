-- | Prints test results all fancy-like.
module Test.Console (runTest) where

import Prelude
import Operators ((^))

import Effect.Aff (attempt, Aff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.List (List, uncons, length, null)
import Data.Maybe (Maybe(..))
import Test.Unit (TestList, TestSuite, collectResults, countSkippedTests, keepErrors, walkSuite)
import Test.Unit.Console (printFail, printPass, printLabel, print)

import Printing (places)

indent :: Int -> String
indent 0 = mempty
indent n = "  " <> indent (n - 1)

indent' :: forall a. List a -> String
indent' = indent <<< length

printLive :: TestSuite -> Aff TestList
printLive tst = walkSuite runSuiteItem tst
  where
    runSuiteItem path (Left label) = liftEffect do
        when (null path) $ print "\n"
        print "\n"
        print $ indent' path
        printLabel label
        print "\n"
        void <<< print $ indent' path
    runSuiteItem path (Right (label ^ t)) = do
        result <- attempt t
        void $ case result of
                  (Right _) -> liftEffect do
                      printPass "\x2713"
                      printPass label
                      print " "
                  (Left err) -> liftEffect do
                      printFail "\x2620"
                      printFail label
                      print " "

printErrors :: TestList -> Int -> Aff Unit
printErrors tests skCount = do
    results      <- collectResults tests
    let errors    = keepErrors results
        failed    = length errors
        total     = length results
        percent   = append "(" <<< flip append "%)" <<< places 2 $ 
                    100.0 * toNumber failed / toNumber total
        skMsg     = case skCount of
                        0 -> ""
                        1 -> " (1 test skipped)"
                        i -> " (" <> show i <> " tests skipped)"
    liftEffect case failed of
          0 -> printPass $ "\n\nYorokobe, shounen! 🎉 All " <> show total 
                        <> " tests passed" <> skMsg <> ".\n\n"
          1 -> do
              printFail $ "\n1/" <> show total <> " test failed " <> percent 
                       <> skMsg <>":\n\n"
              list errors
          i -> do
              printFail $ "\n" <> show i <> "/" <> show total 
                      <> " tests failed " <> percent <> skMsg <> ":\n\n"
              list errors
    where list = traverse_ printItem
          printItem (path ^ err) = do
              printHeader 0 path
              printError err
              print "\n"
          printHeader level path = case uncons path of
              Nothing -> print $ indent level
              Just {head, tail} -> do
                  print $ indent level <> head <> ":\n"
                  printHeader (level + 1) tail
          printError err = do
                printFail $ message err
                print "\n"

runTest :: TestSuite -> Aff TestList
runTest suite = do
    tests <- printLive suite
    printErrors tests (countSkippedTests suite)
    pure tests
