-- | Prints test results all fancy-like.
module Test.Console (runTest, runTestVerbose) where

import StandardLibrary

import Effect.Aff       as Aff
import Effect.Exception as Exception
import Data.Int         as Int
import Data.List        as List

import Data.List (List)
import Test.Unit (TestList, TestSuite, collectResults, countSkippedTests, keepErrors, walkSuite)
import Test.Unit.Console as Console

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
        when (null path) $ Console.print "\n"
        Console.print "\n"
        Console.print $ indent' path
        Console.printLabel label
        Console.print "\n"
        void <<< Console.print $ indent' path
    runSuiteItem path (Right (label : t)) = do
        result <- Aff.attempt t
        void $ case result of
                  (Right _) -> liftEffect do
                      Console.printPass "\x2713"
                      Console.printPass label
                      Console.print " "
                  (Left err) -> liftEffect do
                      Console.printFail "\x2620"
                      Console.printFail label
                      Console.print " "

silently :: TestSuite -> Aff TestList
silently tst = walkSuite runSuiteItem tst
  where
    runSuiteItem path (Left label) = pure unit
    runSuiteItem path (Right (label : t)) = void $ Aff.attempt t

printErrors :: TestList -> Int -> Aff Unit
printErrors tests skCount = do
    results      <- collectResults tests
    let errors    = keepErrors results
        failed    = length errors
        total     = length results
        percent   = append "(" <<< flip append "%)" <<< places 2 $
                    100.0 * Int.toNumber failed / Int.toNumber total
        skMsg     = case skCount of
                        0 -> ""
                        1 -> " (1 test skipped)"
                        i -> " (" <> show i <> " tests skipped)"
    liftEffect case failed of
          0 -> Console.printPass $ "\n\nYorokobe, shounen! 🎉 All "
                                <> show total <> " tests passed" <> skMsg
                                <> ".\n\n"
          1 -> do
              Console.printFail  $ "\n1/" <> show total <> " test failed "
                                <> percent <> skMsg <>":\n\n"
              list errors
          i -> do
              Console.printFail $ "\n" <> show i <> "/" <> show total
                      <> " tests failed " <> percent <> skMsg <> ":\n\n"
              list errors
    where list = traverse_ printItem
          printItem (path : err) = do
              printHeader 0 path
              printError err
              Console.print "\n"
          printHeader level path = case List.uncons path of
              Nothing -> Console.print $ indent level
              Just {head, tail} -> do
                  Console.print $ indent level <> head <> ":\n"
                  printHeader (level + 1) tail
          printError err = do
                Console.printFail $ Exception.message err
                Console.print "\n"

runTestVerbose :: TestSuite -> Aff TestList
runTestVerbose suite = do
    tests <- printLive suite
    printErrors tests (countSkippedTests suite)
    pure tests

runTest :: TestSuite -> Aff TestList
runTest suite = do
    tests <- silently suite
    printErrors tests (countSkippedTests suite)
    pure tests
