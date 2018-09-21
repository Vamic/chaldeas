-- | Retrieves information from [GrandOrder.Wiki](grandorder.wiki)
-- | in the form of maps from Strings (property names)
-- | to String Arrays (list of entries separated by newlines).
module Test.Wiki
  ( Wiki(..), WikiList, wikiLink
  , printBool
  , range
  , scrape
  , lookup, lookupArray
  , fromString
  ) where

import StandardLibrary

import Affjax                   as Affjax
import Global.Unsafe            as Global
import Data.Map                 as Map
import Partial.Unsafe           as Partial
import Affjax.ResponseFormat    as ResponseFormat
import Data.Either              as Either
import Data.Set                 as Set
import Data.String              as String
import Data.String.Regex        as Regex
import Data.String.Regex.Flags  as Flags
import Data.String.Regex.Unsafe as Unsafe

import Data.List (List)
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)

import Test.PairMap  as PairMap

import Test.Base (MaybeRank)
import Test.Parse (translate)
import Test.PairMap (PairMap)

type WikiList a = PairMap a MaybeRank Wiki

wikiLink :: Regex
wikiLink = Unsafe.unsafeRegex """\[\[[^\|\]]+\|([^\]]+)\]\]""" Flags.global

wikiTag :: Regex
wikiTag = Unsafe.unsafeRegex """<[^\s>]+>""" Flags.global

wikiRoot :: String
wikiRoot = "https://grandorder.wiki/index.php?action=raw&title="

data Wiki = Wiki (Map String (Array String)) (Map String (Array (Array String)))
instance _0_ :: Show Wiki where
    show (Wiki mw _) = show mw

lookup :: Wiki -> String -> Maybe (Array String)
lookup (Wiki mw _) = flip Map.lookup mw

lookupArray :: Wiki -> String -> Maybe (Array (Array String))
lookupArray (Wiki _ arrays) = flip Map.lookup arrays

fromString :: String -> MaybeRank -> Wiki
fromString text rank = Wiki fields arrays
  where
    fields = Map.fromFoldable <<< reverse <<< mapMaybe parseEntry <<<
             String.split (Pattern "|") <<< Regex.replace wikiLink "$1" $
             fromMaybe text do
                 fromStart <- splitAny After [show rank <> "="]     text
                 toEnd     <- splitAny Before ["|-|","/onlyinclude"] fromStart
                 pure toEnd
    parseEntry entry = do
        assignment         <- String.indexOf (Pattern "=") entry
        let {before, after} = String.splitAt assignment entry
            afterLines      = String.split (Pattern "<br/>") after
        guard <<< not $ String.null before
        pure <<< Tuple (String.trim $ String.toLower before) <<<
        filter (not <<< String.null) $
        maybeDo (String.stripPrefix $ Pattern "#tip-text:") <<< String.trim <<<
        filterOut (Pattern "%,{}[]()'") <<< Regex.replace wikiTag " " <<<
        maybeDo (splitAny After  ["EN:"]) <<<
        maybeDo (splitAny Before ["}}","/"]) <<<
        maybeDo (String.stripPrefix $ Pattern "=") <$> afterLines
    arrays = Map.fromFoldable $ array text
    array subtext = fromMaybe [] do
        headerStart   <- String.indexOf (Pattern "== ") subtext
        let headerFrom = String.splitAt headerStart subtext
        headerEnd     <- String.indexOf (Pattern " ==") headerFrom.after
        let headerTo   = String.splitAt headerEnd headerFrom.after
            header     = String.trim $ filterOut (Pattern "=") headerTo.before
        sectionEnd    <- String.indexOf (Pattern "}}") headerTo.after
        let section    = String.splitAt sectionEnd headerTo.after
        pure <<< cons (header : parseRows section.before) $ array section.after
    parseRows = fromMaybe [] <<< parseRow 1 <<< String.split (Pattern "\n")
    parseRow row lines = do
        cols <- parseCol row 1 lines
        pure <<< maybe [cols] (cons cols) $ parseRow (row+1) lines
    parseCol row col lines = do
        entry <- find (eq ("|" <> show row <> show col) <<< String.take 3) lines
        assignment <- String.indexOf (Pattern "=") entry
        let {after} = String.splitAt assignment entry
            val     = String.trim $ filterOut (Pattern "=") after
        pure <<< maybe [val] (cons val) $ parseCol row (col+1) lines

data Side = Before | After

splitAny :: ∀ m. Foldable m => Monad m
         => Side -> m String -> String -> Maybe String
splitAny side xs s = go <$> indices
  where
    go {len, i} = case side of
        Before -> _.before $ String.splitAt i s
        After  -> String.drop len <<< _.after $ String.splitAt i s
    indices = oneOf do
        pattern <- xs
        pure $ { len: String.length pattern, i: _ }
            <$> String.indexOf (Pattern pattern) s

scrapeWithRank :: ∀ a f. Ord a => Foldable f => Functor f
       => (a -> String) -> a -> f MaybeRank -> Aff (PairMap a MaybeRank Wiki)
scrapeWithRank show' x ranks = PairMap.fromFoldable <<< wikify <<<
           Partial.unsafePartial Either.fromRight <<<  
           _.body <$> visitUrl (show' x)
  where
    visitUrl    = Affjax.get ResponseFormat.string <<< append wikiRoot <<< 
                  Global.unsafeEncodeURIComponent <<< translate
    wikify text = ((x : _) &&& fromString text) <$> ranks
printBool :: Boolean -> String
printBool true  = "Yes"
printBool false = "No"

scrape :: ∀ a. Ord a => (a -> String) -> Map a (Set MaybeRank) 
       -> Aff (PairMap a MaybeRank Wiki)
scrape show' xs = PairMap.unions <$> traverse scrapeOne xs'
  where
    xs' :: List (a : Set MaybeRank)
    xs' = Map.toUnfoldableUnordered xs
    scrapeOne (x : ranks) = scrapeWithRank show' x 
                            (Set.toUnfoldable ranks :: List MaybeRank)

range :: Wiki -> String -> Array Int -> Array String
range (Wiki mw _) k xs = xs >>= fromMaybe empty <<<
                         flip Map.lookup mw <<< append k <<< show
