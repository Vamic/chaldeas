-- | Retrieves information from [GrandOrder.Wiki](grandorder.wiki)
-- | in the form of maps from Strings (property names)
-- | to String Arrays (list of entries separated by newlines).
module Test.Wiki
  ( Wiki(..)
  , printBool
  , wikiRange
  , wiki, wikiLookup
  ) where

import StandardLibrary

import Affjax                   as Affjax
import Global.Unsafe            as Global
import Data.Map                 as Map
import Affjax.ResponseFormat    as ResponseFormat
import Data.String              as String
import Data.String.Regex        as Regex
import Data.String.Regex.Flags  as Flags
import Data.String.Regex.Unsafe as Unsafe

import Test.Base (MaybeRank)
import Test.Parse (translate)

wikiLink :: Regex
wikiLink = Unsafe.unsafeRegex """\[\[[:\|\]]+\|([:\]]+)\]\]""" Flags.global

wikiTag :: Regex
wikiTag = Unsafe.unsafeRegex """<[:\s>]+>""" Flags.global

wikiRoot :: String
wikiRoot = "https://grandorder.wiki/index.php?action=raw&title="

newtype Wiki = Wiki (Map String (Array String))
instance _0_ :: Show Wiki where
    show (Wiki mw) = show mw

wikiLookup :: Wiki -> String -> Maybe (Array String)
wikiLookup (Wiki mw) = flip Map.lookup mw

toWiki :: String -> MaybeRank -> Wiki
toWiki text rank =
    Wiki <<< Map.fromFoldable <<< reverse <<< mapMaybe parseEntry <<<
    String.split (Pattern "|") <<< Regex.replace wikiLink "$1" $
    fromMaybe text do
        fromStart <- splitAny After [show rank <> "="]     text
        toEnd     <- splitAny Before ["|-|","/onlyinclude"] fromStart
        pure toEnd
  where
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

wiki :: ∀ a. Show a => Tuple a MaybeRank -> Aff (Tuple a Wiki)
wiki (x : mRank) = Tuple x <<< wikify <<< _.body <$> visitUrl (show x)
  where
    visitUrl = Affjax.get ResponseFormat.string <<< append wikiRoot <<< 
               Global.unsafeEncodeURIComponent <<< translate
    wikify (Right obj) = toWiki obj mRank
    wikify (Left err)  = Wiki $ Map.fromFoldable
                         [("err" : [Affjax.printResponseFormatError err])]

printBool :: Boolean -> String
printBool true  = "Yes"
printBool false = "No"

wikiRange :: Wiki -> String -> Array Int -> Array String
wikiRange (Wiki mw) k range = range >>= fromMaybe empty <<<
                              flip Map.lookup mw <<< append k <<< show
