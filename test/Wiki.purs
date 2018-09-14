-- | Retrieves information from [GrandOrder.Wiki](grandorder.wiki)
-- | in the form of maps from Strings (property names) 
-- | to String Arrays (list of entries separated by newlines).
module Test.Wiki
  ( Wiki(..)
  , printBool
  , wikiRange
  , wiki, wikiLookup
  ) where

import Prelude
import Operators ((^), filterOut, maybeDo)

import Affjax                   as AX
import Data.Array               as Array
import Global.Unsafe            as Global
import Data.Map                 as Map
import Data.Maybe               as Maybe
import Affjax.ResponseFormat    as ResponseFormat
import Data.String              as String
import Data.String.Regex        as Regex
import Data.String.Regex.Flags  as Flags
import Data.String.Regex.Unsafe as Unsafe

import Control.MonadZero (guard)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, oneOf)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.String (Pattern(..))
import Data.String.Regex (Regex)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)

import Test.Base (MaybeRank)
import Test.Parse (translate)

wikiLink :: Regex
wikiLink = Unsafe.unsafeRegex """\[\[[^\|\]]+\|([^\]]+)\]\]""" Flags.global

wikiTag :: Regex
wikiTag = Unsafe.unsafeRegex """<[^\s>]+>""" Flags.global

wikiRoot :: String
wikiRoot = "https://grandorder.wiki/index.php?action=raw&title="

newtype Wiki = Wiki (Map String (Array String))
instance _0_ :: Show Wiki where
    show (Wiki mw) = show mw

wikiLookup :: Wiki -> String -> Maybe (Array String)
wikiLookup (Wiki mw) = flip Map.lookup mw

toWiki :: String -> MaybeRank -> Wiki
toWiki text rank = 
    Wiki <<< Map.fromFoldable <<< Array.reverse <<< Array.catMaybes <<< 
    map parseEntry <<< String.split (Pattern "|") <<< 
    Regex.replace wikiLink "$1" $ Maybe.fromMaybe text do 
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
        Array.filter (not <<< String.null) $ 
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
    go (len ^ i) = case side of
        Before -> _.before $ String.splitAt i s
        After  -> String.drop len <<< _.after $ String.splitAt i s
    indices = oneOf do
        pattern <- xs
        pure $ Tuple (String.length pattern) 
            <$> String.indexOf (Pattern pattern) s

wiki :: ∀ a. Show a => Tuple MaybeRank a -> Aff (Tuple a Wiki)
wiki (mRank ^ x) = Tuple x <<< wikify <<< 
                   _.body <$> AX.get ResponseFormat.string encode
  where
    wikify (Right obj) = toWiki obj mRank 
    wikify (Left err)  = Wiki $ Map.fromFoldable 
                         [("err" ^ [AX.printResponseFormatError err])]
    encode  = append wikiRoot <<< Global.unsafeEncodeURIComponent <<< 
              translate $ show x

printBool :: Boolean -> String
printBool true  = "Yes"
printBool false = "No"

wikiRange :: Wiki -> String -> Array Int -> Array String
wikiRange (Wiki mw) k range = join <<< Array.catMaybes $ 
                                   flip Map.lookup mw <<< append k <<< 
                                   show <$> range
