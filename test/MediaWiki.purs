module Test.MediaWiki 
  ( MediaWiki(..)
  , printBool
  , wikiRange
  , wiki, wikiLookup
  ) where

import Prelude
import Operators (maybeDo)

import Affjax                   as AX
import Data.Array               as Array
import Data.String.CodeUnits    as CodeUnits
import Global.Unsafe            as Global
import Data.Map                 as Map
import Data.Maybe               as Maybe
import Affjax.ResponseFormat    as ResponseFormat
import Data.String              as String
import Data.String.Regex        as Regex
import Data.String.Regex.Unsafe as Unsafe

import Control.MonadZero (guard)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.String (Pattern(..))
import Data.String.Regex (Regex)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)

import Test.Base (class CleanShow, cleanShow, MaybeRank, showRank, unRank, indices)
import Test.Parse (translate)

wikiLink :: Regex
wikiLink = Unsafe.unsafeRegex """\[\[[^\|\]]+\|([^\]]+)\]\]""" mempty

unLink :: String -> String
unLink = Regex.replace wikiLink "$1"

wikiRoot :: String
wikiRoot = "https://grandorder.wiki/index.php?action=raw&title="

newtype MediaWiki = MediaWiki (Map String (Array String))
instance _0_ :: Show MediaWiki where
  show (MediaWiki mw) = show mw

wikiLookup :: MediaWiki -> String -> Maybe (Array String)
wikiLookup (MediaWiki mw) = flip Map.lookup mw

toWiki :: String -> MaybeRank -> MediaWiki
toWiki text mRank = 
    MediaWiki <<< Map.fromFoldable <<< Array.reverse <<< Array.catMaybes <<< 
    map parseEntry <<< String.split (Pattern "|") <<< unLink $
    Maybe.fromMaybe text do 
        rank  <- showRank mRank
        start <- String.indexOf (Pattern $ "Rank " <> rank <> "=") text
        let {after} = String.splitAt start text
        end <- indices ["|-|","/onlyinclude"] after
        let {before} = String.splitAt end after
        pure before
  where 
    parseEntry entry = do
        assignment         <- String.indexOf (Pattern "=") entry
        let {before, after} = String.splitAt assignment entry
            afterLines      = String.split (Pattern "<br/>") after
        guard <<< not $ String.null before
        pure <<< Tuple (String.trim $ String.toLower before) <<< 
        Array.filter (not <<< String.null) $ 
        maybeDo (String.stripPrefix $ Pattern "#tip-text:") <<<
        String.trim <<< sanitize <<< maybeDo untilBraces <<< 
        maybeDo (String.stripPrefix $ Pattern "=") <$> afterLines
    untilBraces line = _.before <<< 
                      flip String.splitAt line <$> indices ["}}","/"] line
    sanitize = CodeUnits.fromCharArray <<< Array.filter legal <<<         
               CodeUnits.toCharArray
      where
        legal '%'  = false
        legal ','  = false
        legal '{'  = false
        legal '['  = false
        legal ']'  = false
        legal '('  = false
        legal ')'  = false
        legal '\'' = false
        legal _    = true

wiki :: ∀ a. CleanShow a => Tuple MaybeRank a -> Aff (Tuple a MediaWiki)
wiki (Tuple mRank a) = wikify <<< _.body <$> AX.get ResponseFormat.string encode
  where
    wikify (Left err)  = Tuple a <<< MediaWiki $ Map.fromFoldable 
                         [Tuple "err" [AX.printResponseFormatError err]]
    wikify (Right obj) = Tuple a $ toWiki obj mRank 
    name    = cleanShow a
    encode  = append wikiRoot <<< Global.unsafeEncodeURIComponent <<< 
              translate $ unRank mRank name

printBool :: Boolean -> String
printBool true  = "Yes"
printBool false = "No"

wikiRange :: MediaWiki -> String -> Array Int -> Array String
wikiRange (MediaWiki mw) k range = join <<< Array.catMaybes $ 
                                   flip Map.lookup mw <<< append k <<< 
                                   show <$> range
