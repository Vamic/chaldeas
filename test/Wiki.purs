module Test.Wiki
  ( Wiki(..), toWiki
  , printBool
  , wikiRange
  , wiki, wikiLookup
  ) where

import Prelude
import Operators (filterOut, maybeDo)

import Affjax                   as AX
import Data.Array               as Array
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

import Test.Base (MaybeRank, indices)
import Test.Parse (translate)

wikiLink :: Regex
wikiLink = Unsafe.unsafeRegex """\[\[[^\|\]]+\|([^\]]+)\]\]""" mempty

unLink :: String -> String
unLink = Regex.replace wikiLink "$1"

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
    map parseEntry <<< String.split (Pattern "|") <<< unLink $
    Maybe.fromMaybe text do 
        start <- String.indexOf (Pattern $ show rank <> "=") text
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
    sanitize = filterOut ['%',',','{','}','[',']','(',')','\'']

wiki :: ∀ a. Show a => Tuple MaybeRank a -> Aff (Tuple a Wiki)
wiki (Tuple mRank x) = Tuple x <<< wikify <<< 
                       _.body <$> AX.get ResponseFormat.string encode
  where
    wikify (Right obj) = toWiki obj mRank 
    wikify (Left err)  = Wiki $ Map.fromFoldable 
                         [Tuple "err" [AX.printResponseFormatError err]]
    encode  = append wikiRoot <<< Global.unsafeEncodeURIComponent <<< 
              translate $ show x

printBool :: Boolean -> String
printBool true  = "Yes"
printBool false = "No"

wikiRange :: Wiki -> String -> Array Int -> Array String
wikiRange (Wiki mw) k range = join <<< Array.catMaybes $ 
                                   flip Map.lookup mw <<< append k <<< 
                                   show <$> range
