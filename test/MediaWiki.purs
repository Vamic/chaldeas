module Test.MediaWiki 
  ( class CleanShow, cleanShow
  , MediaWiki(..)
  , wikiRange
  , wiki
  ) where

import Prelude

import Affjax                   as AX
import Data.Array               as Array
import Data.String.CodeUnits    as CodeUnits
import Database                 as DB
import Global.Unsafe            as Global
import Data.Map                 as Map
import Data.Maybe               as Maybe
import Affjax.ResponseFormat    as ResponseFormat
import Data.String              as String
import Data.String.Regex        as Regex
import Data.String.Regex.Unsafe as Unsafe

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String.Regex (Regex)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)

import Test.Parse (translate)

wikiLink :: Regex
wikiLink = Unsafe.unsafeRegex "\\[\\[[^\\|]+\\|([^\\]]+)\\]\\]" mempty

unLink :: String -> String
unLink = Regex.replace wikiLink "$1"

-- | Shows strings without quoting them.
class CleanShow a where
    cleanShow :: a -> String
instance _0_ :: CleanShow String where
    cleanShow = identity
instance _1_ :: CleanShow DB.Servant where
    cleanShow = show
instance _2_ :: CleanShow DB.CraftEssence where
    cleanShow = show
instance _3_ :: CleanShow DB.ActiveEffect where
    cleanShow = show
instance _4_ :: CleanShow DB.RangeInfo where
    cleanShow = show

wikiRoot :: String
wikiRoot = "https://grandorder.wiki/index.php?action=raw&title="

newtype MediaWiki = MediaWiki (Map String (Array String))
derive instance _5_ :: Newtype MediaWiki _
toWiki :: String -> Maybe DB.Rank -> MediaWiki
toWiki text mRank = 
    MediaWiki <<< Map.fromFoldable <<< Array.reverse <<< Array.catMaybes <<< 
    map parseLine <<< String.split (Pattern "|") <<< unLink $
    Maybe.fromMaybe text $ mRank >>= \rank -> do 
        start <- String.indexOf (Pattern $ "Rank " <> show rank <> "=") text
        let {after} = String.splitAt start text
        end <- String.indexOf (Pattern "|-|") after 
            <|> String.indexOf (Pattern "/onlyinclude") after
        let {before} = String.splitAt end after
        pure before
  where 
    stripEquals x = Maybe.fromMaybe x $ String.stripPrefix (Pattern "=") x
    parseLine line = do
        subAt              <- String.indexOf (Pattern "=") line
        let {before, after} = String.splitAt subAt line
            afterLines      = String.split (Pattern "<br/>") after
        guard <<< not $ String.null before
        pure <<< Tuple (String.trim $ String.toLower before) <<< 
        Array.filter (not <<< String.null) $ 
        String.trim <<< sanitize <<< stripEquals <$> afterLines
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

wiki :: ∀ a. CleanShow a => (a -> Maybe DB.Rank) -> a -> Aff (Tuple a MediaWiki)
wiki ranker a = do
    res <- AX.get ResponseFormat.string $ wikiRoot <> encoded
    case res.body of
      Left err  -> pure <<< Tuple a $ wikiFail [AX.printResponseFormatError err]
      Right obj -> pure <<< Tuple a $ toWiki obj mRank
  where
    wikiFail err = MediaWiki $ Map.fromFoldable [Tuple "err" err]
    mRank        = ranker a
    name         = cleanShow a
    encoded      = Global.unsafeEncodeURIComponent <<< translate $
                   Maybe.fromMaybe name do
                      rank <- mRank
                      String.stripSuffix (Pattern $ " " <> show rank) name

wikiRange :: MediaWiki -> String -> Array Int -> Array String
wikiRange (MediaWiki mw) k range = 
    join <<< Array.catMaybes $ 
    (flip Map.lookup) mw <<< append k <<< show <$> range
