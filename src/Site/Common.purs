-- | Utility rendering functions.
module Site.Common where

import StandardLibrary
import Halogen.HTML.Events     as E
import Halogen.HTML            as H
import Halogen.HTML.Properties as P

import Data.Date (Date, Month)
import Halogen.HTML (HTML, ClassName(..))

import Data.DateTime as DateTime
import Data.Int as Int
import Data.Number.Format as Format
import Data.Maybe as Maybe
import Data.String as String
import Data.Time.Duration (Hours(..))
import Partial.Unsafe as Unsafe
import Effect.Now as Now
import Halogen.HTML.Properties (IProp)
import Web.UIEvent.MouseEvent (MouseEvent)

import Printing
import Database
import Site.Preferences

-- | Builds a `Date` out of a year, month, and day.
ymd :: Int -> Month -> Int -> Date
ymd y m d = Unsafe.unsafePartial Maybe.fromJust do
    y' <- toEnum y
    d' <- toEnum d
    DateTime.exactDate y' m d'

getDate :: Effect Date
getDate = DateTime.date <<< 
          maybeDo (DateTime.adjust <<< Hours $ -4.0) <$> Now.nowDateTime

lvlRow :: ∀ a b. RangeInfo -> HTML a b
lvlRow (RangeInfo isPercent x y) = 
    H.tr_ $ toCell isPercent <<< (_ + x) <<< (_ * step) <<< Int.toNumber 
    <$> (0..8) `snoc` 10
  where
    step = (y - x) / 10.0

noBreakName :: Boolean -> String -> String
noBreakName doPrettify = doPrettify ? prettify <<< 
    String.replaceAll (Pattern "Anne Bonny") (Replacement "Anne Bonny") <<<
    String.replaceAll (Pattern "& Mary Read") (Replacement "& Mary Read") <<<
    unBreak <<< String.split (Pattern "(")
  where
    unBreak [x, y] = x <> "(" 
                       <> String.replaceAll (Pattern " ") (Replacement " ") y
    unBreak xs     = String.joinWith "(" xs

mode :: Preferences -> String
mode prefs
  | prefer prefs NightMode = "dark"
  | otherwise              = "light"

places' :: Int -> String
places' = places 0 <<< Int.toNumber

toCell :: ∀ a b. Boolean -> Number -> HTML a b
toCell isPercent = _td <<< (isPercent ? flip append "%") <<< 
                   Format.toString <<< roundTo 2

----------------
-- ABBREVIATIONS
----------------

_i   :: ∀ a b. String -> IProp (id :: String | b) a
_i   = P.id_
_c   :: ∀ a b. String -> IProp (class :: String | b) a
_c   = P.class_ <<< ClassName
_src :: ∀ a b. String -> IProp (src :: String | b) a
_src = P.src
_style :: ∀ a b. String -> IProp (style :: String | b) a
_style = P.attr (H.AttrName "style")

_txt :: ∀ a b. String -> Array (HTML a b)
_txt text = [H.text text]

_click :: ∀ a b. (Unit -> b Unit) -> IProp ( onClick :: MouseEvent | a ) (b Unit)
_click = E.onClick <<< E.input_

_a :: ∀ a b. String -> (Unit -> b Unit) -> HTML a (b Unit)
_a text click = H.a [_click click] $ _txt text

_img :: ∀ a b. String -> HTML a b
_img src = H.img [_src src]

_button :: ∀ a b. String -> Boolean -> (Unit -> b Unit) -> HTML a (b Unit)
_button label enable click
  | enable    = H.button [ _click click, P.enabled enable ] $ _txt label
  | otherwise = H.button [ P.enabled enable ] $ _txt label

_table :: ∀ a b. Array String -> Array (HTML a b) -> HTML a b
_table headings tbody = H.table_
  [ H.colgroup_ $ const (H.col []) <$> headings
  , H.thead_ [ H.tr_ $ H.th_ <<< _txt <$> headings ]
  , H.tbody_ tbody
  ]

_tr :: ∀ a b. String -> Array (HTML a b) -> HTML a b
_tr x y = H.tr_ [ _th x, H.td_ y ]

_radio :: ∀ a b. String -> Boolean -> Array (HTML a b)
_radio label checked = 
    [ H.input [ P.type_ P.InputRadio, P.checked checked ]
    , H.label_ $ _txt label
    ]

_checkbox :: ∀ a b. Maybe (HTML a b) ->  String -> Boolean -> Array (HTML a b)
_checkbox icon label checked =
    [ H.input [P.type_ P.InputCheckbox, P.checked checked ]
    , H.label_ case icon of
                   Nothing -> [H.text label]
                   Just ic -> [ic, H.text label]
    ]

_int :: ∀ a b. (Unit -> b Unit) -> Int -> Int -> Int 
     -> (Int -> Unit -> b Unit) -> Array (HTML a (b Unit))
_int ifFail minVal maxVal actualVal changed =
    [ H.input 
      [ P.type_ P.InputNumber
      , P.value $ show actualVal
      , P.min   $ Int.toNumber minVal
      , P.max   $ Int.toNumber maxVal
      , P.step  $ P.Step 1.0
      , E.onValueInput <<< E.input $ \val -> 
            case Int.fromString val of
                Just intVal 
                  | intVal >= minVal && intVal <= maxVal -> changed intVal
                _ -> ifFail
      ]
    , H.text $ "/" <> show maxVal
    ]

_span :: ∀ a b. String -> HTML a b
_span = H.span_ <<< _txt
_p :: ∀ a b. String -> HTML a b
_p = H.p_ <<< _txt
_strong :: ∀ a b. String -> HTML a b
_strong = H.strong_ <<< _txt
_th :: ∀ a b. String -> HTML a b
_th = H.th_ <<< _txt
_td :: ∀ a b. String -> HTML a b
_td = H.td_ <<< _txt
_h :: ∀ a b. Int -> String -> HTML a b
_h 1 = H.h1_ <<< _txt
_h 2 = H.h2_ <<< _txt
_h 3 = H.h3_ <<< _txt
_h 4 = H.h4_ <<< _txt
_h 5 = H.h5_ <<< _txt
_h _ = H.h6_ <<< _txt
