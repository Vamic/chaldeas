module Site.Common where

import Prelude
import Operators

import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P
import Data.String             as S

import Data.Array
import Data.Int
import Data.Number.Format
import Data.String.CodeUnits
import Halogen.HTML
import Halogen.HTML.Properties
import Web.UIEvent.MouseEvent

import Printing
import Database

lvlRow ∷ ∀ a b. RangeInfo -> HTML a b
lvlRow (RangeInfo isPercent a b) = H.tr_
           $ toCell isPercent <<< (_ + a) <<< (_ * step) <<< toNumber
          <$> (0..8) `snoc` 10
  where
    step = (b - a) / 10.0

noBreakName ∷ String -> String
noBreakName = unBreak <<< S.split (S.Pattern "(")
  where
    unBreak [a, b] = a <> "("
                       <> S.replaceAll (S.Pattern " ") (S.Replacement " ") b
    unBreak xs = S.joinWith "(" xs

fileName ∷ String -> String
fileName = fromCharArray <<< filter legal <<< toCharArray
  where
    legal ':' = false
    legal '/' = false
    legal _   = true

print' ∷ Int -> String
print' = print 0 <<< toNumber

toCell ∷ ∀ a b. Boolean -> Number -> HTML a b
toCell isPercent = _td <<< (isPercent ? (_ <> "%")) <<< toString <<< roundTo 2

urlName ∷ String -> String
urlName = fromCharArray <<< filter legal <<< toCharArray
  where
    legal ' ' = false
    legal ' ' = false
    legal _   = true

----------------
-- ABBREVIATIONS
----------------

_i   ∷ ∀ a b. String -> IProp (id ∷ String | b) a
_i   = P.id_
_c   ∷ ∀ a b. String -> IProp (class ∷ String | b) a
_c   = P.class_ <<< ClassName
_src ∷ ∀ a b. String -> IProp (src ∷ String | b) a
_src = P.src
_style ∷ ∀ a b. String -> IProp (style ∷ String | b) a
_style = P.attr (H.AttrName "style")

_txt ∷ ∀ a b. String -> Array (HTML a b)
_txt = (flip cons) [] <<< H.text

_a ∷ ∀ a b. String -> String -> String -> String -> HTML a b
_a id' class' href' = H.a [_i id', _c class', P.href href'] <<< _txt

_img ∷ ∀ a b. String -> HTML a b
_img src = H.img [_src src]

_click ∷ ∀ a b. (Unit -> b Unit) -> IProp ( onClick ∷ MouseEvent | a ) (b Unit)
_click = E.onClick <<< E.input_

_table ∷ ∀ a b. Array String -> Array (HTML a b) -> HTML a b
_table headings tbody = H.table_
  [ H.colgroup_ $ const (H.col []) <$> headings
  , H.thead_ [ H.tr_ $ H.th_ <<< _txt <$> headings ]
  , H.tbody_ tbody
  ]

_tr ∷ ∀ a b. String -> Array (HTML a b) -> HTML a b
_tr a b = H.tr_ [ _th a, H.td_ b ]

_radio ∷ ∀ a b. String -> Boolean -> Array (HTML a b)
_radio label checked
    = [ H.input [ P.type_ P.InputRadio, P.checked checked ]
      , H.label_ $ _txt label
      ]

_checkbox ∷ ∀ a b. String -> Boolean -> Array (HTML a b)
_checkbox label checked
    = [ H.input [P.type_ P.InputCheckbox, P.checked checked ]
      , H.label_ $ _txt label
      ]

_span ∷ ∀ a b. String -> HTML a b
_span = H.span_ <<< _txt
_p ∷ ∀ a b. String -> HTML a b
_p = H.p_ <<< _txt
_b ∷ ∀ a b. String -> HTML a b
_b = H.b_ <<< _txt
_th ∷ ∀ a b. String -> HTML a b
_th = H.th_ <<< _txt
_td ∷ ∀ a b. String -> HTML a b
_td = H.td_ <<< _txt
_h ∷ ∀ a b. Int -> String -> HTML a b
_h 1 = H.h1_ <<< _txt
_h 2 = H.h2_ <<< _txt
_h 3 = H.h3_ <<< _txt
_h 4 = H.h4_ <<< _txt
_h 5 = H.h5_ <<< _txt
_h _ = H.h6_ <<< _txt
