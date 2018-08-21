module Component.Common where

import Prelude

import Halogen.HTML            as H
import Halogen.HTML.Events     as E
import Halogen.HTML.Properties as P

import Data.Array              (cons)
--import Data.Const              (Const)
import Halogen                 (HTML)
import Halogen.HTML            (ClassName(..))
import Halogen.HTML.Properties (IProp)
import Web.UIEvent.MouseEvent  (MouseEvent)

import Operators

--type ChildQuery = Const Void

_i   ∷ ∀ a b. String → IProp (id ∷ String | b) a 
_i   = P.id_
_c   ∷ ∀ a b. String → IProp (class ∷ String | b) a
_c   = P.class_ ∘ ClassName
_src ∷ ∀ a b. String → IProp (src ∷ String | b) a
_src = P.src
_style ∷ ∀ a b. String → IProp (style ∷ String | b) a
_style = P.attr (H.AttrName "style")

_txt ∷ ∀ a b. String → Array (HTML a b)
_txt = (flip cons) [] ∘ H.text

_a ∷ ∀ a b. String → String → String → String → HTML a b
_a id' class' href' = H.a [_i id', _c class', P.href href'] ∘ _txt

_b ∷ ∀ a b. String → HTML a b
_b = H.b_ ∘ _txt

_span ∷ ∀ a b. String → HTML a b
_span = H.span_ ∘ _txt

_extra ∷ ∀ a b. String → HTML a b
_extra = H.span [_c "extra"] ∘ _txt

_minor ∷ ∀ a b. String → HTML a b
_minor = H.span [_c "minor"] ∘ _txt

_click ∷ ∀ a b. (Unit → b Unit) → IProp ( onClick ∷ MouseEvent | a ) (b Unit)
_click = E.onClick ∘ E.input_
