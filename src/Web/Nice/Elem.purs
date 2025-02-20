module Web.Nice.Elem
  ( class IsElem
  , class IsHTMLElem
  , toElem
  , toHTMLElem
  ) where

import Prelude

import Web.DOM.Element as DOM.Element
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.HTMLTextAreaElement as HTMLTextAreaElement

class IsElem a where
  toElem :: a -> DOM.Element.Element

class (IsElem a) <= IsHTMLElem a where
  toHTMLElem :: a -> HTMLElement.HTMLElement

instance IsElem DOM.Element.Element where
  toElem = identity

-- if this ever somehow needs to cover more real things that AREN'T HTMLElements,
-- just remember to instance chain to the beginning!!
else instance IsHTMLElem a => IsElem a where
  toElem = HTMLElement.toElement <<< toHTMLElem

instance IsHTMLElem HTMLElement.HTMLElement where
  toHTMLElem = identity

instance IsHTMLElem HTMLInputElement.HTMLInputElement where
  toHTMLElem = HTMLInputElement.toHTMLElement

instance IsHTMLElem HTMLTextAreaElement.HTMLTextAreaElement where
  toHTMLElem = HTMLTextAreaElement.toHTMLElement
