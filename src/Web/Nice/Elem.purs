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

class IsElem a where
  toElem :: a -> DOM.Element.Element

class IsHTMLElem a where
  toHTMLElem :: a -> HTMLElement.HTMLElement

-- if this ever somehow needs to cover things that AREN'T HTMLElements, just remember to instance chain to the beginning!!
instance IsHTMLElem a => IsElem a where
  toElem = HTMLElement.toElement

instance IsHTMLElem HTMLInputElement.HTMLInputElement where
  toHTMLElem = HTMLInputElement.toHTMLElement
