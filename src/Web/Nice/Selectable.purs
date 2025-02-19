module Web.Nice.Selectable
  ( Selectable
  , toSelectable
  , selectionStart
  , selectionEnd
  , selectingFrom
  , setSelectingFrom
  ) where

import Prelude

-- why can't Spago find the existing textcursor package 😭😭😭😭😭

import Data.Maybe (Maybe(..))
import Control.Alternative (guard)
import Effect (Effect)

import Web.HTML.HTMLTextAreaElement as TextArea
import Web.HTML.HTMLInputElement as Input
import Web.HTML.HTMLElement (HTMLElement)
import Web.Nice.Node (textContent, setTextContent)

data Selectable = TA TextArea.HTMLTextAreaElement
                | I Input.HTMLInputElement

toSelectable :: HTMLElement -> Effect (Maybe Selectable)
toSelectable elem
  | Just ta <- TextArea.fromHTMLElement elem = pure $ pure $ TA ta
  | Just i <- Input.fromHTMLElement elem = do
    t <- Input.type_ i
    pure $ guard (
      t == "text" ||
      t == "search" ||
      t == "URL" ||
      t == "tel" ||
      t == "password" -- not that I think anyone will ever want to use this outside text LMAO
    ) $> I i
  | otherwise = pure Nothing

selectionStart :: Selectable -> Effect Int
selectionStart (TA e) = TextArea.selectionStart e
selectionStart (I e) = Input.selectionStart e

selectionEnd :: Selectable -> Effect Int
selectionEnd (TA e) = TextArea.selectionEnd e
selectionEnd (I e) = Input.selectionEnd e

selectingFrom :: Selectable -> Effect String
selectingFrom (TA e) = textContent e
selectingFrom (I e) = Input.value e

setSelectingFrom :: String -> Selectable -> Effect Unit
setSelectingFrom to (TA e) = setTextContent to e
setSelectingFrom to (I e) = Input.setValue to e
