module Web.Nice.Selectable
  ( Selectable
  , selectionStart
  , selectionEnd
  ) where

-- why can't Spago find the existing textcursor package 😭😭😭😭😭

import Data.Maybe (Maybe)
import Control.Bind (when)

import Web.HTML.HTMLTextAreaElement as TextArea
import Web.HTML.HTMLInputElement as Input
import Web.HTML.HTMLElement (HTMLElement)

data Selectable = TA TextArea.HTMLTextAreaElement
                | I Input.HTMLInputElement

toSelectable :: HTMLElement -> Effect (Maybe Selectable)
toSelectable elem
  | Just ta <- TextArea.fromHTMLElement elem = pure $ pure $ TA ta
  | Just i <- Input.fromHTMLElement elem = do
    t <- Input.type_ i
    pure $ when (
      t == "text" ||
      t == "search" ||
      t == "URL" ||
      t == "tel" ||
      t == "password" -- not that I think anyone will ever want to use this outside text LMAO
    ) $ pure $ I i
  | otherwise = Nothing

selectionStart :: Selectable -> Effect Int
selectionStart (TA e) = TextArea.selectionStart e
selectionStart (I e) = Input.selectionStart e

selectionEnd :: Selectable -> Effect Int
selectionEnd (TA e) = TextArea.selectionEnd e
selectionEnd (I e) = Input.selectionEnd e
