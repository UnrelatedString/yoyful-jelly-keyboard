module Main
  ( main
  ) where

import Prelude

import Data.Either (either) -- is this really not in Prelude???
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Console (log, error, errorShow)
import Effect.Exception (try, throw, Error)
import Web.HTML (window)
import Web.HTML.Window (Window, document)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Document (Document)
import Web.DOM.Element (Element)
import Web.CSSOM.ElementCSSInlineStyle (style, fromHTMLElement)
import Web.CSSOM.CSSStyleDeclaration (CSSStyleDeclaration, setProperty)

import Web.Nice.Node (appendChild)
import Web.Nice.Builder (Builder, createElement, createText)

main :: Effect Unit
main = try window >>= either noWindow windowedMain

noWindow :: Error -> Effect Unit
noWindow e = do
  errorShow e
  error "This is meant to be run as a bookmarklet!"

windowedMain :: Window -> Effect Unit
windowedMain win = do
  doc <- toDocument <$> document win
  maybeBody <- document win >>= body
  -- TODO: maybe I should actually make it create an empty body instead?
  into <- HTMLElement.toNode <$> expect "There's no body 😭😭😭" maybeBody
  keyboard <- buildKeyboard doc
  appendChild into keyboard
  celebrateSuccess

celebrateSuccess :: Effect Unit
celebrateSuccess = log "🍾"

expect :: forall a. String -> Maybe a -> Effect a
expect message = throw message `maybe` pure

buildKeyboard :: Builder Element
buildKeyboard = do
  keyboard <- createElement "div"
  let append = appendChild keyboard
  text <- createTextNode "keyboard go clicky clacky clicky clacky" doc
  append text
  pure keyboard

inlineDecl :: Element -> Effect CSSStyleDeclaration 
inlineDecl elem = do
  let literallyHowIsThisFallible = HTMLElement.fromElement elem
  htmlElement <- expect
    "Failed to convert Web.DOM.Element to Web.HTML.HTMLElement. SOMEHOW. !?!?!?"
    literallyHowIsThisFallible
  style $ fromHTMLElement htmlElement
