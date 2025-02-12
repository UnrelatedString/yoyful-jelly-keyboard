module Main (
  main
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
import Web.DOM.Node (Node, appendChild)
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element (Element, toNode)
import Web.CSSOM.ElementCSSInlineStyle (style, fromHTMLElement)

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
  -- TODO: maybe I could just make it create an empty body instead lmao
  into <- HTMLElement.toNode <$> expect "There's no body 😭😭😭" maybeBody
  keyboard <- buildKeyboard doc
  appendChild (toNode keyboard) into
  styleKeyboard keyboard
  celebrateSuccess

celebrateSuccess :: Effect Unit
celebrateSuccess = log "🍾"

expect :: forall a. String -> Maybe a -> Effect a
expect message = throw message `maybe` pure

buildKeyboard :: Document -> Effect Element
buildKeyboard doc = do
  root <- createElement "div" doc
  pure root

styleKeyboard :: Element -> Effect Unit
styleKeyboard keyboard = do
  let literallyHowIsThisFallible = HTMLElement.fromElement keyboard
  htmlElement <- expect
    "Failed to convert Web.DOM.Element to Web.HTML.HTMLElement. SOMEHOW. !?!?!?"
    literallyHowIsThisFallible
  css <- style $ fromHTMLElement htmlElement
  pure unit
