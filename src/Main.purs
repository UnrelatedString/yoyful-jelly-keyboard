module Main
  ( main
  ) where

import Prelude

import Data.Either (either) -- is this really not in Prelude???
import Data.Maybe (Maybe, maybe)
import Control.Alternative (guard)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, error, errorShow)
import Effect.Exception (try, throw, Error)
import Web.HTML (window)
import Web.HTML.Window (Window, document, toEventTarget)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Element (Element)
import Web.CSSOM.ElementCSSInlineStyle (style, fromHTMLElement)
import Web.CSSOM.CSSStyleDeclaration (CSSStyleDeclaration, setProperty)
import Web.Event.Event (Event, target, type_)
import Web.UIEvent.KeyboardEvent (fromEvent)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Web.Event.EventTarget (EventListener, eventListener, addEventListenerWithOptions)

import Web.Nice.Node (appendChild)
import Web.Nice.Builder (Builder, runBuilder, createElement, createText)

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
  keyboard <- runBuilder doc buildKeyboard
  appendChild into keyboard
  -- TODO: actually store the handler to be able to close it when the keyboard is closed
  void $ registerKeyHandler win
  celebrateSuccess

celebrateSuccess :: Effect Unit
celebrateSuccess = log "🍾"

expect :: forall a. String -> Maybe a -> Effect a
expect message = throw message `maybe` pure

buildKeyboard :: Builder Element
buildKeyboard = do
  keyboard <- createElement "div"
  let append = liftEffect <<< appendChild keyboard
  text <- createText "keyboard go clicky clacky clicky clacky"
  append text
  pure keyboard

inlineDecl :: Element -> Effect CSSStyleDeclaration 
inlineDecl elem = do
  let literallyHowIsThisFallible = HTMLElement.fromElement elem
  htmlElement <- expect
    "Failed to convert Web.DOM.Element to Web.HTML.HTMLElement. SOMEHOW. !?!?!?"
    literallyHowIsThisFallible
  style $ fromHTMLElement htmlElement

-- Returns so it can be removed later if the keyboard is closed. 
registerKeyHandler :: Window -> Effect EventListener
registerKeyHandler target = do
  listener <- eventListener handleKeyPress
  addEventListenerWithOptions keydown listener {
      capture: true,
      once: false,
      passive: false
    } $ toEventTarget target
  pure listener

handleKeyPress :: Event -> Effect Unit
handleKeyPress event = sequence_ do 
  kbevent <- fromEvent event
  guard $ type_ event == keydown -- because I do not trust this damn spaghetti API
  pure do
    log "foo"
