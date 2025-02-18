module Main
  ( main
  ) where

import Prelude

import Data.Either (either) -- is this really not in Prelude???
import Data.Maybe (Maybe, maybe, isJust)
import Control.Alternative (guard)
import Control.Bind (when)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, error, errorShow)
import Effect.Exception (try, throw, Error)
import Web.HTML (window)
import Web.HTML.Window (Window, document, toEventTarget)
import Web.HTML.HTMLDocument (HTMLDocument, body, toDocument, activeElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Element (Element)
import Web.CSSOM.ElementCSSInlineStyle (style, fromHTMLElement)
import Web.CSSOM.CSSStyleDeclaration (CSSStyleDeclaration, setProperty)
import Web.Event.Event (Event, target, type_)
import Web.UIEvent.KeyboardEvent (fromEvent, code)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Web.Event.EventTarget (EventListener, eventListener, addEventListenerWithOptions)

import Web.Nice.Node (appendChild)
import Web.Nice.Builder (Builder, runBuilder, createElement, createText)

main :: Effect Unit
main = try window >>= either noWindow windowedMain

noWindow :: Error -> Effect Unit
noWindow e = do
  errorShow e
  error "This is meant to be run as a bookmarklet! Will not work if run without a window"

windowedMain :: Window -> Effect Unit
windowedMain win = do
  doc <- toDocument <$> document win
  maybeBody <- document win >>= body
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
  doc <- document target -- ughh maybe this should just be a separate argument or even Builder ctx but this time it actually does have to be specifically HTMLDocument ughhhh
  listener <- eventListener $ handleKeyPress doc
  addEventListenerWithOptions keydown listener {
      capture: true,
      once: false,
      passive: false
    } $ toEventTarget target
  pure listener

isTabPress :: Event -> Boolean
isTabPress event = isJust do
  kbevent <- fromEvent event
  guard $ type_ event == keydown -- because I do not trust this damn spaghetti API
  guard $ code kbevent == "Tab" -- whyyyyyy isn't this an enummmmmmmm I hate JS so much

commitShortcutEdit :: Effect Unit -> HTMLElement.HTMLElement -> Effect Unit
commitShortcutEdit edit elem = if isContentEditable elem then edit else error "not editable"

handleKeyPress :: HTMLDocument -> Event -> Effect Unit
handleKeyPress doc event = when isTabPress event $ activeElement doc >>= traverse_ \elem -> do
  log "uhhh"
