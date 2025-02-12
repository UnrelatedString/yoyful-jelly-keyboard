module Main (
  main
) where

import Prelude

import Data.Either (either) -- is this really not in Prelude???
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Console (log, error, errorShow)
import Effect.Exception (try, throw, Error)
import Web.HTML (window)
import Web.HTML.Window (Window, document)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.Document (createElement)
import Web.DOM.Element (toNode)

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
  into <- HTMLElement.toNode <$> maybe (throw "There's no body 😭😭😭") pure maybeBody
  kbRoot <- createElement "div" doc
  appendChild (toNode kbRoot) into
  setTextContent "please don't tell me <b>THIS</b> works. PLEASE. just let things be structurally self-validating aaaa" $ toNode kbRoot

celebrateSuccess :: Effect Unit
celebrateSuccess = log "🍾"
