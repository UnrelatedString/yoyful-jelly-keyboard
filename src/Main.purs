module Main (
  main
) where

import Prelude

import Data.Either (either) -- is this really not in Prelude???
import Effect (Effect)
import Effect.Console (log, error, errorShow)
import Effect.Exception (try, Error)
import Web.HTML (window)
import Web.HTML.Window (Window, document)
import Web.DOM.Document (createTextNode)

main :: Effect Unit
main = try window >>= either noWindow windowedMain

noWindow :: Error -> Effect Unit
noWindow e = do
  errorShow e
  error "This is meant to be run as a bookmarklet!"

windowedMain :: Window -> Effect Unit
windowedMain win = pure unit
