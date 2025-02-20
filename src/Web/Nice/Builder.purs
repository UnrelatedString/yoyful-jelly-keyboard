module Web.Nice.Builder
  ( Builder
  , runBuilder
  , createElement
  , createText
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Web.DOM.Document as DOM.Document
import Web.DOM.Element (Element)
import Web.DOM.Text (Text)
import Control.Monad.Reader (ReaderT(..), runReaderT)

type Builder = ReaderT DOM.Document.Document Effect

createElement :: String -> Builder Element
createElement = ReaderT <<< DOM.Document.createElement

createText :: String -> Builder Text
createText = ReaderT <<< DOM.Document.createTextNode

runBuilder :: forall a. DOM.Document.Document -> Builder a -> Effect a
runBuilder = flip runReaderT
