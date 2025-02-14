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

data Builder a = Builder (DOM.Document.Document -> Effect a)

instance Functor Builder where
  map f (Builder a) = Builder $ map f <<< a

instance Apply Builder where
  apply (Builder f) (Builder a) = Builder \doc -> f doc <*> a doc

instance Applicative Builder where
  pure = Builder <<< const <<< pure

instance Bind Builder where
  bind (Builder a) f = Builder \doc -> do
    r1 <- a doc
    let Builder r2 = f r1
    r2 doc

instance Monad Builder

instance MonadEffect Builder where
  liftEffect = Builder <<< const

createElement :: String -> Builder Element
createElement = Builder <<< DOM.Document.createElement

createText :: String -> Builder Text
createText = Builder <<< DOM.Document.createTextNode

runBuilder :: forall a. DOM.Document.Document -> Builder a -> Effect a
runBuilder doc (Builder a) = a doc
