module Web.Nice.Builder
  ( Builder
  , ContextT
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

data ContextT :: forall k. Type -> (k -> Type) -> k -> Type
data ContextT c m a = ContextT (c -> m a)
type Builder = ContextT DOM.Document.Document Effect

instance Functor m => Functor (ContextT c m) where
  map f (ContextT a) = ContextT $ map f <<< a

instance Apply m => Apply (ContextT c m) where
  apply (ContextT f) (ContextT a) = ContextT \doc -> f doc <*> a doc

instance Applicative m => Applicative (ContextT c m) where
  pure = ContextT <<< const <<< pure

instance Monad m => Bind (ContextT c m) where
  bind (ContextT a) f = ContextT \doc -> do
    r1 <- a doc
    let ContextT r2 = f r1
    r2 doc

instance Monad m => Monad (ContextT c m)

instance MonadEffect (ContextT c Effect) where
  liftEffect = ContextT <<< const

createElement :: String -> Builder Element
createElement = ContextT <<< DOM.Document.createElement

createText :: String -> Builder Text
createText = ContextT <<< DOM.Document.createTextNode

runBuilder :: forall a. DOM.Document.Document -> Builder a -> Effect a
runBuilder doc (ContextT a) = a doc
