module Web.Nice.Builder
  ( Builder
  , createElement
  , createText
  ) where

import Prelude

import Effect (Effect)
import Web.DOM.Document as DOM.Document
import Web.DOM.Element (Element)

data Builder a = Builder (DOM.Document.Document -> Effect a)

instance Functor Builder where
  map :: forall a b. (a -> b) -> Builder a -> Builder b
  map f (Builder a) = Builder g
    where
    g :: DOM.Document.Document -> Effect b
    g doc = mmm h
      where h :: Effect a
            h = a doc
            mmm :: Effect a -> Effect b
            mmm = map f

instance Apply Builder where
  apply (Builder f) (Builder a) = Builder \doc -> f doc <*> a doc

instance Applicative Builder where
  pure = Builder >>> const

instance Bind Builder where
  bind (Builder a) f = Builder \doc -> (a doc >>= f) doc

instance Monad Builder

createElement :: String -> Builder Element
createElement = Builder >>> DOM.Document.createElement

createText :: String -> Builder Element
createText = Builder >>> DOM.Document.createTextNode
