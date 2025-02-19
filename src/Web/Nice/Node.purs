module Web.Nice.Node
  ( class IsNode
  , toNode
  , appendChild
  , textContent
  ) where 

import Prelude

import Effect (Effect)
import Web.DOM.Node (Node)
import Web.DOM.Node as DOM.Node
import Web.DOM.Element as DOM.Element
import Web.DOM.Text as DOM.Text
import Web.HTML.HTMLElement as HTMLElement

class IsNode a where
  toNode :: a -> Node

-- Note: argument order is reversed because the existing one makes zero sense to curry!
appendChild :: forall a b. IsNode a => IsNode b => a -> b -> Effect Unit
appendChild parent child = DOM.Node.appendChild (toNode child) $ toNode parent

textContent :: forall a. IsNode a => a -> Effect String
textContent = DOM.Node.textContent <<< toNode

instance IsNode Node where
  toNode = identity

instance IsNode DOM.Element.Element where
  toNode = DOM.Element.toNode

instance IsNode DOM.Text.Text where
  toNode = DOM.Text.toNode

instance IsNode HTMLElement.HTMLElement where
  toNode = HTMLElement.toNode
