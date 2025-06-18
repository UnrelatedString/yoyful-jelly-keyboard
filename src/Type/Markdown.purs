-- placeholder for compile-time parsed and/or validated markdown that should definitely
-- just go in its own package LMAO

module Type.Markdown
  ( Markdown
  , md
  ) where

import Prelude
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

type Markdown = String

md :: forall @sym. IsSymbol sym => Markdown
md = reflectSymbol (Proxy @sym)
