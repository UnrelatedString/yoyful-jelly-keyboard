-- placeholder for compile-time parsed and/or validated markdown that should definitely
-- just go in its own package LMAO

module Type.Markdown
  ( Markdown
  , md
  ) where

import Prelude
import Data.Reflectable (reflectType)
import Type.Proxy (Proxy(..))

type Markdown = String

md :: forall (@sym :: Symbol). Markdown
md = reflectType (Proxy @sym)
