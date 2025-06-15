module Jelly.Builtins
  ( Builtin(..)
  , builtin
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Jelly.Codepage (Jel(..), BuiltinForm(..))

newtype Builtin = Builtin
  { mnemonic :: Maybe String
  , keywords :: Array String
  , description :: String
  }

builtin :: BuiltinForm -> Maybe Builtin
builtin = const Nothing
