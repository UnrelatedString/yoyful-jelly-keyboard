module Jelly.Builtins
  ( Builtin(..)
  , BuiltinType(..)
  , Adicity(..)
  , LiteralChar(..)
  , builtin
  , literalChar
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Jelly.Codepage (Jel(..), BuiltinForm(..))

newtype Builtin = Builtin
  { mnemonic :: Maybe String
  , keywords :: Array String
  , originalDescription :: String
  , revisedDescription :: String
  }

data BuiltinType
  = Atom Adicity
  | Quick -- TODO: systematically represent quick argument counts and how they get adicity
  | Separator

data Adicity = Niladic | Monadic | Dyadic

data LiteralChar

builtin :: BuiltinForm -> Maybe Builtin
builtin = const Nothing
