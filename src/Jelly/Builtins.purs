module Jelly.Builtins
  ( Builtin(..)
  , Builtin'
  , BuiltinType(..)
  , Adicity(..)
  , builtin
  , stringTerminator
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Maybe (Maybe(..))
import Jelly.Codepage (Jel(..), BuiltinForm(..))
import Type.Markdown (Markdown, md)

data Builtin = Builtin BuiltinType Builtin'

type Builtin' =
  { mnemonic :: Maybe Markdown
  , keywords :: Array Markdown
  , originalDescription :: Markdown
  , revisedDescription :: Markdown
  }

data BuiltinType
  = Atom Adicity
  | Quick -- TODO: systematically represent quick argument counts and how they get adicity
  | Syntax

data Adicity = Niladic | Monadic | Dyadic

builtin :: BuiltinForm -> Maybe Builtin
builtin _ = Nothing

-- separate from builtin so I don't have to build that "is this also a terminator??"
-- into the builtin data itself lmao
stringTerminator :: Jel -> Maybe Markdown
stringTerminator OpenGuillemet = Just $
  md @"Terminates a plain string; equivalent to `”`."
stringTerminator CloseGuillemet = Just $
  md @"Terminates a dictionary-compressed string."
stringTerminator OpenSingleQuote = Just $
  md @"Terminates a list of Jelly codepoints."
stringTerminator CloseSingleQuote = Just $
  md @"Terminates a base-250 number."
stringTerminator OpenDoubleQuote = Just $
  md @"Separates elements of a list of strings within one literal."
stringTerminator CloseDoubleQuote = Just $
  md @"Terminates a plain string."
stringTerminator _ = Nothing
