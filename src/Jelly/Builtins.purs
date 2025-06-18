module Jelly.Builtins
  ( Builtin(..)
  , Builtin'
  , BuiltinType(..)
  , Adicity(..)
  , builtin
  , stringTerminator
  ) where

import Data.Maybe (Maybe(..))
import Jelly.Codepage (Jel(..), BuiltinForm(..))
import Type.Markdown (Markdown, md)

data Builtin = Builtin BuiltinType Builtin'

type Builtin' =
  { mnemonic :: Maybe String
  , keywords :: Array String
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
stringTerminator :: Jel -> Maybe Builtin'
stringTerminator OpenGuillemet = Just
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "[No original description -- not an intentional builtin]"
  , revisedDescription: md @
    "Terminates a plain string; equivalent to `”`."
  }
stringTerminator CloseGuillemet = Just
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Terminates a dictionary-compressed string."
  , revisedDescription: md @
    "Terminates a dictionary-compressed string."
  }
stringTerminator OpenSingleQuote = Just
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Terminates a code-page index list. Jelly's version of `ord()`."
  , revisedDescription: md @
    "Terminates a string to be interpreted as a numeric list of Jelly codepoints."
  }
stringTerminator CloseSingleQuote = Just
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Terminates a base-250 number."
  , revisedDescription: md @
    "Terminates a base-250 number."
  }
stringTerminator OpenDoubleQuote = Just
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Begins a string literal, and separates a list of strings inside a string literal."
  , revisedDescription: md @
    "Separates elements of a list of strings within one literal."
  }
stringTerminator CloseDoubleQuote = Just
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Terminates a regular string or a list of strings. Without `“`, a character literal."
  , revisedDescription: md @
    "Terminates a plain string."
  }
stringTerminator _ = Nothing
