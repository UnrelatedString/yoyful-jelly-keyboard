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
builtin (Single Copyright) = Just $ Builtin Quick
  { mnemonic: Just "copy"
  , keywords: []
  , originalDescription: md @
    "Copy link result to register (`®` atom to retrieve)."
  , revisedDescription: md @
    "Copy the result to the register, returning it unchanged."
  }
builtin (Single Eszett) = Just $ Builtin Quick
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "This link, with the same arity."
  , revisedDescription: md @
    "Recursively invoke the current furlong, with adicity `-1`."
  }
builtin (Single Cent) = Just $ Builtin Quick
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Last link as a nilad."
  , revisedDescription: md @
    "Invoke the furlong above this one, as a nilad."
  }
builtin (Single BigCCedilla) = Just $ Builtin Quick
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Last link as a monad."
  , revisedDescription: md @
    "Invoke the furlong above this one, as a monad."
  }
builtin (Single LittleCCedilla) = Just $ Builtin Quick
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Last link as a dyad."
  , revisedDescription: md @
    "Invoke the furlong above this one, as a dyad."
  }
builtin (Single BigEnye) = Just $ Builtin Quick
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Next link as a monad."
  , revisedDescription: md @
    "Invoke the furlong below this one, as a monad."
  }
builtin (Single LittleEnye) = Just $ Builtin Quick
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Next link as a dyad."
  , revisedDescription: md @
    "Invoke the furlong below this one, as a dyad."
  }
builtin (Single Pound) = Just $ Builtin Quick
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Link at index n as a nilad."
  , revisedDescription: md @
    "Invoke the (non-main) furlong at the given index from the top, as a nilad."
  }
builtin (Single BigLOverdot) = Just $ Builtin Quick
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Link at index n as a monad."
  , revisedDescription: md @
    "Invoke the (non-main) furlong at the given index from the top, as a monad."
  }
builtin (Single LittleLOverdot) = Just $ Builtin Quick
  { mnemonic: Nothing
  , keywords: []
  , originalDescription: md @
    "Link at index n as a dyad."
  , revisedDescription: md @
    "Invoke the (non-main) furlong at the given index from the top, as a dyad."
  }
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
