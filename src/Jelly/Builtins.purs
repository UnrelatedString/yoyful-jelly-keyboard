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

data Builtin = Builtin BuiltinType Builtin'

type Builtin' =
  { mnemonic :: Maybe String
  , keywords :: Array String
  , originalDescription :: String
  , revisedDescription :: String
  }

data BuiltinType
  = Atom Adicity
  | Quick -- TODO: systematically represent quick argument counts and how they get adicity
  | Syntax

data Adicity = Niladic | Monadic | Dyadic

builtin :: BuiltinForm -> Maybe Builtin
builtin form
  =   (nilads1 form <#> Builtin (Atom Niladic))
  <|> (monads1 form <#> Builtin (Atom Monadic))
  <|> (dyads1 form <#> Builtin (Atom Dyadic))
  <|> (nilads2 form <#> Builtin (Atom Niladic))
  <|> (monads2 form <#> Builtin (Atom Monadic))
  <|> (dyads2 form <#> Builtin (Atom Dyadic))
  <|> (quicks form <#> Builtin Quick)
  <|> (syntaxConstructs form <#> Builtin Syntax)

nilads1 ::

-- separate from builtin so I don't have to build that "is this also a terminator??"
-- into the builtin data itself lmao
stringTerminator :: Jel -> Maybe String
-- wait I'm going to want to be able to handle text formatting aren't I ughhhh
stringTerminator OpenGuillemet = Just
  "Terminates a plain string; equivalent to `”`."
stringTerminator CloseGuillemet = Just
  "Terminates a dictionary-compressed string."
stringTerminator OpenSingleQuote = Just
  "Terminates a list of Jelly codepoints."
stringTerminator CloseSingleQuote = Just
  "Terminates a base-250 number."
stringTerminator OpenDoubleQuote = Just
  "Separates elements of a list of strings within one literal."
stringTerminator CloseDoubleQuote = Just
  "Terminates a plain string."
stringTerminator _ = Nothing
